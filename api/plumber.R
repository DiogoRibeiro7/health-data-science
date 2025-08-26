#* @apiTitle Health Data Science API
#* @apiDescription REST API for healthcare analytics
#* @apiVersion 1.0.0

library(plumber)
library(logger)

source("R/lca.R")
source("R/config.R")
source("R/security.R")
source("R/monitoring.R")
source("R/api_utils.R")
source("R/etl.R")

metrics <- tryCatch(init_monitoring(), error = function(e) NULL)

#! Metrics collection
#* @filter metrics
function(req, res){
  start <- Sys.time()
  on.exit({
    if (!is.null(metrics)) {
      dur <- as.numeric(difftime(Sys.time(), start, units = "secs"))
      record_metric(metrics$request_latency, dur, labels = list(endpoint = req$PATH_INFO))
    }
  })
  if (!is.null(metrics)) {
    record_metric(metrics$http_requests, labels = list(method = req$REQUEST_METHOD, endpoint = req$PATH_INFO))
    monitor_resources(metrics)
  }
  forward()
}

#! Distributed tracing
#* @filter trace
function(req, res){
  span <- start_trace(req$PATH_INFO)
  on.exit(end_trace(span))
  forward()
}

#! Authentication and rate limiting
#* @filter auth
function(req, res){
  logger::log_info(sprintf("Incoming request: %s %s", req$REQUEST_METHOD, req$PATH_INFO))
  token <- req$HTTP_AUTHORIZATION
  api_key <- req$HTTP_X_API_KEY
  authorised <- FALSE
  id <- "anonymous"
  if (!is.null(token) && grepl("Bearer ", token)) {
    tok <- sub("Bearer ", "", token)
    authorised <- validate_oidc_token(tok, required_role = "user")
    id <- substr(tok, 1, 8)
  } else if (!is.null(api_key)) {
    authorised <- verify_api_key(api_key, required_role = "user")
    id <- api_key
  }
  if (!authorised) {
    res$status <- 401
    if (!is.null(metrics)) record_metric(metrics$errors_total, labels = list(endpoint = req$PATH_INFO))
    return(list(error = "unauthorized"))
  }
  if (!check_rate_limit(id, limit = 60, window = 60)) {
    res$status <- 429
    return(list(error = "rate limit exceeded"))
  }
  forward()
}

#! Error handling
#* @filter errors
function(req, res){
  tryCatch({
    forward()
  }, error = function(e){
    logger::log_error(sprintf("Unhandled error on %s: %s", req$PATH_INFO, e$message))
    res$status <- 500
    list(error = "internal_error", message = e$message)
  })
}

#* Liveness probe
#* @get /v1/health/live
function(){ health_check() }

#* Readiness probe
#* @get /v1/health/ready
function(){ health_check() }

#* Metrics endpoint
#* @get /metrics
function(res){
  if (is.null(metrics)) {
    res$status <- 500
    return("metrics unavailable")
  }
  prometheus::registry_render_metrics(metrics$registry)
}

#* Run latent class analysis asynchronously
#* @param file:file CSV data
#* @post /v1/lca
function(file, res){
  if (is.null(file) || is.null(file$datapath)) {
    res$status <- 400
    return(list(error = "No file provided"))
  }
  cfg <- read_config("config/default.yaml")
  job <- submit_job(function(path){
    out <- tempfile(fileext = ".RData")
    run_lca(path, out, cfg, verbose = FALSE, show_progress = FALSE)
    load(out)
    lc7
  }, file$datapath)
  list(job_id = job)
}

#* Check job status
#* @get /v1/jobs/<id>
function(id, res){
  status <- get_job_status(id)
  if (status$status == "unknown") res$status <- 404
  status
}

#* Download sample data with pagination
#* @param page:int Page number
#* @param per_page:int Records per page
#* @get /v1/sample
function(page = 1, per_page = 50){
  data <- read.csv("data/puf_early.csv")
  paginate(data, as.integer(page), as.integer(per_page))
}

#* Register webhook
#* @param url:string Endpoint URL
#* @post /v1/webhooks
function(url, res){
  if (is.null(url)) {res$status <- 400; return(list(error = "url required"))}
  register_webhook(url)
  list(status = "registered")
}

#* Trigger ETL pipeline
#* @post /v1/etl
function(){
  submit_job(run_etl, config = "config/etl.yaml")
  list(status = "started")
}

#* Get patient record in FHIR format
#* @get /v1/fhir/patient/<id>
function(id, res){
  data <- read.csv("data/puf_early.csv")
  rec <- data[data$id == as.integer(id),]
  if (nrow(rec) == 0) {res$status <- 404; return(list(error = "not found"))}
  to_fhir_patient(rec[1,])
}

#* OAuth token refresh (stub)
#* @post /v1/token/refresh
function(){ list(token = uuid::UUIDgenerate()) }

# WebSocket for real-time updates
#* @plumber
function(pr){
  pr$handle("ws", "/v1/updates", function(ws){
    ws$onMessage(function(binary, message){
      ws$send(message)
    })
  })
  pr
}
