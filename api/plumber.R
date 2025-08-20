library(plumber)
library(logger)
source("R/lca.R")
source("R/config.R")
source("R/security.R")
source("R/monitoring.R")

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

#* @filter auth
function(req, res){
  logger::log_info(sprintf("Incoming request: %s %s", req$REQUEST_METHOD, req$PATH_INFO))
  key <- req$HTTP_X_API_KEY
  if (is.null(key) || !verify_api_key(key, required_role = "user")) {
    res$status <- 401
    if (!is.null(metrics)) record_metric(metrics$errors_total, labels = list(endpoint = req$PATH_INFO))
    return(list(error = "unauthorized"))
  }
  forward()
}

#* Liveness probe
#* @get /health/live
function(){ health_check() }

#* Readiness probe
#* @get /health/ready
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

#* Run latent class analysis
#* @param file:file CSV data
#* @post /lca
#* @serializer rds
function(file, res){
  if (is.null(file) || is.null(file$datapath)) {
    logger::log_error("No file provided to /lca endpoint")
    res$status <- 400
    return(list(error = "No file provided"))
  }
  config <- read_config("config/default.yaml")
  out <- tempfile(fileext = ".RData")
  tryCatch({
    run_lca(file$datapath, out, config, verbose = FALSE, show_progress = FALSE)
    load(out)
    lc7
  }, error = function(e) {
    logger::log_error(sprintf("LCA endpoint failure: %s", e$message))
    if (!is.null(metrics)) record_metric(metrics$errors_total, labels = list(endpoint = "/lca"))
    res$status <- 500
    list(error = "LCA failed", message = e$message)
  })
}

#* Download sample data
#* @get /sample
#* @serializer csv
function(){
  read.csv("data/puf_early.csv")
}
