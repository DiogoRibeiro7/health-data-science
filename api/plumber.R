library(plumber)
source("R/lca.R")
source("R/config.R")

#* @filter auth
function(req, res){
  plumber::basicAuth(req, res,
    user = Sys.getenv("API_USER", "user"),
    password = Sys.getenv("API_PASSWORD", "password"))
}

#* Health check
#* @get /ping
function(){ list(status = "ok") }

#* Run latent class analysis
#* @param file:file CSV data
#* @post /lca
#* @serializer rds
function(file){
  config <- read_config("config/default.yaml")
  out <- tempfile(fileext = ".RData")
  run_lca(file$datapath, out, config, verbose = FALSE, show_progress = FALSE)
  load(out)
  lc7
}

#* Download sample data
#* @get /sample
#* @serializer csv
function(){
  read.csv("data/puf_early.csv")
}
