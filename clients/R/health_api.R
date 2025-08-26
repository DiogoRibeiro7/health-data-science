# Simple R wrapper for the Health Data Science API

health_api <- function(base_url, api_key = NULL, token = NULL) {
  headers <- list()
  if (!is.null(api_key)) headers$`X-API-Key` <- api_key
  if (!is.null(token)) headers$Authorization <- paste("Bearer", token)
  list(
    health = function(){
      httr::GET(paste0(base_url, "/v1/health/live"), httr::add_headers(.headers = headers))
    }
  )
}
