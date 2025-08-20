#' Launch the Shiny application
#'
#' Starts the Shiny app located in `app/` after verifying that required
#' packages are installed. Authentication credentials are read from the
#' `APP_USER` and `APP_PASSWORD` environment variables.
#'
#' @param port Integer port to bind the application to.
#' @return Invisibly returns the Shiny app object.
#' @examples
#' \dontrun{
#' launch_app()
#' }
#' @export
launch_app <- function(port = 3838) {
  app <- shiny::shinyAppDir("app")
  shiny::runApp(app, port = port, launch.browser = FALSE)
  invisible(app)
}

#' Launch the Plumber API
#'
#' Initializes the REST API defined in `api/plumber.R` with basic
#' authentication. Credentials are sourced from the `API_USER` and
#' `API_PASSWORD` environment variables.
#'
#' @param port Integer port to bind the API to.
#' @return Invisibly returns the Plumber router object.
#' @examples
#' \dontrun{
#' launch_api()
#' }
#' @export
launch_api <- function(port = 8000) {
  pr <- plumber::plumb("api/plumber.R")
  pr$run(port = port)
  invisible(pr)
}
