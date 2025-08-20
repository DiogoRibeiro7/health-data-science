library(shiny)
source("R/lca.R")
source("R/config.R")

ui <- fluidPage(
  uiOutput("page")
)

server <- function(input, output, session){
  credentials <- reactiveValues(valid = FALSE)
  output$page <- renderUI({
    if (!credentials$valid) {
      tagList(
        textInput("user", "Username"),
        passwordInput("password", "Password"),
        actionButton("login", "Log in"),
        textOutput("error")
      )
    } else {
      fluidPage(
        titlePanel("Health Data Analysis"),
        sidebarLayout(
          sidebarPanel(
            fileInput("file", "Upload CSV", accept = ".csv"),
            actionButton("run", "Run LCA"),
            downloadButton("download", "Download Results")
          ),
          mainPanel(verbatimTextOutput("summary"))
        )
      )
    }
  })
  observeEvent(input$login, {
    if (input$user == Sys.getenv("APP_USER", "user") &&
        input$password == Sys.getenv("APP_PASSWORD", "password")) {
      credentials$valid <- TRUE
    } else {
      output$error <- renderText("Invalid credentials")
    }
  })
  results <- eventReactive(input$run, {
    req(credentials$valid)
    req(input$file)
    tmp <- tempfile(fileext = ".csv")
    file.copy(input$file$datapath, tmp, overwrite = TRUE)
    out <- tempfile(fileext = ".RData")
    config <- read_config("config/default.yaml")
    run_lca(tmp, out, config, verbose = FALSE, show_progress = FALSE)
    load(out)
    lc7
  })
  output$summary <- renderPrint({
    req(results())
    summary(results())
  })
  output$download <- downloadHandler(
    filename = function() "lca_results.RData",
    content = function(file){
      req(results())
      save(results(), file = file)
    }
  )
}

shinyApp(ui, server)
