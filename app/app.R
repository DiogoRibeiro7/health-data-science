library(shiny)
library(bs4Dash)
library(plotly)
library(DT)
library(shinyjs)
library(shinyWidgets)
source("R/lca.R")
source("R/config.R")
source("R/audit.R")

users <- yaml::read_yaml("config/users.yaml")$users

# authenticate user and return role
validate_user <- function(user, password) {
  match <- Filter(function(x) x$username == user && x$password == password, users)
  if (length(match) == 1) match[[1]] else NULL
}

# load and save user preferences -------------------------------------------------
load_prefs <- function(user) {
  path <- file.path("outputs", paste0(user, "_prefs.rds"))
  if (file.exists(path)) readRDS(path) else list(theme = "light")
}

save_prefs <- function(user, prefs) {
  dir.create("outputs", showWarnings = FALSE)
  saveRDS(prefs, file.path("outputs", paste0(user, "_prefs.rds")))
}

# shared notes path for collaborative editing
notes_path <- file.path("outputs", "shared_notes.txt")

ui <- function(request) {
  tagList(useShinyjs(), uiOutput("page"))
}

server <- function(input, output, session) {
  creds <- reactiveValues(valid = FALSE, user = NULL, role = NULL,
                          prefs = list(theme = "light"))

  output$page <- renderUI({
    if (!creds$valid) {
      fluidPage(title = "Login",
                tags$h2("Health Analytics Dashboard"),
                textInput("user", "Username"),
                passwordInput("password", "Password"),
                actionButton("login", "Log in"),
                textOutput("error"))
    } else {
      bs4DashPage(
        title = "Health Analytics Dashboard",
        dark = identical(creds$prefs$theme, "dark"),
        navbar = bs4DashNavbar(skin = "light", fixed = TRUE,
                               rightUi = tagList(
                                 materialSwitch("theme", "Dark mode", status = "primary", value = identical(creds$prefs$theme, "dark")),
                                 actionLink("logout", NULL, icon = icon("sign-out-alt"))
                               )),
        sidebar = bs4DashSidebar(skin = "light", status = "primary",
                                 sidebarMenu(
                                   menuItem("Analyze", tabName = "analyze", icon = icon("flask")),
                                   menuItem("Real-time", tabName = "realtime", icon = icon("chart-line")),
                                   conditionalPanel("output.isAdmin", menuItem("Admin", tabName = "admin", icon = icon("user-shield")))
                                 )),
        body = bs4DashBody(
          tabItems(
            tabItem(tabName = "analyze",
                    fluidRow(
                      bs4Card(width = 4, title = "Data & Parameters", status = "primary", solidHeader = TRUE,
                              fileInput("file", "Upload CSV", accept = ".csv"),
                              numericInput("nclass", "Classes", value = 3, min = 1, max = 10),
                              actionButton("run", "Run LCA"),
                              downloadButton("download", "Results"),
                              downloadButton("report", "Report")),
                      bs4Card(width = 8, title = "Outputs", status = "primary",
                              tabsetPanel(
                                tabPanel("Summary", verbatimTextOutput("summary")),
                                tabPanel("Profiles", plotlyOutput("profiles")),
                                tabPanel("Data", DTOutput("preview"))))) ,
                    fluidRow(
                      bs4Card(width = 12, title = "Team Notes", status = "info",
                              textAreaInput("notes", "Shared Notes", width = "100%", height = "100px")))) ,
            tabItem(tabName = "realtime",
                    fluidRow(bs4Card(width = 12, title = "Streaming Metrics", status = "primary",
                                      plotlyOutput("stream_plot")))) ,
            tabItem(tabName = "admin", h3("Administrative tools"))
          ))
      )
    }
  })

  output$isAdmin <- reactive(creds$role == "admin")
  outputOptions(output, "isAdmin", suspendWhenHidden = FALSE)

  observeEvent(input$login, {
    user <- validate_user(input$user, input$password)
    if (!is.null(user)) {
      creds$valid <- TRUE
      creds$user <- user$username
      creds$role <- user$role
      creds$prefs <- load_prefs(user$username)
      audit_log(user$username, "login_success")
    } else {
      output$error <- renderText("Invalid credentials")
      audit_log(if(!is.null(input$user)) input$user else "", "login_failed")
    }
  })

  observeEvent(input$logout, {
    audit_log(creds$user, "logout")
    creds$valid <- FALSE
    creds$user <- NULL
    creds$role <- NULL
  })

  observeEvent(input$theme, {
    creds$prefs$theme <- if (isTRUE(input$theme)) "dark" else "light"
    bs4Dash::updatebs4DashOptions(session, dark = isTRUE(input$theme))
    save_prefs(creds$user, creds$prefs)
  })

  data <- reactive({
    req(input$file)
    readr::read_csv(input$file$datapath, show_col_types = FALSE)
  })

  output$preview <- renderDT({
    req(data())
    datatable(head(data(), 50))
  })

  results <- eventReactive(input$run, {
    req(data())
    withProgress(message = "Running LCA...", {
      tmp <- tempfile(fileext = ".csv")
      readr::write_csv(data(), tmp)
      out <- tempfile(fileext = ".RData")
      cfg <- load_config("default")
      cfg$lca$nclass <- input$nclass
      run_lca(tmp, out, cfg, verbose = FALSE, show_progress = FALSE)
      load(out)
      audit_log(creds$user, "run_lca")
      lc7
    })
  })

  output$summary <- renderPrint({
    req(results())
    summary(results())
  })

  output$profiles <- renderPlotly({
    req(results())
    probs <- results()$probs
    df <- do.call(rbind, lapply(names(probs), function(var) {
      data.frame(variable = var,
                 category = names(probs[[var]]),
                 probability = probs[[var]],
                 stringsAsFactors = FALSE)
    }))
    p <- ggplot2::ggplot(df, ggplot2::aes(x = category, y = probability, fill = variable)) +
      ggplot2::geom_col(position = "dodge") +
      ggplot2::facet_wrap(~variable, scales = "free_x") +
      ggplot2::labs(x = NULL, y = "Probability") +
      ggplot2::theme_minimal()
    ggplotly(p)
  })

  output$download <- downloadHandler(
    filename = function() paste0("lca_results_", Sys.Date(), ".RData"),
    content = function(file) {
      req(results())
      save(results(), file = file)
    }
  )

  output$report <- downloadHandler(
    filename = function() paste0("lca_report_", Sys.Date(), ".html"),
    content = function(file) {
      req(results())
      rmarkdown::render("templates/lca_report.Rmd", params = list(model = results()),
                        output_file = file, quiet = TRUE)
    }
  )

  observeEvent(input$notes, {
    dir.create("outputs", showWarnings = FALSE)
    writeLines(input$notes, notes_path)
  })

  observe({
    invalidateLater(2000, session)
    if (file.exists(notes_path)) {
      txt <- paste(readLines(notes_path, warn = FALSE), collapse = "\n")
      if (!identical(txt, input$notes)) {
        updateTextAreaInput(session, "notes", value = txt)
      }
    }
  })

  stream_data <- reactiveValues(x = numeric())
  observe({
    invalidateLater(1000, session)
    stream_data$x <- c(stream_data$x, rnorm(1))
  })

  output$stream_plot <- renderPlotly({
    plot_ly(y = stream_data$x, type = "scatter", mode = "lines")
  })
}

shinyApp(ui, server)
