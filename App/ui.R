sidebar <- dashboardSidebar(
  sidebarMenu(
    shinydashboard::menuItem("Start Page", tabName = "start", icon = shiny::icon("play"), selected = TRUE),
    shinydashboard::menuItem("Loan", tabName = "loan_page", icon = shiny::icon("wallet"),
    shinydashboard::menuSubItem("Loan Performance", tabName = "loan", icon = shiny::icon("wallet")),
    # shinydashboard::menuSubItem("Pre Processing", tabName = "loan_pre", icon = shiny::icon("wallet")),
    shinydashboard::menuSubItem("Data Exploration", tabName = "loan_data", icon = shiny::icon("wallet")),
    shinydashboard::menuSubItem("Single Data Exploration", tabName = "loan_data_single", icon = shiny::icon("wallet")),
    shinydashboard::menuSubItem("Multiple Data Exploration", tabName = "loan_data_multiple", icon = shiny::icon("wallet")),
    shinydashboard::menuSubItem("Models", tabName = "loan_models", icon = shiny::icon("wallet")),
    shinydashboard::menuSubItem("Explainability", tabName = "loan_explainability", icon = shiny::icon("wallet")),
    shinydashboard::menuSubItem("Stability", tabName = "loan_stability", icon = shiny::icon("wallet"))),
    shinydashboard::menuItem("Time Series", tabName = "time_page", icon = shiny::icon("chart-line"),
    shinydashboard::menuSubItem("Time Series", tabName = "time", icon = shiny::icon("chart-line")),
    shinydashboard::menuSubItem("Data Exploration", tabName = "ts_data", icon = shiny::icon("chart-line")),
    shinydashboard::menuSubItem("Modelling", tabName = "ts_modelling", icon = shiny::icon("chart-line")),
    # shinydashboard::menuItem("Classic XAI", tabName = "ts_classic", icon = shiny::icon("chart-line")),
    shinydashboard::menuSubItem("X-Function", tabName = "ts_xfunction", icon = shiny::icon("chart-line")),
    shinydashboard::menuSubItem("Application", tabName = "ts_usecase", icon = shiny::icon("chart-line")))
  )
)


body <- dashboardBody(
    tabItems(
      startUi("start"),
      loanUi("loan"),
      loanPreUi("loan_pre"),
      loanDataUi("loan_data"),
      loanDataSingleUi("loan_data_single"),
      loanDataMultipleUi("loan_data_multiple"),
      loanModelsUi("loan_models"),
      loanExplainabilityUi("loan_explainability"),
      loanStabilityUi("loan_stability"),
      timeUi("time"),
      xfunctionUi("ts_xfunction"),
      tsModellingUi("ts_modelling"),
      tsUsecaseUi("ts_usecase"),
      tsExplorationUi("ts_data"),
      tsClassicUi("ts_classic")
    )
)


dashboardPage(
  skin = "blue",
  header = dashboardHeader(title = "XAI Finance"),
  sidebar = sidebar,
  body = body
)
