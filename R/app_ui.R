#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny shinydashboard leaflet dplyr dashboardthemes plotly
#' @import flexdashboard DT tidyr
#' @import formattable leaflet.extras sp bdutilities.app rintrojs shinyBS shinyjs countup
#' @noRd
app_ui <- function(request) {
  dashboardPage(
    skin = "yellow",
    dashboardHeader(title = "bddashboard Demo"),
    dashboardSidebar(
      sidebarMenu(
        id = "sideBar",
        menuItem(
          "Data Input",
          tabName = "dataInputTab",
          icon = icon("database")
        ),
        menuItem(
          "Data Overview",
          icon = icon("eye"),
          menuSubItem(
            "Data Summary",
            tabName = "dataSummary"
          ),
          menuSubItem(
            "Missing Data Overview",
            tabName = "missing_overview"
          )
        ),
        menuItem(
          "Leaflet",
          tabName = "leaflet",
          icon=icon("map-marked")
        ),
        menuItem(
          "DT",
          tabName = "DT",
          icon=icon("database")
        ),
        menuItem(
          "Plotly",
          tabName = "Plotly",
          icon = icon("eye")
        ),
        menuItem("Citations",
                 tabName = "cite",
                 icon = icon("university")
        )
      )
    ),
    dashboardBody(
      shinyDashboardThemes(
        theme = "grey_dark"
      ),
      golem_add_external_resources(),
      useShinyjs(),
      tabItems(
        tabItem(
          tabName = "dataInputTab",
          bdutilities.app::mod_add_data_ui("bdFileInput"),
          bdutilities.app::mod_darwinize_ui("darwinize")
        ),
        tabItem(
          tabName = "dataSummary",
          mod_data_summary_ui("data_summary_ui_1")
        ),
        tabItem(
          tabName = "missing_overview",
          mod_missing_data_ui("missing_data_ui_1")
        ),
        tabItem(
          tabName = "leaflet",
          mod_leaflet_ui("leaflet_ui_1")
        ),
        tabItem(
          tabName = "DT",
          mod_DT_ui("DT_ui_1")
        ),
        tabItem(
          tabName = "Plotly",
          mod_plotly_ui("plotly_ui_1")
        ),
        tabItem(
          "cite",
          fluidRow(
          div(
            bdutilities.app::mod_citation_ui("bdcite", "dashboard.demo")
            )
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources 
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'dashboard.ideas'
    ),
    tags$link(rel = "stylesheet", type = "text/css", href = "www/custom.css")
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

