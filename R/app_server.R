#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny bdclean
#' @noRd
app_server <- function(input, output, session) {
  # List the first level callModules here
  dictionary <- read.csv("dictionary.csv")
  session$onSessionEnded(stopApp)
  
  options(shiny.maxRequestSize = 5000 * 1024 ^ 2)
  
  pre_selected <- c(
    "countryCode",
    "locality",
    "decimalLatitude",
    "decimalLongitude",
    "verbatimLatitude",
    "verbatimLongitude",
    "coordinateUncertaintyInMeters",
    "coordinatePrecision"
  )
  
  
  
  data_store <-
    shiny::reactiveValues(
      darwinized_data = data.frame(),
      input_data = data.frame()
  )
  
  
  data_store$input_data <- 
    callModule(
      bdutilities.app::mod_add_data_server,
      id = "bdFileInput",
      "darwinControlInner"
    )
  
  
  data_store$darwinized_data <-
    callModule(bdutilities.app::mod_darwinize_server,
               "darwinize",
               dat = data_store$input_data)
  
  group <- reactive(create_group(dictionary, data_store$darwinized_data()))
  
  callModule(mod_data_summary_server, "data_summary_ui_1", data_store$darwinized_data)
  
  callModule(mod_missing_data_server, "missing_data_ui_1", data_store$darwinized_data)
  
  callModule(mod_leaflet_server, "leaflet_ui_1", data_store$darwinized_data)
  
  # temp <- callModule(mod_field_selection_server, "field_selection_ui_1", data_store$darwinized_data, pre_selected)
  
  callModule(mod_DT_server, "DT_ui_1", data_store$darwinized_data, pre_selected, group)
  
  callModule(mod_plotly_server, "plotly_ui_1", data_store$darwinized_data)
  
  callModule(
    bdutilities.app::mod_citation_server,
    id = "bdcite",
    package = "dashboard.demo"
  )
  
}
