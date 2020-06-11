#' field_selection UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_field_selection_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(
      actionButton(ns("show"), "Show modal dialog"),
      div(style = "display:none;",
          checkboxGroupInput(ns("checkboxgroup_spatial"), "Input checkbox 2",
                             c("Item A", "Item B", "Item C")
          )
      )
    )
  )
}
    
#' field_selection Server Function
#'
#' @noRd 
mod_field_selection_server <- function(input, output, session, dataset, pre_selected){
  ns <- session$ns
  missing <- vector()
  x <- vector()
  choices <- vector()
  a <- vector()
  column_present <- c(
    #spatial Columns
    "countryCode",
    "locality",
    "decimalLatitude",
    "decimalLongitude",
    "verbatimLatitude",
    "verbatimLongitude",
    "coordinateUncertaintyInMeters",
    "coordinatePrecision",
    "elevation",
    "elevationAccuracy",
    "depth",
    "depthAccuracy",
    "establishmentMeans",
    
    #Taxonomic Columns
    
    "kingdom",
    "phylum",
    "order",
    "family",
    "genus",
    "species",
    "name",
    "taxonRank",
    "scientificName",
    "taxonKey",
    "speciesKey",
    "identifiedBy",
    "dateIdentified",
    "recordedBy",
    "recordNumber",
    "typeStatus",
    
    #temporal Columns
    
    "eventDate",
    "day",
    "month",
    "year",
    "dateIdentified",
    "lastInterpreted",
    "dateModified",
    "datecollected",
    "begin_date",
    "observed_on",
    "date"
  )
  
  
  name_with_missing_number <- reactive({
    
    df <- dataset()
    missing_name <- vector()
    names <- vector()
    total_records <- vector()
    missing_records <- vector()
    records_percentage <- vector()

    for(i in colnames(df)){
        names <- c(names,i)
        total_records <- c(
          total_records,
          nrow(df[i])
        )
        missing_records <- c(
          missing_records,
          sum(
            is.na(
              df[i]
            )
          )
        )
        records_percentage <- c(
          records_percentage,
          round(
            (
              (
                nrow(
                  df[i]
                ) - sum(
                  is.na(
                    df[i]
                  )
                )
              ) /
                nrow(
                  df[i]
                )
            ),
            2
          ) * 100
        )
      
    }
    return (setNames(as.list(records_percentage), names) ) 
  })

  
 
  add_row <- function(id1, id2, col_name, selected = FALSE){

    if(col_name %in% colnames(dataset())){
      if(col_name %in% pre_selected){
        selected = TRUE
      }
      fluidRow(
        column(
          6,
          progressBar(id = id1, value = name_with_missing_number()[[col_name]], display_pct = TRUE),
        ),
        column(
          6,
          checkboxInput(
            ns(id2),
            label = col_name,
            value = selected
          )
        )
      )
    }
  }
  

  observeEvent(input[["show"]], {
    showModal(
      modalDialog(
        fluidPage(
          fluidRow(
            column(
              3,
              verbatimTextOutput(ns("te")),
              radioButtons(
                ns("a"),
                label = "", 
                choices = c("Select Default", "Select Core"),
                inline = TRUE, selected = 'Select Default'
              )
            ),
            column(
              3,
              selectInput(
                ns("select_input"),
                label = "",
                choices = c("a","b","c"),
                selected = 'a'
              )
            ),
            column(
              3,
              checkboxInput(
                ns("checkbox1"),
                label = "Select/Deselect All",
                value = TRUE
              )
            )
          ),
          fluidRow(
            column(
              3,
              style = "overflow-y:scroll; max-height: 600px",
              fluidRow(
                column(
                  3,
                  "spatial",
                ),
                column(
                  9,
                  checkboxInput(
                    ns("cehck_select_spatial"),
                    label = "Select/Deselect All",
                    value = FALSE
                  )
                )
              ),
              add_row("pb_countryCode", "cb_countryCode", "countryCode"),
              add_row("pb_locality", "cb_locality", "locality"),
              add_row("pb_decimalLatitude", "cb_decimalLatitude", "decimalLatitude"),
              add_row("pb_decimalLongitude", "cb_decimalLongitude", "decimalLongitude"),
              add_row("pb_coordinateUncertaintyInMeters", "cb_coordinateUncertaintyInMeters", "coordinateUncertaintyInMeters"),
              add_row("pb_coordinatePrecision", "cb_coordinatePrecision", "coordinatePrecision"),
              add_row("pb_elevation", "cb_elevation", "elevation"),
              add_row("pb_elevationAccuracy", "cb_elevationAccuracy", "elevationAccuracy"),
              add_row("pb_depth", "cb_depth", "depth"),
              add_row("pb_depthAccuracy", "cb_depthAccuracy", "depthAccuracy"),
              add_row("pb_verbatimLatitude", "cb_verbatimLatitude", "verbatimLatitude"),
              add_row("pb_verbatimLongitude", "cb_verbatimLongitude", "verbatimLongitude"),
              add_row("pb_establishmentMeans", "cb_establishmentMeans", "establishmentMeans")
            ),
            column(
              3,
              style = "overflow-y:scroll; max-height: 600px",
              fluidRow(
                column(
                  3,
                  "Taxonomic",
                ),
                column(
                  9,
                  checkboxInput(
                    ns("cehck_select_taxonomic"),
                    label = "Select/Deselect All",
                    value = TRUE
                  )
                )
              ),
              add_row("pb_kingdom", "cb_kingdom", "kingdom"),
              add_row("pb_phylum", "cb_phylum", "phylum"),
              add_row("pb_order", "cb_order", "order"),
              add_row("pb_family", "cb_family", "family"),
              add_row("pb_genus", "cb_genus", "genus"),
              add_row("pb_species", "cb_species", "species"),
              add_row("pb_name", "cb_name", "name"),
              add_row("pb_taxonRank", "cb_taxonRank", "taxonRank"),
              add_row("pb_scientificName", "cb_scientificName", "scientificName"),
              add_row("pb_taxonKey", "cb_taxonKey", "taxonKey"),
              add_row("pb_speciesKey", "cb_speciesKey", "speciesKey"),
              add_row("pb_identifiedBy", "cb_identifiedBy", "identifiedBy"),
              add_row("pb_recordedBy", "cb_recordedBy", "recordedBy"),
              add_row("pb_recordNumber", "cb_recordNumber", "recordNumber"),
              add_row("pb_typeStatus", "cb_typeStatus", "typeStatus")
            ),
            column(
              3,
              style = "overflow-y:scroll; max-height: 600px",
              fluidRow(
                column(
                  3,
                  "Temporal",
                ),
                column(
                  9,
                  checkboxInput(
                    ns("cehck_select_temporal"),
                    label = "Select/Deselect All",
                    value = TRUE
                  )
                )
              ),
              add_row("pb_eventDate", "cb_eventDate", "eventDate"),
              add_row("pb_day", "cb_day", "day"),
              add_row("pb_month", "cb_month", "month"),
              add_row("pb_year", "cb_year", "year"),
              add_row("pb_dateIdentified", "cb_dateIdentified", "dateIdentified"),
              add_row("pb_lastInterpreted", "cb_lastInterpreted", "lastInterpreted"),
              add_row("pb_dateModified", "cb_dateModified", "dateModified"),
              add_row("pb_datecollected", "cb_datecollected", "datecollected"),
              add_row("pb_begin_date", "cb_begin_date", "begin_date"),
              add_row("pb_observed_on", "cb_observed_on", "observed_on"),
              add_row("pb_date", "cb_date", "date")
            ),
            column(
              3,
              style = "overflow-y:scroll; max-height: 600px",
              fluidRow(
                column(
                  3,
                  "Unknown",
                ),
                column(
                  9,
                  checkboxInput(
                    ns("cehck_select_unknown"),
                    label = "Select/Deselect All",
                    value = TRUE
                  )
                )
              ),
              for(i in colnames(dataset())){
                if(i %in% column_present){
                }else{
                  missing <- c(missing, i)
                }
              },
              lapply(missing, function(i){
                if(name_with_missing_number()[[i]]){
                  if(i %in% pre_selected){
                    selected = TRUE
                  }
                  fluidRow(
                    column(
                      6,
                      progressBar(id = ns(paste0("pb_",i)), value = name_with_missing_number()[[i]], display_pct = TRUE),
                    ),
                    column(
                      6,
                      checkboxInput(
                        ns(paste0("cb_",i)),
                        label = i,
                        value = FALSE
                      )
                    )
                  )
                }
              })
            )
          )
        ),
        # textInput(ns("dataset"), "Choose data set",
        #           placeholder = 'Try "mtcars" or "abc"', "fasfafsafa"
        # ),
        # span('(Try the name of a valid data object like "mtcars", ',
        #      'then a name of a non-existent object like "abc")'),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("ok"), "OK")
        )
      )
    )
  })
  
  observe({
    for(i in colnames(dataset())){
      if(!is.null(input[[paste0("cb_",i)]])){
        choices <- c(choices, i)
        if(input[[paste0("cb_",i)]]==TRUE){
          x <- c(x, i)
        }
      }
    }
    # Can use character(0) to remove all choices
    if (is.null(x))
      x <- character(0)
    
    # Can also set the label and select items
    updateCheckboxGroupInput(session, "checkboxgroup_spatial",
                             label = paste("Checkboxgroup label", length(x)),
                             choices = choices,
                             selected = x
    )
    

  })
  
  
  observeEvent(input$ok,{
    removeModal()
  })
  
  
  return(
    list(
      columns = reactive({input$checkboxgroup_spatial}),
      click = reactive({input$ok}),
      test = reactive({input$dataset}),
      inp = input
    ) 
  )
}
    
## To be copied in the UI
# mod_field_selection_ui("field_selection_ui_1")
    
## To be copied in the server
# callModule(mod_field_selection_server, "field_selection_ui_1")
 
