#' DT UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_DT_ui <- function(id){
  
  ns <- NS(id)
  fluidPage(
    fluidRow(
      actionButton(ns("show"), "Show modal dialog"),
      div(style = "display:none;",
          checkboxGroupInput(ns("checkboxgroup_spatial"), "Input checkbox 2",
                             c("Item A", "Item B", "Item C")
          )
      )
    ),
    fluidRow(
      div(
        id = ns("summary_data_table_id"),
        DT::DTOutput(ns("summary_data_table"))
      )
    )
  )
}
    
#' DT Server Function
#'
#' @noRd 
mod_DT_server <- function(input, output, session, dataset, pre_selected, group){
  ns <- session$ns
  
  
  missing <- vector()
  x <- vector()
  choices <- vector()
  a <- vector()
  previously_selected <- vector()
  first_time_pre_selected <- TRUE
  spatial_present <- c(
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
    "establishmentMeans"
  )
  
  temporal_present <- c(
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
  
  taxonomic_present <- c(
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
    "typeStatus"
  )
  core <- c(
    "scientificName",
    "date",
    "observed_on",
    "kingdom",
    "phylum",
    "order",
    "family",
    "genus",
    "species",
    "countryCode",
    "locality",
    "identifiedBy"
  )
  
  column_present <- c(
    spatial_present,
    temporal_present,
    taxonomic_present
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
    return (setNames(as.list(records_percentage), names))
  })
  
  
  
  add_row <- function(id1, id2, col_name, selected = FALSE){
    if(col_name %in% colnames(dataset())){
      if(col_name %in% pre_selected){
        selected = TRUE
      }
      if(first_time_pre_selected && selected){
        selected = TRUE
      }else{
        selected = FALSE
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
  
  create_column <- function(group_name){
    column(
      3,
      style = "overflow-y:scroll; max-height: 600px; border-radius: 25px; border: 2px solid #73AD21; height: 600px;",
      fluidRow(
        column(
          3,
          h4(group_name),
        ),
        column(
          9,
          checkboxInput(
            ns(paste0("check_select_",group_name)),
            label = "Select/Deselect All",
            value = FALSE
          )
        ),
      ),
      lapply(group()[[group_name]], function(i){
        add_row(paste0("pb_",i), paste0("cb_",i), i)
      })
    )
  }
  

  
  
  observeEvent(input$show, {
    showModal(
      modalDialog(
        fluidPage(
          fluidRow(
            div(
              style = "border-radius: 25px;border: 2px solid #73AD21; height: 67px;",
              column(
                3,
                radioButtons(
                  ns("core_or_default"),
                  label = "", 
                  choices = c("Select Default"="default", "Select Core"="core"),
                  inline = TRUE, selected = 'default'
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
                  ns("select_all_checkbox"),
                  label = "Select/Deselect All",
                  value = FALSE
                )
              )
              
            )
          ),
          fluidRow(
            lapply(names(group()), function(i){
              create_column(i)
            })
            
            # column(
            #   3,
            #   style = "overflow-y:scroll; max-height: 600px; border-radius: 25px; border: 2px solid #73AD21; height: 600px;",
            #   fluidRow(
            #     column(
            #       3,
            #       h4("Spatial"),
            #     ),
            #     column(
            #       9,
            #       checkboxInput(
            #         ns("check_select_spatial"),
            #         label = "Select/Deselect All",
            #         value = FALSE
            #       )
            #     )
            #   ),
            #   add_row("pb_countryCode", "cb_countryCode", "countryCode"),
            #   add_row("pb_locality", "cb_locality", "locality"),
            #   add_row("pb_decimalLatitude", "cb_decimalLatitude", "decimalLatitude"),
            #   add_row("pb_decimalLongitude", "cb_decimalLongitude", "decimalLongitude"),
            #   add_row("pb_coordinateUncertaintyInMeters", "cb_coordinateUncertaintyInMeters", "coordinateUncertaintyInMeters"),
            #   add_row("pb_coordinatePrecision", "cb_coordinatePrecision", "coordinatePrecision"),
            #   add_row("pb_elevation", "cb_elevation", "elevation"),
            #   add_row("pb_elevationAccuracy", "cb_elevationAccuracy", "elevationAccuracy"),
            #   add_row("pb_depth", "cb_depth", "depth"),
            #   add_row("pb_depthAccuracy", "cb_depthAccuracy", "depthAccuracy"),
            #   add_row("pb_verbatimLatitude", "cb_verbatimLatitude", "verbatimLatitude"),
            #   add_row("pb_verbatimLongitude", "cb_verbatimLongitude", "verbatimLongitude"),
            #   add_row("pb_establishmentMeans", "cb_establishmentMeans", "establishmentMeans")
            # ),
            # column(
            #   3,
            #   style = "overflow-y:scroll; max-height: 600px; border-radius: 25px; border: 2px solid #73AD21; height: 600px;",
            #   fluidRow(
            #     column(
            #       3,
            #       h4("Taxonomic"),
            #     ),
            #     column(
            #       9,
            #       checkboxInput(
            #         ns("check_select_taxonomic"),
            #         label = "Select/Deselect All",
            #         value = FALSE
            #       )
            #     )
            #   ),
            #   add_row("pb_kingdom", "cb_kingdom", "kingdom"),
            #   add_row("pb_phylum", "cb_phylum", "phylum"),
            #   add_row("pb_order", "cb_order", "order"),
            #   add_row("pb_family", "cb_family", "family"),
            #   add_row("pb_genus", "cb_genus", "genus"),
            #   add_row("pb_species", "cb_species", "species"),
            #   add_row("pb_name", "cb_name", "name"),
            #   add_row("pb_taxonRank", "cb_taxonRank", "taxonRank"),
            #   add_row("pb_scientificName", "cb_scientificName", "scientificName"),
            #   add_row("pb_taxonKey", "cb_taxonKey", "taxonKey"),
            #   add_row("pb_speciesKey", "cb_speciesKey", "speciesKey"),
            #   add_row("pb_identifiedBy", "cb_identifiedBy", "identifiedBy"),
            #   add_row("pb_recordedBy", "cb_recordedBy", "recordedBy"),
            #   add_row("pb_recordNumber", "cb_recordNumber", "recordNumber"),
            #   add_row("pb_typeStatus", "cb_typeStatus", "typeStatus")
            # ),
            # column(
            #   3,
            #   style = "overflow-y:scroll; max-height: 600px; border-radius: 25px; border: 2px solid #73AD21; height: 600px;",
            #   fluidRow(
            #     column(
            #       3,
            #       h4("Temporal"),
            #     ),
            #     column(
            #       9,
            #       checkboxInput(
            #         ns("check_select_temporal"),
            #         label = "Select/Deselect All",
            #         value = FALSE
            #       )
            #     )
            #   ),
            #   add_row("pb_eventDate", "cb_eventDate", "eventDate"),
            #   add_row("pb_day", "cb_day", "day"),
            #   add_row("pb_month", "cb_month", "month"),
            #   add_row("pb_year", "cb_year", "year"),
            #   add_row("pb_dateIdentified", "cb_dateIdentified", "dateIdentified"),
            #   add_row("pb_lastInterpreted", "cb_lastInterpreted", "lastInterpreted"),
            #   add_row("pb_dateModified", "cb_dateModified", "dateModified"),
            #   add_row("pb_datecollected", "cb_datecollected", "datecollected"),
            #   add_row("pb_begin_date", "cb_begin_date", "begin_date"),
            #   add_row("pb_observed_on", "cb_observed_on", "observed_on"),
            #   add_row("pb_date", "cb_date", "date")
            # ),
            # column(
            #   3,
            #   style = "overflow-y:scroll; max-height: 600px; border-radius: 25px; border: 2px solid #73AD21; height: 600px;",
            #   fluidRow(
            #     column(
            #       3,
            #       h4("Unknown"),
            #     ),
            #     column(
            #       9,
            #       checkboxInput(
            #         ns("check_select_unknown"),
            #         label = "Select/Deselect All",
            #         value = FALSE
            #       )
            #     )
            #   ),
            #   # for(i in colnames(dataset())){
            #   #   if(i %in% column_present){
            #   #   }else{
            #   #     missing <- c(missing, i)
            #   #   }
            #   # },
            #   lapply(group()[["unlisted"]], function(i){
            #     if(!is.null(name_with_missing_number()[[i]])){
            #       if(i %in% pre_selected){
            #         selected = TRUE
            #       }
            #       fluidRow(
            #         column(
            #           6,
            #           progressBar(id = ns(paste0("pb_",i)), value = name_with_missing_number()[[i]], display_pct = TRUE),
            #         ),
            #         column(
            #           6,
            #           checkboxInput(
            #             ns(paste0("cb_",i)),
            #             label = i,
            #             value = FALSE
            #           )
            #         )
            #       )
            #     }
            #   })
            # )
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
    first_time_pre_selected <<- FALSE
  })
  
  
  
  first_time <- TRUE
  
  
  observe({
    lapply(names(group()), function(i){
      observeEvent(input[[paste0("check_select_",i)]],{
        if(input[[paste0("check_select_",i)]]){
          for(i in group()[[i]]){
            if(i %in% colnames(dataset())){
              updateCheckboxInput(session, paste0("cb_",i), value = TRUE)
            }
          }
        }else{
          for(i in group()[[i]]){
            if(i %in% colnames(dataset())){
              updateCheckboxInput(session, paste0("cb_",i), value = FALSE)
            }
          }
        }
      }, ignoreInit = TRUE)
    })
  })
  


  
  #Spatial Select All Button
  # observeEvent(input$check_select_spatial,{
  #   if(input$check_select_spatial){
  #     for(i in spatial_present){
  #       if(i %in% colnames(dataset())){
  #         updateCheckboxInput(session, paste0("cb_",i), value = TRUE)
  #       }
  #     }
  #   }else{
  #     for(i in spatial_present){
  #       if(i %in% colnames(dataset())){
  #         updateCheckboxInput(session, paste0("cb_",i), value = FALSE)
  #       }
  #     }
  #   }
  # }, ignoreInit = TRUE)
  
  #Temporal Select All Button
  # observeEvent(input$check_select_temporal,{
  #   if(input$check_select_temporal){
  #     for(i in temporal_present){
  #       if(i %in% colnames(dataset())){
  #         updateCheckboxInput(session, paste0("cb_",i), value = TRUE)
  #       }
  #     }
  #   }else{
  #     for(i in temporal_present){
  #       if(i %in% colnames(dataset())){
  #         updateCheckboxInput(session, paste0("cb_",i), value = FALSE)
  #       }
  #     }
  #   }
  # }, ignoreInit = TRUE)
  
  #Taxonomic Select All Button
  # observeEvent(input$check_select_taxonomic,{
  #   if(input$check_select_taxonomic){
  #     for(i in taxonomic_present){
  #       if(i %in% colnames(dataset())){
  #         updateCheckboxInput(session, paste0("cb_",i), value = TRUE)
  #       }
  #     }
  #   }else{
  #     for(i in taxonomic_present){
  #       if(i %in% colnames(dataset())){
  #         updateCheckboxInput(session, paste0("cb_",i), value = FALSE)
  #       }
  #     }
  #   }
  # }, ignoreInit = TRUE)
  
  #Unknown Select All Button
  # observeEvent(input$check_select_unknown,{
  #   if(input$check_select_unknown){
  #     for(i in colnames(dataset())){
  #       if(i %in% column_present){
  #       }else{
  #         updateCheckboxInput(session, paste0("cb_",i), value = TRUE)
  #       }
  #     }
  #   }else{
  #     for(i in colnames(dataset())){
  #       if(i %in% column_present){
  #       }else{
  #         updateCheckboxInput(session, paste0("cb_",i), value = FALSE)
  #       }
  #     }
  #   }
  # }, ignoreInit = TRUE)
  
  # #Select all
  observeEvent(input$select_all_checkbox,{
    if(input$select_all_checkbox){
      for(i in names(group())){
        updateCheckboxInput(session, paste0("check_select_",i), value = TRUE)
      }
    }else{
      for(i in names(group())){
        updateCheckboxInput(session, paste0("check_select_",i), value = FALSE)
      }
    }
  }, ignoreInit = TRUE)
   # observeEvent(input$select_all_checkbox,{
   #   if(input$select_all_checkbox){
   #     updateCheckboxInput(session, "check_select_taxonomic", value = TRUE)
   #     updateCheckboxInput(session, "check_select_spatial", value = TRUE)
   #     updateCheckboxInput(session, "check_select_temporal", value = TRUE)
   #     updateCheckboxInput(session, "check_select_unknown", value = TRUE)
   #   }else{
   #     updateCheckboxInput(session, "check_select_taxonomic", value = FALSE)
   #     updateCheckboxInput(session, "check_select_spatial", value = FALSE)
   #     updateCheckboxInput(session, "check_select_temporal", value = FALSE)
   #     updateCheckboxInput(session, "check_select_unknown", value = FALSE)
   #   }
   # }, ignoreInit = TRUE)




  

  observeEvent(input$core_or_default,{
      if(input$core_or_default == "default"){
        for(i in colnames(dataset())){
          if(i %in% pre_selected){
            updateCheckboxInput(session, paste0("cb_",i), value = TRUE)
          }else{
            updateCheckboxInput(session, paste0("cb_",i), value = FALSE)
          }
        }
      }else{
        for(i in colnames(dataset())){
          if(i %in% core){
            updateCheckboxInput(session, paste0("cb_",i), value = TRUE)
          }else{
            updateCheckboxInput(session, paste0("cb_",i), value = FALSE)
          }
        }
      }
  },ignoreInit = TRUE)

  
  
  observe({
    x <- vector()
    choices <- vector()
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
  
  
  observeEvent(input[["show"]],{
    first_time <<- TRUE
    print(length(previously_selected))
    if(length(previously_selected)==0){
      for(i in colnames(dataset())){
        if(i %in% pre_selected){
          updateCheckboxInput(session, paste0("cb_",i), value = TRUE)
        }else{
          updateCheckboxInput(session, paste0("cb_",i), value = FALSE)
        }
      }
    }else{
      print(paste0("cb_",previously_selected))
      for(i in colnames(dataset())){
        if(i %in% previously_selected){
          updateCheckboxInput(session, paste0("cb_",i), value = TRUE)
        }else{
          updateCheckboxInput(session, paste0("cb_",i), value = FALSE)
        }
      }
    }
  })
  


  
  
  
  observeEvent(input$ok,{
    previously_selected <<- input$checkboxgroup_spatial
    output$summary_data_table <- DT::renderDT({
      DT::datatable(
        dataset()[previously_selected],        
        filter = 'top',
        extensions = c('Buttons', "ColReorder", "Scroller"), #'Select', 'SearchPanes'
        options = list(
          scrollX = TRUE,
          dom = "Bfrtip",#'Pfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          colReorder = TRUE,
          deferRender = TRUE,
          scrollY = 500,
          scroller = TRUE
          # columnDefs = list(list(
          #   searchPanes = list(show = FALSE), targets = 1:50
          # ))
        ),
        style = "bootstrap"
        #selection = 'none'
      )
    })
    removeModal()
  })

  
  
  filter_selected <- vector()
  output$summary_data_table <- DT::renderDT({
    for(i in pre_selected){
      if(i %in% colnames(dataset()))
        filter_selected <- c(filter_selected, i)
    }
    DT::datatable(
      dataset()[filter_selected],        
      filter = 'top',
      extensions = c('Buttons', "ColReorder", "Scroller"), #'Select', 'SearchPanes'
      options = list(
        scrollX = TRUE,
        dom = "Bfrtip",#'Pfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        colReorder = TRUE,
        deferRender = TRUE,
        scrollY = 500,
        scroller = TRUE
        # columnDefs = list(list(
        #   searchPanes = list(show = FALSE), targets = 1:50
        # ))
      ),
      style = "bootstrap"
      #selection = 'none'
    )
  })
  
 
  
  
  
}
    
## To be copied in the UI
# mod_DT_ui("DT_ui_1")
    
## To be copied in the server
# callModule(mod_DT_server, "DT_ui_1")
 
