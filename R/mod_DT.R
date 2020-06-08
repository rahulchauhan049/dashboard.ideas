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
      verbatimTextOutput(ns("text")),
      br(),
      selectizeInput(
        ns("show_vars"),
        "Columns to show:",
        choices = NULL,
        multiple = TRUE
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
mod_DT_server <- function(input, output, session, dataset, temp){
  ns <- session$ns
  
  output$text <- renderText({
    "This is temperary"
  })
  
  observeEvent(temp$click(),{
    if(!is.null(temp$test())){
      output$text <- renderText({
        temp$test()
      })
    }
  })

  
  
  # observeEvent(temp$vals$click(),{
  #   output$text <- renderText({
  #     temp$vals$test()
  #   })
  # })

  
  
  
  observe({
    choices = colnames(dataset())
    column_names <- vector()
    for(i in choices){
      if(i %in% colnames(dataset())){
        column_names <- c(column_names, i)
      }
    }
    
    # Can also set the label and select items
    updateSelectInput(session, "show_vars",
                      "Select columns to show:",
                      choices = column_names,
                      selected = tail(column_names, 10)
    )
  })
  
  
  
  output$summary_data_table <- DT::renderDT({
    DT::datatable(
      dataset()[input$show_vars],        
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
 
