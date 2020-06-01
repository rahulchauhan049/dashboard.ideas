#' plotly UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_plotly_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(
        6,
        selectizeInput(
          ns("scatter_input_1"),
          "Select Column X",
          choices = NULL
        )
      ),
      column(
        6,
        selectizeInput(
          ns("scatter_input_2"),
          "Select Column Y",
          choices = NULL
        )
      )
    ),
    fluidRow(
      plotlyOutput(
        ns("scatter")
      )
    )
  )
}
    
#' plotly Server Function
#'
#' @noRd 
mod_plotly_server <- function(input, output, session, dataset){
  ns <- session$ns
  
  #Scatter Plot X
  observe({
    column_names <- colnames(dataset())
    
    # Can also set the label and select items
    updateSelectInput(session, "scatter_input_1",
                      choices = column_names,
                      selected = "year"
    )
  })
  
  #Scatter Plot y
  observe({
    column_names <- colnames(dataset())
    
    # Can also set the label and select items
    updateSelectInput(session, "scatter_input_2",
                      choices = column_names,
                      selected = "day"
    )
  })
  
   output$scatter <- renderPlotly({
     d <- dataset()[c(input$scatter_input_1, input$scatter_input_2)]
     colnames(d) <- c("x", "y")
     fig <- plot_ly(
       d, x = ~x, y = ~y,
       color = ~x, size = ~x
     )
   })


}
    
## To be copied in the UI
# mod_plotly_ui("plotly_ui_1")
    
## To be copied in the server
# callModule(mod_plotly_server, "plotly_ui_1")
 
