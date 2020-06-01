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
    ),
    
    #Bar
    fluidRow(
      column(
        4,
        selectizeInput(
          ns("bar_input_1"),
          "Select Column X1",
          choices = NULL
        )
      ),
      column(
        4,
        selectizeInput(
          ns("bar_input_2"),
          "Select Column X2",
          choices = NULL
        )
      ),
      column(
        4,
        selectizeInput(
          ns("bar_input_3"),
          "Select Column Y",
          choices = NULL
        )
      )
    ),
    fluidRow(
      plotlyOutput(
        ns("bar")
      )
    ),
    
    
    #Bubble
    fluidRow(
      column(
        6,
        selectizeInput(
          ns("bubble_input_1"),
          "Select Column X",
          choices = NULL
        )
      ),
      column(
        6,
        selectizeInput(
          ns("bubble_input_2"),
          "Select Column Y",
          choices = NULL
        )
      )
    ),
    fluidRow(
      plotlyOutput(
        ns("bubble")
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
   
   #############################      BAR      ############################
   
   #Bar Plot X1
   observe({
     column_names <- colnames(dataset())
     
     # Can also set the label and select items
     updateSelectInput(session, "bar_input_1",
                       choices = column_names,
                       selected = "day"
     )
   })
   
   #Bar Plot X1
   observe({
     column_names <- colnames(dataset())
     
     # Can also set the label and select items
     updateSelectInput(session, "bar_input_2",
                       choices = column_names,
                       selected = "familyKey"
     )
   })
   
   #Bat Plot y
   observe({
     column_names <- colnames(dataset())
     
     # Can also set the label and select items
     updateSelectInput(session, "bar_input_3",
                       choices = column_names,
                       selected = "year"
     )
   })
   
   output$bar <- renderPlotly({
     d <- dataset()[c(input$bar_input_1, input$bar_input_2, input$bar_input_3)]
     colnames(d) <- c("x1", "x2", "y")
     fig <- plot_ly(d, x = ~x1, y = ~y, type = 'bar', name = input$bar_input_1,
                    marker = list(color = 'rgb(55, 83, 109)'))
     fig <- fig %>% add_trace(y = ~x2, name = input$bar_input_2, marker = list(color = 'rgb(26, 118, 255)'))
     fig <- fig %>% layout(title = 'Bar Chart Example',
                           xaxis = list(
                             title = "",
                             tickfont = list(
                               size = 14,
                               color = 'rgb(107, 107, 107)')),
                           yaxis = list(
                             title = '',
                             titlefont = list(
                               size = 16,
                               color = 'rgb(107, 107, 107)'),
                             tickfont = list(
                               size = 14,
                               color = 'rgb(107, 107, 107)')),
                           legend = list(x = 0, y = 1, bgcolor = 'rgba(255, 255, 255, 0)', bordercolor = 'rgba(255, 255, 255, 0)'),
                           barmode = 'group', bargap = 0.15, bargroupgap = 0.1)
     
     fig
   })
   
   
   
   #############################      Bubble      ############################
   
   #Bar Plot X
   observe({
     column_names <- colnames(dataset())
     
     # Can also set the label and select items
     updateSelectInput(session, "bubble_input_1",
                       choices = column_names,
                       selected = "scientificName"
     )
   })
   
   #Bar Plot Y
   observe({
     column_names <- colnames(dataset())
     
     # Can also set the label and select items
     updateSelectInput(session, "bubble_input_2",
                       choices = column_names,
                       selected = "year"
     )
   })
   

   
   output$bubble <- renderPlotly({
     d <- dataset()[c(input$bubble_input_1, input$bubble_input_2)]
     colnames(d) <- c("x", "y")
     d <- as.data.frame(table(d))
     d <- d[d$Freq != 0,]
     
     fig <- plot_ly(d, x = ~x, y = ~y, type = 'scatter', mode = 'markers', size = ~Freq, color = ~x, colors = 'Paired',
                    sizes = c(10, 50),
                    marker = list(opacity = 0.5, sizemode = 'diameter'),
                    hoverinfo = 'text',
                    text = ~paste(input$bubble_input_1,": ", x, '<br>',input$bubble_input_2,': ', y,
                                  '<br> Freq: ', Freq))
     fig <- fig %>% layout(title = 'Gender Gap in Earnings per University',
                           xaxis = list(showgrid = FALSE),
                           yaxis = list(showgrid = FALSE),
                           showlegend = FALSE)
     
     fig
   })
   


}
    
## To be copied in the UI
# mod_plotly_ui("plotly_ui_1")
    
## To be copied in the server
# callModule(mod_plotly_server, "plotly_ui_1")
 
