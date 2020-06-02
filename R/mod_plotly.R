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
    ),
    
    
    #Hbar
    fluidRow(
      column(
        12,
        selectizeInput(
          ns("hbar_input_1"),
          "Select Column X",
          choices = NULL
        )
      )
    ),
    fluidRow(
      plotlyOutput(
        ns("hbar")
      )
    ),
    
    
    #Sunburst
    fluidRow(
      column(
        4,
        selectizeInput(
          ns("sunburst_input_1"),
          "Select Layer 1",
          choices = NULL
        )
      ),
      column(
        4,
        selectizeInput(
          ns("sunburst_input_2"),
          "Select Layer 2",
          choices = NULL
        )
      ),
      column(
        4,
        selectizeInput(
          ns("sunburst_input_3"),
          "Select Layer 3",
          choices = NULL
        )
      )
    ),
    fluidRow(
      plotlyOutput(
        ns("sunburst")
      )
    ),
    
    
    #Histogram
    fluidRow(
      column(
        6,
        selectizeInput(
          ns("histogram_input_1"),
          "Select Layer 1",
          choices = NULL
        )
      ),
      column(
        6,
        selectizeInput(
          ns("histogram_input_2"),
          "Select Layer 2",
          choices = NULL
        )
      )
    ),
    fluidRow(
      plotlyOutput(
        ns("histogram")
      )
    ),
    
    
    #Radar
    fluidRow(
      column(
        12,
        selectizeInput(
          ns("radar_input_1"),
          "Select Layer 1",
          choices = NULL
        )
      )
    ),
    fluidRow(
      plotlyOutput(
        ns("radar")
      )
    ),
    
    
    
    #Animation
    fluidRow(
      column(
        4,
        selectizeInput(
          ns("animation_input_1"),
          "Select Layer 1",
          choices = NULL
        )
      ),
      column(
        4,
        selectizeInput(
          ns("animation_input_2"),
          "Select Layer 1",
          choices = NULL
        )
      ),
      column(
        4,
        selectizeInput(
          ns("animation_input_3"),
          "Select Layer 1",
          choices = NULL
        )
      )
    ),
    fluidRow(
      plotlyOutput(
        ns("animation")
      )
    ),
    
    
    #Slider
    fluidRow(
      column(
        6,
        selectizeInput(
          ns("slider_input_1"),
          "Select Layer 1",
          choices = NULL
        )
      ),
      column(
        6,
        selectizeInput(
          ns("slider_input_2"),
          "Select Layer 1",
          choices = NULL
        )
      )
    ),
    fluidRow(
      plotlyOutput(
        ns("slider")
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
   
   #Bar Plot y
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
   
   #Bubble Plot X
   observe({
     column_names <- colnames(dataset())
     
     # Can also set the label and select items
     updateSelectInput(session, "bubble_input_1",
                       choices = column_names,
                       selected = "scientificName"
     )
   })
   
   #Bubble Plot Y
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
   
   
   #############################      Hbar      ############################
   
   #HBar Plot X
   observe({
     column_names <- colnames(dataset())
     
     # Can also set the label and select items
     updateSelectInput(session, "hbar_input_1",
                       choices = column_names,
                       selected = "species"
     )
   })
   
   
   
   output$hbar <- renderPlotly({
     d <- dataset()[c(input$hbar_input_1)]
     colnames(d) <- c("x")
     d <- as.data.frame(table(d))
     d <- d[d$Freq != 0,]
     
     fig <- plot_ly(d, x = ~Freq, y = ~d, type = 'bar', orientation = 'h', name = input$hbar_input_1,
                    marker = list(color = 'rgba(246, 78, 139, 0.6)',
                                  line = list(color = 'rgba(246, 78, 139, 1.0)',
                                              width = 3)))

     fig <- fig %>% layout(barmode = 'stack',
                           xaxis = list(title = ""),
                           yaxis = list(title =""))
     
     fig
   })
   
   
   #############################      Sunburst      ############################
   
   #Sunburst Plot X
   observe({
     column_names <- colnames(dataset())
     
     # Can also set the label and select items
     updateSelectInput(session, "sunburst_input_1",
                       choices = column_names,
                       selected = "family"
     )
   })
   
   #Sunburst Plot X
   observe({
     column_names <- colnames(dataset())
     
     # Can also set the label and select items
     updateSelectInput(session, "sunburst_input_2",
                       choices = column_names,
                       selected = "genus"
     )
   })
   
   #Sunburst Plot X
   observe({
     column_names <- colnames(dataset())
     
     # Can also set the label and select items
     updateSelectInput(session, "sunburst_input_3",
                       choices = column_names,
                       selected = "species"
     )
   })
   
   
   
   output$sunburst <- renderPlotly({
     d <- dataset()[c(input$sunburst_input_1, input$sunburst_input_2, input$sunburst_input_3)]
     colnames(d) <- c("x", "y", "z")
     d <- unique(d)
     
     fig <- plot_ly() 

     fig <- fig %>%
       add_trace(
         ids = d$z,
         labels = d$y,
         parents = d$x,
         type = 'sunburst',
         maxdepth = 3,
         domain = list(column = 1)
       ) 
     fig <- fig %>%
       layout(
         sunburstcolorway = c(
           "#636efa","#EF553B","#00cc96","#ab63fa","#19d3f3",
           "#e763fa", "#FECB52","#FFA15A","#FF6692","#B6E880"
         ),
         extendsunburstcolors = TRUE)
     fig
   })
   
   
   #############################      Histogram      ############################
   
   #Histogram Plot X
   observe({
     column_names <- colnames(dataset())
     
     # Can also set the label and select items
     updateSelectInput(session, "histogram_input_1",
                       choices = column_names,
                       selected = "species"
     )
   })
   
   #Histogram Plot X
   observe({
     column_names <- colnames(dataset())
     
     # Can also set the label and select items
     updateSelectInput(session, "histogram_input_2",
                       choices = column_names,
                       selected = "genus"
     )
   })
   

   
   
   output$histogram <- renderPlotly({
     d <- dataset()[c(input$histogram_input_1, input$histogram_input_2)]
     colnames(d) <- c("x", "y")

     fig <- plot_ly(d,
       type='histogram',
       x=~x
     )
     
     fig <- fig %>% add_trace(d,
       type='histogram',
       x=~y
     )
     
     fig <- fig %>% layout(
       barmode="stack",
       bargap=0.1)
     
     fig
   })
   
   
   
   #############################      Radar      ############################
   
   #Histogram Plot X
   observe({
     column_names <- colnames(dataset())
     
     # Can also set the label and select items
     updateSelectInput(session, "radar_input_1",
                       choices = column_names,
                       selected = "species"
     )
   })
   

   
   
   
   output$radar <- renderPlotly({
     d <- dataset()[c(input$radar_input_1)]
     d <- as.data.frame(table(d))
     d <- unique(d)
     d <- d[d$Freq != 0,]
     
     
     fig <- plot_ly(
       type = 'scatterpolar',
       fill = 'toself'
     ) 
     fig <- fig %>%
       add_trace(
         r = d$Freq,
         theta = d$d,
         name = input$radar_input_1
       ) 
     
     fig <- fig %>%
       layout(
         polar = list(
           radialaxis = list(
             visible = T,
             range = c(0,100)
           )
         )
       )
     
     fig
   })
   
   
   
   #############################      Animation      ############################
   
   #Histogram Plot X
   observe({
     column_names <- colnames(dataset())
     
     # Can also set the label and select items
     updateSelectInput(session, "animation_input_1",
                       choices = column_names,
                       selected = "day"
     )
   })
   
   observe({
     column_names <- colnames(dataset())
     
     # Can also set the label and select items
     updateSelectInput(session, "animation_input_2",
                       choices = column_names,
                       selected = "year"
     )
   })
   
   observe({
     column_names <- colnames(dataset())
     
     # Can also set the label and select items
     updateSelectInput(session, "animation_input_3",
                       choices = column_names,
                       selected = "year"
     )
   })
   
   
   
   
   
   output$animation <- renderPlotly({
   d <- dataset()[c(input$animation_input_1, input$animation_input_2, input$animation_input_3)]
   colnames(d) <- c("x", "y", "z")
   
   fig <- d %>%
     plot_ly(
       x = ~x, 
       y = ~y, 
       frame = ~z, 
       text = ~x, 
       hoverinfo = "text",
       type = 'scatter',
       mode = 'markers'
     )
   fig <- fig %>% layout(
     xaxis = list(
       type = "log"
     )
   )
   fig
   })
   
   
   
   #############################      Slider      ############################
   
   #Histogram Plot X
   observe({
     column_names <- colnames(dataset())
     
     # Can also set the label and select items
     updateSelectInput(session, "slider_input_1",
                       choices = column_names,
                       selected = "decimalLatitude"
     )
   })
   
   observe({
     column_names <- colnames(dataset())
     
     # Can also set the label and select items
     updateSelectInput(session, "slider_input_2",
                       choices = column_names,
                       selected = "decimalLongitude"
     )
   })
  
   
   
   output$slider <- renderPlotly({
     a <- dataset()[c(input$slider_input_1, input$slider_input_2)]
     colnames(a) <- c("lat", "lon")
     a$id <- seq_len(nrow(a))
     d <- a %>%
       gather(key, value, -id) %>%
       separate(key, c("l", "line"), "\\.") %>%
       spread(l, value)
     
     geo <- list(
       showland = TRUE,
       showlakes = TRUE,
       showcountries = TRUE,
       showocean = TRUE,
       countrywidth = 0.5,
       landcolor = 'rgb(230, 145, 56)',
       lakecolor = 'rgb(0, 255, 255)',
       oceancolor = 'rgb(0, 255, 255)',
       projection = list(
         type = 'orthographic',
         rotation = list(
           lon = -100,
           lat = 40,
           roll = 0
         )
       ),
       lonaxis = list(
         showgrid = TRUE,
         gridcolor = toRGB("gray40"),
         gridwidth = 0.5
       ),
       lataxis = list(
         showgrid = TRUE,
         gridcolor = toRGB("gray40"),
         gridwidth = 0.5
       )
     )
     
     ## add custom events
     
     # dropdown
     projections = data.frame(type = c("equirectangular", "mercator", "orthographic", "natural earth","kavrayskiy7", 
                                       "miller", "robinson", "eckert4", "azimuthal equal area","azimuthal equidistant", 
                                       "conic equal area", "conic conformal", "conic equidistant", "gnomonic", "stereographic", 
                                       "mollweide", "hammer", "transverse mercator", "albers usa", "winkel tripel"))
     
     all_buttons <- list()
     for (i in 1:length(projections[,])) { 
       all_buttons[[i]] <- list(method = "relayout",
                                args = list(list(geo.projection.type = projections$type[i])),
                                label = projections$type[i])
     }
     
     # sliders
     lon_range = data.frame(seq(-180, 180, 10))
     lat_range = data.frame(seq(-90, 90, 10))
     colnames(lon_range) <- "x"
     colnames(lat_range) <- "x"
     
     all_lat <- list()
     for (i in 1:length(lat_range[,])) { 
       all_lat[[i]] <- list(method = "relayout",
                            args = list(list(geo.projection.rotation.lat = lat_range$x[i])),
                            label = lat_range$x[i])
     }
     
     all_lon <- list()
     for (i in 1:length(lon_range[,])) {  
       all_lon[[i]] <- list(method = "relayout", 
                            args = list(list(geo.projection.rotation.lon = lon_range$x[i])),
                            label = lon_range$x[i]) 
     } 
     
     # annotations
     annot <- list(x = 0, y=0.8, text = "Projection", yanchor = 'bottom', 
                   xref = 'paper', xanchor = 'right',
                   showarrow = FALSE)
     
     
     # original d3-globe with contours
     fig<- plot_geo(d) 
     fig <- fig %>% add_markers(x = ~lon, y = ~lat, colors = 'Reds') 
     fig <- fig %>% layout(
       showlegend = FALSE, geo = geo
     )
     
     # plot with custom events
     fig<- fig
     fig <- fig %>% layout(annotations = annot,
                           updatemenus = list(list(active = 2, x = 0, y = 0.8, 
                                                   buttons=all_buttons)),
                           sliders = list(
                             
                             list(
                               active = (length(lon_range[,])-1)/2, 
                               currentvalue = list(prefix = "Longitude: "), 
                               pad = list(t = 20), 
                               
                               steps = all_lon),
                             
                             list(
                               active = (length(lat_range[,])-1)/2, 
                               currentvalue = list(prefix = "Latitude: "), 
                               pad = list(t = 100), 
                               
                               steps = all_lat)))
     
     fig
     
   })


}
    
## To be copied in the UI
# mod_plotly_ui("plotly_ui_1")
    
## To be copied in the server
# callModule(mod_plotly_server, "plotly_ui_1")
 
