#' data_summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_data_summary_ui <- function(id){

  ns <- NS(id)
  fluid_design <- function(id, w, x, y, z) {
    fluidRow(
      div(
        id = id,
        column(
          width = 6,
          textOutput(w),
          textOutput(y)
        ),
        column(
          width = 6,
          textOutput(x),
          textOutput(z)
        )
      )
    )
  }
  fluidPage(
    
    fluidRow(
      style = 'padding-top:-50px;',
      column(
        4,
        shinydashboard::valueBoxOutput(
          ns("box_a"),
          width = "100%"
        )
      ),
      column(
        4,
        shinydashboard::valueBoxOutput(
          ns("box_b"),
          width = "100%"
        )
      ),
      column(
        4,
        shinydashboard::valueBoxOutput(
          ns("box_c"),
          width = "100%"
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        introBox(
          bsButton(ns("patients"), 
                   label = "Spatial", 
                   icon = icon("user"), 
                   style = "success bsButton"),
          bsButton(ns("antimicrobials"), 
                   label = "Temporal", 
                   icon = icon("spinner", class = "spinner-box"), 
                   style = "success bsButton"),
          bsButton(ns("diagnostics"), 
                   label = "Taxonomic", 
                   icon = icon("flask", class = "flask-box"), 
                   style = "success bsButton")
          )
      )
    ),
    fluid_design(ns("antimicrobials_panel"), ns("box1"), ns("box2"), ns("box3"), ns("box4")),
    fluid_design(ns("diagnostics_panel"), ns("box5"), ns("box6"), ns("box7"), ns("box8")),
    fluid_design(ns("outcome_panel"), ns("box_los1"), ns("box_los2"), ns("box_los3"), NULL),
    
    fluidRow(
      style = 'padding-top:30px;',
      div(
        id = ns("patients_panel"),
        leafletOutput(ns("mymap"), height = "450"),
        absolutePanel(
          top = 280,
          right = 20,
          selectInput(
            ns("mapTexture"),
            "Map Texture",
            choices = list(
              "OpenTopoMap" = "OpenTopoMap",
              "OpenStreetMap.Mapnik" = "OpenStreetMap.Mapnik",
              "OpenStreetMap.BlackAndWhite" = "OpenStreetMap.BlackAndWhite",
              "Stamen.Toner" = "Stamen.Toner",
              "CartoDB.Positron" = "CartoDB.Positron",
              "Esri.NatGeoWorldMap" = "Esri.NatGeoWorldMap",
              "Stamen.Watercolor" = "Stamen.Watercolor",
              "Stamen.Terrain" = "Stamen.Terrain",
              "Esri.WorldImagery" = "Esri.WorldImagery",
              "Esri.WorldTerrain" = "Esri.WorldTerrain"
            ),
            selected = "OpenTopoMap"
          ),
          selectInput(
            ns("mapColor"),
            "Points Color",
            choices = list(
              "Red" = 'red',
              "Green" = "green",
              "Blue" = "blue",
              "Black" = "black"
            ),
            selected = "blue"
          )
        )
      )
    ),
    fluidRow(
      id = ns("spatial_value_box"),
      style = 'padding-top:30px;',
      column(
        4,
        shinydashboard::valueBoxOutput(
          ns("map_coordinates"),
          width = "100%"
        )
      ),
      column(
        4,
        shinydashboard::valueBoxOutput(
          ns("map_countries"),
          width = "100%"
        )
      ),
      column(
        4,
        shinydashboard::valueBoxOutput(
          ns("map_locality"),
          width = "100%"
        )
      )
    ),
    fluidRow(
      id = ns("spatial_image1"),
      style = 'padding-top:30px;',
      column(
        12,
        imageOutput(
          ns("spatial_image_1"),
          width = "100%",
          height = "100%"
        )
      )
    ),
    fluidRow(
      id = ns("spatial_image2"),
      style = 'padding-top:30px;',
      column(
        12,
        imageOutput(
          ns("spatial_image_2"),
          width = "100%",
          height = "100%"
        )
      )
    ),
    fluidRow(
      id = ns("spatial_image3"),
      style = 'padding-top:30px;',
      column(
        12,
        imageOutput(
          ns("spatial_image_3"),
          width = "100%",
          height = "100%"
        )
      )
    ),
    fluidRow(
      id = ns("temporal_image1"),
      style = 'padding-top:30px;',
      column(
        12,
        imageOutput(
          ns("image1_temporal"),
          width = "100%",
          height = "100%"
        )
      )
    ),
    fluidRow(
      id = ns("temporal_image2"),
      style = 'padding-top:30px;',
      column(
        12,
        imageOutput(
          ns("image2_temporal"),
          width = "100%",
          height = "100%"
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
    
#' data_summary Server Function
#'
#' @noRd 
mod_data_summary_server <- function(input, output, session, dataset){
  ns <- session$ns
  

  

  output$box5 <- renderText("Taxonomic")

  
  
  
  output$box_a <- shinydashboard::renderValueBox({
    
    validate(
      need(length(dataset())>0, 'Please upload/download a dataset first')
    )
    shinydashboard::valueBox(
      value = countup::countup(nrow(dataset())),
      subtitle = "# of Records",
      icon = icon("compass"),
      color = "aqua",
      width = 1
    )
  })
  
  output$box_b <-  shinydashboard::renderValueBox({
    validate(
      need(length(dataset())>0, 'Please upload/download a dataset first')
    )
    
    validate(
      need('name' %in% colnames(dataset()), 'No appropriate Column found')
    )
    shinydashboard::valueBox(
      value = countup::countup(nrow(unique(dataset()["name"]))),
      subtitle = "# of Taxa",
      icon = icon("file-signature"),
      color = "blue",
      width = 1
    )
  })
  
  output$box_c <-  shinydashboard::renderValueBox({
    validate(
      need(length(dataset())>0, 'Please upload/download a dataset first')
    )
    shinydashboard::valueBox(
      value = countup::countup(length(dataset())),
      subtitle = "# of Attributes",
      icon = icon("area-chart"),
      color = "light-blue",
      width = 1
    )
  })
  
  
  
  

  
  
  
  
  # hide the underlying selectInput in sidebar for better design
  
  observeEvent("", {
    shinyjs::hide("patients_panel")
    shinyjs::hide("spatial_value_box")
    shinyjs::hide("antimicrobials_panel")
    shinyjs::hide("diagnostics_panel")
    shinyjs::hide("outcome_panel")
    shinyjs::hide("spatial_image1")
    shinyjs::hide("spatial_image2")
    shinyjs::hide("spatial_image3")
    shinyjs::hide("temporal_image2")
    shinyjs::hide("temporal_image1")
  }, once = TRUE)
  
  observeEvent(input$patients, {
    shinyjs::show("patients_panel")
    shinyjs::show("spatial_value_box")
    shinyjs::show("spatial_image1")
    shinyjs::show("spatial_image2")
    shinyjs::show("spatial_image3")
    shinyjs::hide("diagnostics_panel")
    shinyjs::hide("antimicrobials_panel")
    shinyjs::hide("outcome_panel")
    shinyjs::hide("temporal_image2")
    shinyjs::hide("temporal_image1")
  })
  observeEvent(input$antimicrobials, {
    shinyjs::show("antimicrobials_panel")
    shinyjs::show("temporal_image2")
    shinyjs::show("temporal_image1")
    shinyjs::hide("diagnostics_panel")
    shinyjs::hide("outcome_panel")
    shinyjs::hide("patients_panel")
    shinyjs::hide("spatial_value_box")
    shinyjs::hide("spatial_image1")
    shinyjs::hide("spatial_image2")
    shinyjs::hide("spatial_image3")
  })
  observeEvent(input$diagnostics, {
    shinyjs::show("diagnostics_panel")
    shinyjs::hide("antimicrobials_panel")
    shinyjs::hide("outcome_panel")
    shinyjs::hide("patients_panel")
    shinyjs::hide("spatial_value_box")
    shinyjs::hide("spatial_image1")
    shinyjs::hide("spatial_image2")
    shinyjs::hide("spatial_image3")
    shinyjs::hide("temporal_image2")
    shinyjs::hide("temporal_image1")
  })
  observeEvent(input$outcome, {
    shinyjs::show("outcome_panel")
    shinyjs::hide("diagnostics_panel")
    shinyjs::hide("antimicrobials_panel")
    shinyjs::hide("patients_panel")
    shinyjs::hide("spatial_value_box")
    shinyjs::hide("spatial_image1")
    shinyjs::hide("spatial_image2")
    shinyjs::hide("spatial_image3")
    shinyjs::hide("temporal_image2")
    shinyjs::hide("temporal_image1")
  })
  
  
  
  
  output$mymap <- renderLeaflet({
    dat <- dataset()
    validate(
      need(length(dat)>0, 'Please upload/download a dataset first')
    )
    latitudeName <- "verbatimLatitude"
    longitudeName <- "verbatimLongitude"
    
    if("decimalLatitude" %in% colnames(dat))
    {
      latitudeName <- "decimalLatitude"
    }
    
    if("decimalLongitude" %in% colnames(dat))
    {
      longitudeName <- "decimalLongitude"
    }
    
    validate(
      need(longitudeName %in% colnames(dat), 'No location Data available in Databse to plot map')
    )
    validate(
      need(latitudeName %in% colnames(dat), 'No location Data available in Databse to plot map')
    )
    
    switch (latitudeName,
            "verbatimLatitude" = dat$verbatimLatitude <- as.numeric(dat$verbatimLatitude),
            "decimalLatitude" = dat$decimalLatitude <- as.numeric(dat$decimalLatitude),
    )
    switch (longitudeName,
            "verbatimLongitude" = dat$verbatimLongitude <- as.numeric(dat$verbatimLongitude),
            "decimalLongitude" = dat$decimalLongitude <- as.numeric(dat$decimalLongitude),
    )
    
    leaflet(
      data = na.omit(
        dat[c(latitudeName, longitudeName)]
      )
    ) %>%
      addProviderTiles(
        input$mapTexture
      ) %>%
      addCircles(
        
        switch(
          longitudeName,
          "decimalLongitude" = ~decimalLongitude,
          "verbatimLongitude" = ~verbatimLongitude
        ),
        switch(
          latitudeName,
          "decimalLatitude" = ~decimalLatitude,
          "verbatimLatitude" = ~verbatimLatitude
        ),
        color = input$mapColor
      ) %>%
      fitBounds(
        
        switch(
          longitudeName,
          "decimalLongitude" = ~min(decimalLongitude),
          "verbatimLongitude" = ~min(verbatimLongitude)
        ),
        switch(
          latitudeName,
          "decimalLatitude" = ~min(decimalLatitude),
          "verbatimLatitude" = ~min(verbatimLatitude)
        ),
        switch(
          longitudeName,
          "decimalLatitude" = ~max(decimalLongitude),
          "verbatimLatitude" = ~max(verbatimLongitude)
        ),
        switch(
          latitudeName,
          "decimalLatitude" = ~max(decimalLatitude),
          "verbatimLatitude" = ~max(verbatimLatitude)
        )
        
      ) 
  })
  
  output$map_coordinates <- shinydashboard::renderValueBox({
    dat <- dataset()
    if("verbatimLatitude" %in% colnames(dat))
    {
      latitudeName <- "verbatimLatitude"
    }else {
      latitudeName <- "decimalLatitude"
    }
    
    if("verbatimLongitude" %in% colnames(dat))
    {
      longitudeName <- "verbatimLongitude"
    }else {
      longitudeName <- "decimalLatitude"
    }
    validate(
      need(length(dataset())>0, 'Please upload/download a dataset first')
    )
    latitude <- nrow(
      (
        na.omit(
          dataset()[latitudeName]
        )
      )
    )
    
    longitude <- nrow(
      (
        na.omit(
          dataset()[longitudeName]
        )
      )
    )
    
    shinydashboard::valueBox(
      
      if(latitude>longitude){
        value = countup::countup(longitude)
      } else {
        value = countup::countup(latitude)
      },
      subtitle = "# of Geo Coordinates",
      icon = icon("compass"),
      color = "navy",
      width = 4
    )
  })
  
  output$map_countries <- shinydashboard::renderValueBox({
    df <- dataset()
    
    country_code_column_name <- 'countryCode'
    if('place_guess' %in% colnames(df)){
      country_code_column_name <- 'place_guess'
    } else if('calculatedCountry' %in% colnames(df)){
      country_code_column_name <- 'calculatedCountry'
    } else if('country' %in% colnames(df)){
      country_code_column_name <- 'country'
    } else if('country' %in% colnames(df)){
      country_code_column_name <- 'country'
    } 
    
    validate(
      need(length(df)>0, 'Please upload/download a dataset first')
    )
    
    validate(
      need(country_code_column_name %in% colnames(df), 'No appropriate Column found with country names in it.')
    )
    
    shinydashboard::valueBox(
      value = countup::countup(nrow(
        unique(
          na.omit(
            dataset()[country_code_column_name]
          )
        )
      )),
      subtitle = "# of Countries",
      icon = icon("copyright"),
      color = "navy",
      width = 4
    )
  })
  
  output$map_locality <- shinydashboard::renderValueBox({
    validate(
      need(length(dataset())>0, 'Please upload/download a dataset first')
    )
    
    validate(
      need('locality' %in% colnames(dataset()), 'No appropriate Column found with locality data.')
    )
    
    shinydashboard::valueBox(
      value = countup::countup(nrow(
        unique(
          na.omit(
            dataset()["locality"]
          )
        )
      )),
      subtitle = "# of Localities",
      icon = icon("street-view"),
      color = "navy",
      width = 4
    )
  })
  
  output$spatial_image_1 <- renderImage({
    
    
    list(src = "inst/app/www/images/1.png",
         alt = paste("Image number"), style="height: 100%;width: 100%;")
  }, deleteFile = FALSE
    
  )
  
  output$spatial_image_2 <- renderImage({
    
    
    list(src = "inst/app/www/images/4.png",
         alt = paste("Image number"), style="height: 100%;width: 100%;")
  }, deleteFile = FALSE
  
  )
  
  output$spatial_image_3 <- renderImage({
    
    
    list(src = "inst/app/www/images/2.png",
         alt = paste("Image number"), style="height: 100%;width: 100%;")
  }, deleteFile = FALSE
  
  )
  
  output$image1_temporal <- renderImage({
    list(src = "inst/app/www/images/3.png",
         alt = paste("Image number"), style="height: 100%;width: 100%;")
  }, deleteFile = FALSE
  
  )
  output$image2_temporal <- renderImage({
    list(src = "inst/app/www/images/5.png",
         alt = paste("Image number"), style="height: 100%;width: 100%;")
  }, deleteFile = FALSE
  
  )
  
  
  
  
  output$summary_data_table <- DT::renderDT({
    DT::datatable(dataset(),options = list(scrollX = TRUE, pageLength = 5),style = "bootstrap")
  })
 
}
    
## To be copied in the UI
# mod_data_summary_ui("data_summary_ui_1")
    
## To be copied in the server
# callModule(mod_data_summary_server, "data_summary_ui_1")
 
