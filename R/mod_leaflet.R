#' leaflet UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_leaflet_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(
      style = 'padding-top:30px;',
      div(
        id = ns("patients_panel"),
        leafletOutput(ns("mymap"), height = "450"),
        absolutePanel(
          top = 200,
          right = "78%",
          sliderInput(ns("radius"), "Radius",
                      min = 0, max = 10,
                      value = 5),
          checkboxInput(ns("stroke"), "Stroke", value = TRUE),
          sliderInput(ns("weight"), "Weight",
                      min = 0, max = 10,
                      value = 5),
          sliderInput(ns("opacity"), "Opacity",
                      min = 0.1, max = 1.0,
                      value = 0.5)
          
          
        ),
        absolutePanel(
          top = 130,
          right = 20,

          selectInput(
            ns("mapTexture"),
            "Base Maps",
            choices = list(
              "OpenTopoMap" = "OpenTopoMap",
              "OpenStreetMap.Mapnik" = "OpenStreetMap.Mapnik",
              "Stamen.Toner" = "Stamen.Toner",
              "CartoDB.Positron" = "CartoDB.Positron",
              "Esri.NatGeoWorldMap" = "Esri.NatGeoWorldMap",
              "Stamen.Watercolor" = "Stamen.Watercolor",
              "Stamen.Terrain" = "Stamen.Terrain",
              "Esri.WorldImagery" = "Esri.WorldImagery",
              "Esri.WorldTerrain" = "Esri.WorldTerrain",
              "OpenStreetMap.DE" = "OpenStreetMap.DE",
              "OpenStreetMap.France" = "OpenStreetMap.France",
              "OpenStreetMap.HOT" = "OpenStreetMap.HOT",
              "Stamen.Toner",
              "Stamen.TonerBackground",
              "Stamen.TonerLite",
              "Stamen.Watercolor",
              "Stamen.Terrain",
              "Stamen.TerrainBackground",
              "Stamen.TerrainLabels",
              "Esri.WorldStreetMap",
              "Esri.DeLorme",
              "Esri.WorldTopoMap",
              "Esri.WorldImagery",
              "Esri.WorldTerrain",
              "Esri.WorldShadedRelief",
              "Esri.WorldPhysical",
              "Esri.OceanBasemap",
              "Esri.NatGeoWorldMap",
              "Esri.WorldGrayCanvas",
              "MtbMap",
              "CartoDB.Positron",
              "CartoDB.PositronNoLabels",
              "CartoDB.PositronOnlyLabels",
              "CartoDB.DarkMatter",
              "CartoDB.DarkMatterNoLabels",
              "CartoDB.DarkMatterOnlyLabels",
              "CartoDB.Voyager",
              "CartoDB.VoyagerNoLabels",
              "CartoDB.VoyagerOnlyLabels",
              "CartoDB.VoyagerLabelsUnder",
              "HikeBike.HikeBike",
              "NASAGIBS.ModisTerraTrueColorCR",
              "NASAGIBS.ModisTerraBands367CR",
              "NASAGIBS.ViirsEarthAtNight2012",
              "Wikimedia",
              "GeoportailFrance.ignMaps",
              "GeoportailFrance.maps",
              "GeoportailFrance.orthos",
              "OpenRailwayMap",
              "SafeCast",
              "Stamen.TonerHybrid"
            ),
            selected = "CartoDB.DarkMatter"
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
          ),
          checkboxInput(ns("fillColor"), "Fill Color", value = TRUE),
          sliderInput(ns("fillOpacity"), "Fill Opacity",
                      min = 0.1, max = 1.0,
                      value = 0.5),
          checkboxInput(ns("fill"), "Fill", value = TRUE)
        )
      )
    ),
    fluidRow(
      column(
        width = 3,
        "OpenTopoMap",
        leafletOutput(ns("OpenTopoMap"), height = "250")
      ),
      column(
        width = 3,
        "OpenStreetMap.Mapnik",
        leafletOutput(ns("OpenStreetMap.Mapnik"), height = "250")
      ),
      column(
        width = 3,
        "Stamen.Toner",
        leafletOutput(ns("Stamen.Toner"), height = "250")
      ),
      column(
        width = 3,
        "CartoDB.Positron",
        leafletOutput(ns("CartoDB.Positron"), height = "250")
      )
    ),
    fluidRow(
      column(
        width = 3,
        "Esri.NatGeoWorldMap",
        leafletOutput(ns("Esri.NatGeoWorldMap"), height = "250")
      ),
      column(
        width = 3,
        "Stamen.Watercolor",
        leafletOutput(ns("Stamen.Watercolor"), height = "250")
      ),
      column(
        width = 3,
        "Stamen.Terrain",
        leafletOutput(ns("Stamen.Terrain"), height = "250")
      ),
      column(
        width = 3,
        "Esri.WorldImagery",
        leafletOutput(ns("Esri.WorldImagery"), height = "250")
      )
    ),
    fluidRow(
      column(
        width = 3,
        "Esri.WorldTerrain",
        leafletOutput(ns("Esri.WorldTerrain"), height = "250")
      ),
      column(
        width = 3,
        "Stamen.TonerHybrid",
        leafletOutput(ns("Stamen.TonerHybrid"), height = "250")
      ),
      column(
        width = 3,
        "OpenStreetMap.DE",
        leafletOutput(ns("OpenStreetMap.DE"), height = "250")
      ),
      column(
        width = 3,
        "OpenStreetMap.France",
        leafletOutput(ns("OpenStreetMap.France"), height = "250")
      )
    ),
    fluidRow(
      column(
        width = 3,
        "OpenStreetMap.HOT",
        leafletOutput(ns("OpenStreetMap.HOT"), height = "250")
      ),
      column(
        width = 3,
        "SafeCast",
        leafletOutput(ns("SafeCast"), height = "250")
      ),
      column(
        width = 3,
        "Stamen.TonerBackground",
        leafletOutput(ns("Stamen.TonerBackground"), height = "250")
      ),
      column(
        width = 3,
        "Stamen.TonerLite",
        leafletOutput(ns("Stamen.TonerLite"), height = "250")
      )
    ),
    fluidRow(
      column(
        width = 3,
        "OpenRailwayMap",
        leafletOutput(ns("OpenRailwayMap"), height = "250")
      ),
      column(
        width = 3,
        "Stamen.TerrainBackground",
        leafletOutput(ns("Stamen.TerrainBackground"), height = "250")
      ),
      column(
        width = 3,
        "Stamen.TerrainLabels",
        leafletOutput(ns("Stamen.TerrainLabels"), height = "250")
      ),
      column(
        width = 3,
        "Esri.WorldStreetMap",
        leafletOutput(ns("Esri.WorldStreetMap"), height = "250")
      )
    ),
    fluidRow(
      column(
        width = 3,
        "Esri.DeLorme",
        leafletOutput(ns("Esri.DeLorme"), height = "250")
      ),
      column(
        width = 3,
        "Esri.WorldTopoMap",
        leafletOutput(ns("Esri.WorldTopoMap"), height = "250")
      ),
      column(
        width = 3,
        "GeoportailFrance.orthos",
        leafletOutput(ns("GeoportailFrance.orthos"), height = "250")
      ),
      column(
        width = 3,
        "GeoportailFrance.maps",
        leafletOutput(ns("GeoportailFrance.maps"), height = "250")
      )
    ),
    fluidRow(
      column(
        width = 3,
        "Esri.WorldShadedRelief",
        leafletOutput(ns("Esri.WorldShadedRelief"), height = "250")
      ),
      column(
        width = 3,
        "Esri.WorldPhysical",
        leafletOutput(ns("Esri.WorldPhysical"), height = "250")
      ),
      column(
        width = 3,
        "Esri.OceanBasemap",
        leafletOutput(ns("Esri.OceanBasemap"), height = "250")
      ),
      column(
        width = 3,
        "GeoportailFrance.ignMaps",
        leafletOutput(ns("GeoportailFrance.ignMaps"), height = "250")
      )
    ),
    fluidRow(
      column(
        width = 3,
        "Esri.WorldGrayCanvas",
        leafletOutput(ns("Esri.WorldGrayCanvas"), height = "250")
      ),
      column(
        width = 3,
        "MtbMap",
        leafletOutput(ns("MtbMap"), height = "250")
      ),
      column(
        width = 3,
        "Wikimedia",
        leafletOutput(ns("Wikimedia"), height = "250")
      ),
      column(
        width = 3,
        "CartoDB.PositronNoLabels",
        leafletOutput(ns("CartoDB.PositronNoLabels"), height = "250")
      )
    ),
    fluidRow(
      column(
        width = 3,
        "CartoDB.PositronOnlyLabels",
        leafletOutput(ns("CartoDB.PositronOnlyLabels"), height = "250")
      ),
      column(
        width = 3,
        "CartoDB.DarkMatter",
        leafletOutput(ns("CartoDB.DarkMatter"), height = "250")
      ),
      column(
        width = 3,
        "CartoDB.DarkMatterNoLabels",
        leafletOutput(ns("CartoDB.DarkMatterNoLabels"), height = "250")
      ),
      column(
        width = 3,
        "CartoDB.DarkMatterOnlyLabels",
        leafletOutput(ns("CartoDB.DarkMatterOnlyLabels"), height = "250")
      )
    ),
    fluidRow(
      column(
        width = 3,
        "CartoDB.Voyager",
        leafletOutput(ns("CartoDB.Voyager"), height = "250")
      ),
      column(
        width = 3,
        "CartoDB.VoyagerNoLabels",
        leafletOutput(ns("CartoDB.VoyagerNoLabels"), height = "250")
      ),
      column(
        width = 3,
        "CartoDB.VoyagerOnlyLabels",
        leafletOutput(ns("CartoDB.VoyagerOnlyLabels"), height = "250")
      ),
      column(
        width = 3,
        "CartoDB.VoyagerLabelsUnder",
        leafletOutput(ns("CartoDB.VoyagerLabelsUnder"), height = "250")
      )
    ),
    fluidRow(
      column(
        width = 3,
        "HikeBike.HikeBike",
        leafletOutput(ns("HikeBike.HikeBike"), height = "250")
      ),
      column(
        width = 3,
        "NASAGIBS.ModisTerraTrueColorCR",
        leafletOutput(ns("NASAGIBS.ModisTerraTrueColorCR"), height = "250")
      ),
      column(
        width = 3,
        "NASAGIBS.ModisTerraBands367CR",
        leafletOutput(ns("NASAGIBS.ModisTerraBands367CR"), height = "250")
      ),
      column(
        width = 3,
        "NASAGIBS.ViirsEarthAtNight2012",
        leafletOutput(ns("NASAGIBS.ViirsEarthAtNight2012"), height = "250")
      )
    )
    
  )
}
    
#' leaflet Server Function
#'
#' @noRd 
mod_leaflet_server <- function(input, output, session, dataset){
  ns <- session$ns
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
        color = input$mapColor,
        radius = input$radius,
        stroke = input$stroke,
        weight = input$weight,
        opacity = input$opacity,
        fill = input$fill,
        fillColor = input$fillColor,
        fillOpacity = input$fillOpacity,
        group = "circles"
      ) %>%
      addMarkers(
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
        clusterOptions = markerClusterOptions(),
        clusterId = "quakesCluster",
        group = "markers"
      ) %>%
      addMiniMap(tiles = input$mapTexture, toggleDisplay = TRUE,
                 position = "bottomleft") %>%
      htmlwidgets::onRender("
    function(el, x) {
      var myMap = this;
      myMap.on('baselayerchange',
        function (e) {
          myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
        })
    }") %>%
      addEasyButton(easyButton(
        states = list(
          easyButtonState(
            stateName="unfrozen-markers",
            icon="ion-toggle",
            title="Freeze Clusters",
            onClick = JS("
          function(btn, map) {
            var clusterManager =
              map.layerManager.getLayer('cluster', 'quakesCluster');
            clusterManager.freezeAtZoom();
            btn.state('frozen-markers');
          }")
          ),
          easyButtonState(
            stateName="frozen-markers",
            icon="ion-toggle-filled",
            title="UnFreeze Clusters",
            onClick = JS("
          function(btn, map) {
            var clusterManager =
              map.layerManager.getLayer('cluster', 'quakesCluster');
            clusterManager.unfreeze();
            btn.state('unfrozen-markers');
          }")
          )
        )
      )) %>%
      addLayersControl(
        overlayGroups = c("circles", "markers"),
        options = layersControlOptions(collapsed = TRUE)
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
  
  output$OpenTopoMap <- renderLeaflet({
    leaflet() %>% addProviderTiles("OpenTopoMap")
  })

  output$OpenStreetMap.Mapnik <- renderLeaflet({
    leaflet() %>% addProviderTiles("OpenStreetMap.Mapnik")
  })

  output$Stamen.Toner <- renderLeaflet({
    leaflet() %>% addProviderTiles("Stamen.Toner")
  })

  output$CartoDB.Positron <- renderLeaflet({
    leaflet() %>% addProviderTiles("CartoDB.Positron")
  })

  output$Esri.NatGeoWorldMap <- renderLeaflet({
    leaflet() %>% addProviderTiles("Esri.NatGeoWorldMap")
  })

  output$Stamen.Watercolor <- renderLeaflet({
    leaflet() %>% addProviderTiles("Stamen.Watercolor")
  })

  output$Stamen.Terrain <- renderLeaflet({
    leaflet() %>% addProviderTiles("Stamen.Terrain")
  })

  output$Esri.WorldImagery <- renderLeaflet({
    leaflet() %>% addProviderTiles("Esri.WorldImagery")
  })

  output$Esri.WorldTerrain <- renderLeaflet({
    leaflet() %>% addProviderTiles("Esri.WorldTerrain")
  })
  
  output$Stamen.TonerHybrid <- renderLeaflet({
    leaflet() %>% addProviderTiles("Stamen.TonerHybrid")
  })
  
  output$OpenStreetMap.DE <- renderLeaflet({
    leaflet() %>% addProviderTiles("OpenStreetMap.DE")
  })
  
  output$OpenStreetMap.France <- renderLeaflet({
    leaflet() %>% addProviderTiles("OpenStreetMap.France")
  })
  
  output$OpenStreetMap.HOT <- renderLeaflet({
    leaflet() %>% addProviderTiles("OpenStreetMap.HOT")
  })
  
  output$SafeCast <- renderLeaflet({
    leaflet() %>% addProviderTiles("SafeCast")
  })
  
  output$Stamen.TonerBackground <- renderLeaflet({
    leaflet() %>% addProviderTiles("Stamen.TonerBackground")
  })
  
  output$Stamen.TonerLite <- renderLeaflet({
    leaflet() %>% addProviderTiles("Stamen.TonerLite")
  })
  
  output$OpenRailwayMap <- renderLeaflet({
    leaflet() %>% addProviderTiles("OpenRailwayMap")
  })
  
  output$GeoportailFrance.orthos <- renderLeaflet({
    leaflet() %>% addProviderTiles("GeoportailFrance.orthos")
  })
  
  output$Stamen.TerrainBackground <- renderLeaflet({
    leaflet() %>% addProviderTiles("Stamen.TerrainBackground")
  })
  
  output$Stamen.TerrainLabels <- renderLeaflet({
    leaflet() %>% addProviderTiles("Stamen.TerrainLabels")
  })
  
  output$Esri.WorldStreetMap <- renderLeaflet({
    leaflet() %>% addProviderTiles("Esri.WorldStreetMap")
  })
  
  output$Esri.DeLorme <- renderLeaflet({
    leaflet() %>% addProviderTiles("Esri.DeLorme")
  })
  
  output$Esri.WorldTopoMap <- renderLeaflet({
    leaflet() %>% addProviderTiles("Esri.WorldTopoMap")
  })
  
  output$GeoportailFrance.maps <- renderLeaflet({
    leaflet() %>% addProviderTiles("GeoportailFrance.maps")
  })
  
  output$GeoportailFrance.ignMaps <- renderLeaflet({
    leaflet() %>% addProviderTiles("GeoportailFrance.ignMaps")
  })
  
  output$Esri.WorldShadedRelief <- renderLeaflet({
    leaflet() %>% addProviderTiles("Esri.WorldShadedRelief")
  })
  
  output$Esri.WorldPhysical <- renderLeaflet({
    leaflet() %>% addProviderTiles("Esri.WorldPhysical")
  })
  
  output$Esri.OceanBasemap <- renderLeaflet({
    leaflet() %>% addProviderTiles("Esri.OceanBasemap")
  })
  
  output$Esri.NatGeoWorldMap <- renderLeaflet({
    leaflet() %>% addProviderTiles("Esri.NatGeoWorldMap")
  })
  
  output$Esri.WorldGrayCanvas <- renderLeaflet({
    leaflet() %>% addProviderTiles("Esri.WorldGrayCanvas")
  })
  
  output$MtbMap <- renderLeaflet({
    leaflet() %>% addProviderTiles("MtbMap")
  })
  
  output$Wikimedia <- renderLeaflet({
    leaflet() %>% addProviderTiles("Wikimedia")
  })
  
  output$CartoDB.PositronNoLabels <- renderLeaflet({
    leaflet() %>% addProviderTiles("CartoDB.PositronNoLabels")
  })
  
  output$CartoDB.PositronOnlyLabels <- renderLeaflet({
    leaflet() %>% addProviderTiles("CartoDB.PositronOnlyLabels")
  })
  
  output$CartoDB.DarkMatter <- renderLeaflet({
    leaflet() %>% addProviderTiles("CartoDB.DarkMatter")
  })
  
  output$CartoDB.DarkMatterNoLabels <- renderLeaflet({
    leaflet() %>% addProviderTiles("CartoDB.DarkMatterNoLabels")
  })
  
  output$CartoDB.DarkMatterOnlyLabels <- renderLeaflet({
    leaflet() %>% addProviderTiles("CartoDB.DarkMatterOnlyLabels")
  })
  
  output$CartoDB.Voyager <- renderLeaflet({
    leaflet() %>% addProviderTiles("CartoDB.Voyager")
  })
  
  output$CartoDB.VoyagerNoLabels <- renderLeaflet({
    leaflet() %>% addProviderTiles("CartoDB.VoyagerNoLabels")
  })
  
  output$CartoDB.VoyagerOnlyLabels <- renderLeaflet({
    leaflet() %>% addProviderTiles("CartoDB.VoyagerOnlyLabels")
  })
  
  output$CartoDB.VoyagerLabelsUnder <- renderLeaflet({
    leaflet() %>% addProviderTiles("CartoDB.VoyagerLabelsUnder")
  })
  
  output$HikeBike.HikeBike <- renderLeaflet({
    leaflet() %>% addProviderTiles("HikeBike.HikeBike")
  })
  
  output$NASAGIBS.ModisTerraTrueColorCR <- renderLeaflet({
    leaflet() %>% addProviderTiles("NASAGIBS.ModisTerraTrueColorCR")
  })
  
  output$NASAGIBS.ModisTerraBands367CR <- renderLeaflet({
    leaflet() %>% addProviderTiles("NASAGIBS.ModisTerraBands367CR")
  })
  
  output$NASAGIBS.ViirsEarthAtNight2012 <- renderLeaflet({
    leaflet() %>% addProviderTiles("NASAGIBS.ViirsEarthAtNight2012")
  })
  


}
    
## To be copied in the UI
# mod_leaflet_ui("leaflet_ui_1")
    
## To be copied in the server
# callModule(mod_leaflet_server, "leaflet_ui_1")
 
