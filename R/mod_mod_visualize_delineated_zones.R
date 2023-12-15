#' mod_visualize_delineated_zones UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_mod_visualize_delineated_zones_ui <- function(id,
                                                  lblToPlot = "Cluster to plot",
                                                  multiple = FALSE) {
  ns <- NS(id)
  tagList(shinyjs::hidden(
    selectInput(
      ns("varToPlot"),
      label = lblToPlot,
      choices = NULL,
      multiple = multiple
    )
  ),
  leaflet::leafletOutput(ns("mylfltmap")))
}
    
#' mod_visualize_delineated_zones Server Functions
#'
#' @noRd 
mod_mod_visualize_delineated_zones_server <- function(id,
                                                      dataset,
                                                      vars,
                                                      poly = reactive(NULL),
                                                      maxMarkerToShow = reactive(20000)
){
  stopifnot(is.reactive(dataset))
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    
    observeEvent(dataset(), {
      ## INITIAL STATE
      shinyjs::reset("varToPlot", asis = TRUE)
      shiny::updateSelectInput(
        'varToPlot',
        choices = NULL,
        selected = NULL,
        session = session
        
      )
    })
    
    
    data <- reactive({
      req(inherits(dataset(), "sf"))
      validate(need(!is.na(sf::st_crs(dataset())),
                    message = "Select a correct Original EPSG code."))
      df_sf <- sf::st_transform(dataset(), 4326)
      if (nrow(df_sf) > maxMarkerToShow() &
          not_null(maxMarkerToShow())) {
        showNotification(
          paste0(
            'Data has ',
            nrow(df_sf),
            ' observations, but ',
            maxMarkerToShow(),
            ' will be plot'
          ),
          id = ns('maxRows'),
          type = "message",
          session = session
        )
        df_sf <- df_sf[sample(nrow(df_sf), maxMarkerToShow()),]
      }
      golem::print_dev('Data for plot...')
      df_sf
    })
    
    myLeaflet <- reactive({
      req(inherits(data(), "sf") & !is.na(sf::st_crs(data())))
      golem::print_dev('myLeaflet...')
      tryCatch({
        leaflet::leaflet() %>%
          leaflet::addTiles(group = "OSM") %>%
          leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
          leaflet::addProviderTiles("Esri.WorldTopoMap", group = "Topographic Map") %>%
          leaflet::addProviderTiles("Esri.WorldStreetMap") %>%
          leaflet::addMiniMap(tiles = "Esri.WorldStreetMap",
                              toggleDisplay = TRUE,
                              position = "bottomleft") %>%
          leaflet::addEasyButton(
            leaflet::easyButton(
              id = "buttonid",
              icon = "fa-crosshairs",
              title = "Locate dataset",
              onClick =  leaflet::JS(
                paste0(
                  "function(btn, map) {",
                  "Shiny.onInputChange('",
                  ns('goDataset'),
                  "', 'clicked', {priority: 'event'});
                                }"
                )
              )
            )
          )
      }, warning = function(w) {
        showNotification(
          'Please check coordinates!',
          id = ns('leaflet'),
          type = "error",
          session = session
        )
        return()
      }, error = function(e) {
        showNotification(
          'Please check coordinates!',
          id = ns('leaflet'),
          type = "error",
          session = session
        )
        return()
      })
      
    })
    
    output$mylfltmap <- leaflet::renderLeaflet({
      req(data())
      myLeaflet()
    })
    
    
    observeEvent(input$varToPlot, {
      req(data())
      # req(input$varToPlot, cancelOutput = TRUE)
      req(input$varToPlot)
      df_sf <- data()
      MyMap <- leaflet::leafletProxy("mylfltmap", session)
      
      
      for (i in input$varToPlot) {
        myTgtVct <- df_sf[[i]]
        req(myTgtVct)
        if (i == "") break
        myCol <- NULL
        if (length(unique(myTgtVct)) < 15 & !is.null(myTgtVct)) {
          myTgtVct <- as.factor(myTgtVct)
        }
        if (is.numeric(myTgtVct) & !is.null(myTgtVct)) {
          myTgtVctrnd <- formatC(myTgtVct, format = "f")
          myPal <-
            sample(c("viridis", "magma", "inferno", "plasma"), 1)
          pal <- leaflet::colorNumeric(myPal, myTgtVct)
          myCol <- pal(myTgtVct)
        } 
        if (!is.numeric(myTgtVct) & !is.null(myTgtVct)) {
          myTgtVctrnd <- myTgtVct
          myPal <- sample(c("RdYlBu", "Blues"), 1)
          pal <- leaflet::colorFactor(myPal, myTgtVct)
          
          myCol <- pal(myTgtVct)
          
        }
        
        MyMap <-
          MyMap %>%
          leaflet::clearControls()
        
        if (has_sf_points(df_sf)) {
          MyMap <-
            MyMap %>%
            leaflet::addCircleMarkers(
              data = select_sf_points(df_sf),
              color = myCol,
              label = myTgtVctrnd,
              labelOptions = leaflet::labelOptions(direction = "top"),
              group = i,
              stroke = FALSE,
              fillOpacity = 0.7
            )
        }
        if (has_sf_polygon(df_sf)) {
          MyMap <-
            MyMap %>%
            leaflet::addPolygons(
              data = select_sf_polygon(df_sf),
              color = myCol,
              label = myTgtVctrnd,
              labelOptions = leaflet::labelOptions(direction = "top"),
              group = i,
              stroke = FALSE,
              fillOpacity = 0.7
            )
        }
        if (has_sf_multipoints(df_sf)) {
          df_sf_p <- sf::st_cast(select_sf_multipoints(df_sf), 'POINT')
          MyMap <-
            MyMap %>%
            leaflet::addCircleMarkers(
              data = select_sf_points(df_sf_p),
              color = myCol,
              label = myTgtVctrnd,
              labelOptions = leaflet::labelOptions(direction = "top"),
              group = i,
              stroke = FALSE,
              fillOpacity = 0.7
            )
        }
        MyMap <-
          MyMap %>%
          leaflet::clearControls() %>%
          leaflet::addLegend(
            pal = pal,
            values = myTgtVct,
            title = i,
            opacity = 1.0
          )
        
      }
      tryCatch({
        MyMap %>%
          leaflet::addLayersControl(
            baseGroups = c("OSM", "Satellite", "Topographic Map"),
            overlayGroups = input$varToPlot,
            options = leaflet::layersControlOptions(collapsed = FALSE),
            position = "topleft"
          )
      }, warning = function(w) {
        showNotification(
          'Please check coordinates!',
          id = ns('leaflet'),
          type = "error",
          session = session
        )
        return()
      }, error = function(e) {
        showNotification(
          'Please check coordinates!',
          id = ns('leaflet'),
          type = "error",
          session = session
        )
        return()
      })
    })
    
    
    
  })
}
    
## To be copied in the UI
# mod_mod_visualize_delineated_zones_ui("mod_visualize_delineated_zones_1")
    
## To be copied in the server
# mod_mod_visualize_delineated_zones_server("mod_visualize_delineated_zones_1")
