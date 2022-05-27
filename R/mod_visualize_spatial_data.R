#' visualize_spatial_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
mod_visualize_spatial_data_ui <-
  function(id,
           lblToPlot = "Variable to plot",
           multiple = FALSE) {
    ns <- NS(id)
    tagList(
            shinyjs::hidden(
              selectInput(
                ns("varToPlot"),
                label = lblToPlot,
                choices = NULL,
                multiple = multiple
              )
            ),
            leaflet::leafletOutput(ns("mylfltmap"))
    )
  }

#' visualize_spatial_data Server Functions
#'
#' @noRd
mod_visualize_spatial_data_server <-
  function(id,
           dataset,
           vars,
           poly = reactive(NULL),
           maxMarkerToShow = reactive(7000)
           ) {
    stopifnot(is.reactive(dataset))
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observeEvent(dataset() == 1 | vars() == 2, {
      ## INITIAL STATE
      shinyjs::reset("varToPlot")
      shiny::updateSelectInput(
        'varToPlot',
        choices = NULL,
        selected = NULL,
        session = session

      )
    })

    data <- reactive({
      req(inherits(dataset(), "sf"))
      df_sf <- sf::st_transform(dataset(), 4326)

      if (nrow(df_sf) > maxMarkerToShow() & not_null(maxMarkerToShow())) {
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
            df_sf <- df_sf[sample(nrow(df_sf), maxMarkerToShow()), ]
          }
          df_sf
    })

    myLeaflet <- reactive({
      req(inherits(dataset(), "sf"))
      
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

    observeEvent(vars(), {
      req(data())
      req(vars())

      shinyjs::show("varToPlot")

      shiny::updateSelectInput(
        'varToPlot',
        choices = colnames(sf::st_drop_geometry(data())),
        selected = vars()[1],
        session = session

      )
    })


    output$mylfltmap <- leaflet::renderLeaflet({
       myLeaflet()
    })

    observeEvent(input$goDataset, {
      bbox <- req(as.vector(sf::st_bbox(data())))
      
      myMap <- leaflet::leafletProxy("mylfltmap", session)
      if (nrow(data()) < 3000) {
        myMap %>% 
          leaflet::flyToBounds(bbox[1], bbox[2],bbox[3],bbox[4])
      } else {
        myMap %>% 
        leaflet::fitBounds(bbox[1], bbox[2],bbox[3],bbox[4])
      }
     
      
       
    })
    # 
    observeEvent(input$varToPlot, {
      req(data())
      req(input$varToPlot, cancelOutput = TRUE)
      df_sf <- data()

      MyMap <- leaflet::leafletProxy("mylfltmap", session)


      for (i in input$varToPlot) {
        myTgtVct <- df_sf[[i]]

        if (is.numeric(myTgtVct)) {
          myTgtVctrnd <- formatC(myTgtVct, format = "f")
          myPal <-
            sample(c("viridis", "magma", "inferno", "plasma"), 1)
          pal <- leaflet::colorNumeric(myPal, myTgtVct)
          myCol <- pal(myTgtVct)
        } else {
          myTgtVctrnd <- myTgtVct
          myPal <- sample(c("RdYlBu", "Blues"), 1)
          pal <- leaflet::colorFactor(myPal, myTgtVct)

          myCol <- pal(myTgtVct)

        }

        MyMap <-
          MyMap %>%
          leaflet::clearControls() %>%
          leaflet::addCircleMarkers(
            data = df_sf,
            color = myCol,
            label = myTgtVctrnd,
            labelOptions = leaflet::labelOptions(direction = "top"),
            group = i,
            stroke = FALSE,
            fillOpacity = 0.7
          ) %>%
          leaflet::addLegend(
            pal = pal,
            values = myTgtVct,
            title = i,
            opacity = 1.0
          )
      }
      #
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

    observeEvent(poly(), {
      req(poly())
      req(inherits(poly(), "sf"))
      poly <- sf::st_transform(poly(), 4326)
      bbox <- as.vector(sf::st_bbox(poly))

      leaflet::leafletProxy("mylfltmap", session) %>%
        leaflet::clearGroup("Boundary") %>%
        leaflet::addPolygons(
          data = poly,
          group = "Boundary",
          color = "#444444",
          weight = 1,
          smoothFactor = 0.5,
          opacity = 1.0,
          fillOpacity = 0
        ) %>%
        leaflet::addLayersControl(
          baseGroups = c("OSM", "Satellite", "Topographic Map"),
          overlayGroups = "Boundary",
          options = leaflet::layersControlOptions(collapsed = FALSE),
          position = "topleft"
        ) %>%
        leaflet::flyToBounds(bbox[1], bbox[2],bbox[3],bbox[4])

    }, ignoreInit = TRUE)

  })
}

## To be copied in the UI
# mod_visualize_spatial_data_ui("visualize_spatial_data_ui_1")

## To be copied in the server
# mod_visualize_spatial_data_server("visualize_spatial_data_ui_1")
