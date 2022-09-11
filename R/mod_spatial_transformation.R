#' spatial_transformation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_spatial_transformation_ui <- function(id,
                                          lblOrigcde = "Original EPSG code",
                                          lbltgtcde = "Target EPSG code") {
  ns <- NS(id)
  tagList(shinyjs::hidden(
    numericInput(
      ns("epsg_orig"),
      lblOrigcde,
      value = 4326 ,
      min = 1024,
      max = 32766,
      step = 1
    )
  ),
  shinyjs::hidden(
    numericInput(
      ns("epsg_tgt"),
      lbltgtcde,
      value = 32720,
      min = 1024,
      max = 32767,
      step = 1
    )
  ))
}

#' spatial_transformation Server Functions
#'
#' @noRd
mod_spatial_transformation_server <-
  function(id, dataset, coords, readyToShow = reactive(TRUE)) {
    stopifnot(is.reactive(dataset))
    
    moduleServer(id, function(input, output, session) {
      ns <- session$ns
      
      test_epsg_tgt <- reactive({
        req(input$epsg_tgt)
        test_latlong(input$epsg_tgt)
      })
      test_epsg_orig <- reactive({
        req(input$epsg_orig)
        test_latlong(input$epsg_orig)
      })
      
      observeEvent(length(dataset()) == 1, {
        shinyjs::hide("epsg_orig")
        shinyjs::hide("epsg_tgt")
        
        updateNumericInput(
          value = NULL,
          inputId = ns("epsg_orig"),
          session = session
        )
        epsg <- 32720
        if (inherits(dataset(), 'sf')) {
          epsg <- guess_utm(dataset())
        }
        updateNumericInput(
          value = epsg,
          inputId = ns("epsg_tgt"),
          session = session
        )
      })
      
      
      
      observeEvent(length(dataset()) == 1 |
                     length(coords()) == 2 |
                     length(readyToShow()) == 3,
                   {
                     ### INITIAL STATE
                     ## reset if dataset changed
                     shinyjs::hide("epsg_orig")
                     shinyjs::hide("epsg_tgt")
                     
                     if (!inherits(dataset(), "sf")) {
                       req(!is.null(coords()))
                     }
                     # req(!is.null(coords()), cancelOutput = TRUE)
                     req(readyToShow(), cancelOutput = TRUE)
                     if (!inherits(dataset(), "sf")) {
                       shinyjs::delay(500, shinyjs::show('epsg_orig'))
                       shinyjs::delay(500, shinyjs::show('epsg_tgt'))
                     }
                     
                     if (inherits(dataset(), "sf")) {
                       tst <- test_latlong(dataset())
                       
                       if (is.na(tst)) {
                         shinyjs::delay(500, shinyjs::show('epsg_orig'))
                         shinyjs::delay(500, shinyjs::show('epsg_tgt'))
                       }
                       
                       if (isTRUE(tst)) {
                         # shinyjs::show('epsg_orig')
                         shinyjs::show('epsg_tgt')
                       }
                       
                       if (isFALSE(tst)) {
                         # shinyjs::show('epsg_orig')
                         # shinyjs::show('epsg_tgt')
                       }
                       
                       
                     }
                     
                   })
      
      
      observeEvent(test_epsg_tgt(), {
        if (is.na(test_epsg_tgt())) {
          shiny::showNotification(
            "Provide a valid target epsg code",
            type = 'error',
            id = ns('msg_vld_epsgtgt')
          )
        }
        
        if (isTRUE(test_epsg_tgt())) {
          shiny::showNotification(
            "For target epsg, provide a projected coordinate system",
            type = 'error',
            id = ns('msg_vld_epsgtgt')
          )
        }
        
      })
      
      observeEvent(test_epsg_orig, {
        if (is.na(test_epsg_orig())) {
          shiny::showNotification(
            "Provide a valid original epsg code",
            type = 'error',
            id = ns('msg_vld_epsgorg')
          )
        }
        
        
      })
      
      observeEvent(test_epsg_tgt() == 1 | test_epsg_orig() == 1, {
        if ((!(is.na(test_epsg_tgt()) &
               !isTRUE(test_epsg_tgt()))) &
            !is.na(test_epsg_orig())) {
          req(dataset(), cancelOutput = TRUE)
          if (!inherits(dataset(), "sf")) {
            req(coords())
          }
          # print("No deberia llegar aqui")
          # golem::print_dev(input$epsg_orig)
          
          
          # tgtvariable <- variables$tgtvariable()
          
         if (!is.null(input$epsg_orig)) {
           message <- paste0(
              "Converting coordinates ",
              "from epsg:",
              input$epsg_orig,
              " to epsg:",
              input$epsg_tgt
            )
          } else {
            message <- paste0(
              "Converting coordinates to epsg:",
              input$epsg_tgt
            )
          }
          
          if (input$epsg_orig == input$epsg_tgt) {
            message <- paste0(
              "Converting coordinates to epsg:",
              input$epsg_tgt
            )
          }
          shiny::showNotification(
            message,
            type = 'message',
            id = ns("conv_coords")
          )
          
        }
        
      })
      
      myDataTransform <- reactive({
        req(dataset(), cancelOutput = TRUE)
        if (!inherits(dataset(), "sf")) {
          req(coords(), cancelOutput = TRUE)
          req((!(
            is.na(test_epsg_tgt()) &
              !isTRUE(test_epsg_tgt())
          )) & !is.na(test_epsg_orig()))
          
        }
        
        coords <- coords()
        dat <- dataset()
        myDat <- tryCatch({
          spatial_transformation(
            dat,
            coords = coords,
            orgn_epsg = input$epsg_orig,
            tgt_epsg = input$epsg_tgt
          )
        }, error = function(e) {NULL})
       
        if (all(sf::st_is_empty(myDat))) {
          myDat <- NULL
        }
        
        return(myDat)

      })
      
      
    })
  }

## To be copied in the UI
# mod_spatial_transformation_ui("spatial_transformation_ui_1")
    
## To be copied in the server
# mod_spatial_transformation_server("spatial_transformation_ui_1")
