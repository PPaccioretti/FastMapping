#' make_boundary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
mod_make_boundary_ui <- function(id,
                                 lblUpload = "Upload file with boundary",
                                 lblfile = h4("Boundary file:"),
                                 lblconvty = "Concavity",
                                 lblthresh = "Segment length threshold",
                                 lbldnd = "Download Boundary file") {
  ns <- NS(id)
  tagList(checkboxInput(
    inputId = ns("hasfile"),
    label = lblUpload
  ),
  shinyjs::hidden(div(
    id = ns("read_boundary_content"),
    mod_read_boundary_fromfile_ui(ns("readboundary"),
                                  lblfile = lblfile)
  )),
  shinyjs::hidden(div(
    id = ns("concave_hull_content"),
    mod_concave_hull_ui(
      ns("makeboundary"),
      lblconvty = lblconvty,
      lblthresh = lblthresh
    )
  )),
  shinyjs::hidden(
    downloadButton(ns("btndndboundary"),
                   label = lbldnd)
  )
  )

}

#' make_boundary Server Functions
#'
#' @noRd
mod_make_boundary_server <- function(id, dataset = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    observeEvent(dataset(), {
      req(dataset())
      shinyjs::show("concave_hull_content")
    })
    
    observeEvent(input$hasfile, {
      req(dataset())
      if (input$hasfile) {
        shinyjs::show("read_boundary_content")
        shinyjs::hide("concave_hull_content")
      } else {
        shinyjs::show("concave_hull_content")
        shinyjs::hide("read_boundary_content")
      }
      
    })
    
    observeEvent(myHull(), {
      req(getBorders())
      shinyjs::show("btndndboundary")
    })

    output$btndndboundary <- downloadHandler(
      
      filename = function() {
        paste('Boundary-',
              format(Sys.time(), "%Y_%m_%d-%H_%M"),
              '.gpkg',
              sep = '')
      },
      content = function(con) {
        req(getBorders())
        borders_data <- getBorders()
        sf::write_sf(borders_data, con)
      }
    )
    
    myData <- mod_read_boundary_fromfile_server("readboundary")

    myHull <- mod_concave_hull_server("makeboundary", 
                                      myBoundary_file()[['dataset']])
    
    myBoundary_file <- reactive({
      req(dataset())
      # req(myData())
      
     
      if (isFALSE(input$hasfile)) {
        return(list(return_my_Hull = TRUE,
                    dataset = reactive(dataset())))
      } 
      shinyjs::hide("concave_hull_content")
      if (isTRUE(input$hasfile)) {
        
        if (inherits(try(myData(), silent = TRUE), "try-error")) {
          shinyjs::hide("concave_hull_content")
          req(myData())
        } else {
          myDataToRet <- myData()
        }
      } else {
        myDataToRet <- dataset()
      }
      myPossibleBoundary <- try({
        sf::st_as_sf(
          sf::st_cast(
            sf::st_combine(myData()), 
            "POLYGON")
        )
      }, silent = TRUE)
      
      if (inherits(myPossibleBoundary, 'try-error') || 
          !has_sf_polygon(myPossibleBoundary)) {
        
        showNotification(
          'Please check EPSG codes!! Probably something is wrong!',
          id = ns('error-coordinates'),
          type = "warning",
          session = session
        )
        
        if (!is.null(myData())) {
          shinyjs::show("concave_hull_content")
        }
        
        my_dataset <- reactive(myDataToRet())
        return_my_Hull <- TRUE
      } else {
        shinyjs::hide("concave_hull_content")
        return_my_Hull <- FALSE
        my_dataset <- reactive(myPossibleBoundary)
      }
      
      list(return_my_Hull = return_my_Hull,
           dataset = my_dataset)
    })
    
    
    getBorders <-
      reactive({
        req(dataset())
        # req(myData())
        # req(is.logical(myBoundary_file()[['return_my_Hull']]))
        
        if (myBoundary_file()[["return_my_Hull"]]) {
          

          return(myHull())
        } else {
          
          #Check if boundary file has same crs than uploaded file
          myBoundary <- myBoundary_file()[['dataset']]()
          if (sf::st_crs(dataset()) != sf::st_crs(myBoundary)) {
            showNotification(
              'The crs of the boundary will be changed to match the crs of the file',
              id = ns('msg-crs'),
              type = "message",
              session = session
            )
            
            myBoundary <- sf::st_transform(myBoundary, 
                                           sf::st_crs(dataset()))
          }
          return(myBoundary)
        }

      })
    getBorders
  })
}

## To be copied in the UI
# mod_make_boundary_ui("make_boundary_ui_1")

## To be copied in the server
# mod_make_boundary_server("make_boundary_ui_1")
