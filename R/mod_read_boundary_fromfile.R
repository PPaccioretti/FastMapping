#' read_boundary_fromfile UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_read_boundary_fromfile_ui <- function(id, 
                                          lblfile = h4("Boundary file:")) {
  ns <- NS(id)
  tagList(
    mod_upload_file_ui(ns("boundary"),
                       label = lblfile),
    mod_select_variables_ui(ns("boundary_coords")),
    mod_spatial_transformation_ui(ns("boundary_spatial_transf"))
  )
  
}
    
#' read_boundary_fromfile Server Functions
#'
#' @noRd 
mod_read_boundary_fromfile_server  <- function(id,
                                               onlyCoords = TRUE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    myBoundary <- mod_upload_file_server("boundary")

    myBoundCoords <-
      mod_select_variables_server("boundary_coords",
                                  myBoundary,
                                  onlyCoords = onlyCoords)
    isReady <- reactive({
      if (onlyCoords) {
        req(myBoundCoords$coords())
        TRUE 
      } else {
        req(myBoundCoords$tgtvariable())
        TRUE
      }
      
    })
    
    myBoundaryT <-
      mod_spatial_transformation_server("boundary_spatial_transf",
                                        myBoundary,
                                        myBoundCoords$coords,
                                        isReady)
    
    myBoundaryT
    
  })
}
    
## To be copied in the UI
# mod_read_boundary_fromfile_ui("read_boundary_fromfile_ui_1")
    
## To be copied in the server
# mod_read_boundary_fromfile_server("read_boundary_fromfile_ui_1")
