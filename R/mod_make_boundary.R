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
    
    # observeEvent(myHull(), {
    #   req(MyHull())
    #   shinyjs::show("btndndboundary")
    # })

    # output$btndndboundary <- downloadHandler(
    #   
    #   filename = function() {
    #     paste('Boundary-',
    #           format(Sys.time(), "%Y_%m_%d-%H_%M"),
    #           '.gpkg',
    #           sep = '')
    #   },
    #   content = function(con) {
    #     borders_data <- getBorders()
    #     sf::write_sf(borders_data, con)
    #   }
    # )
    
    myData <- mod_read_boundary_fromfile_server("readboundary")
    
    
    myHull <- mod_concave_hull_server("makeboundary", dataset)
    
    observeEvent(myData(), {
      req(myData())
      
      if (!has_sf_polygon(myData())) {
        shinyjs::show("concave_hull_content")
        dataset <- myData
      } else {
        myHull <- myData
      }
    })
    
    reactive({
      req(myHull())
      try(myHull())
    })
  })
}

## To be copied in the UI
# mod_make_boundary_ui("make_boundary_ui_1")

## To be copied in the server
# mod_make_boundary_server("make_boundary_ui_1")
