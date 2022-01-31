#' concave_hull UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_concave_hull_ui <- function(id, 
                                lblconvty = "Concavity", 
                                lblthresh = "Segment length threshold") {
  ns <- NS(id)
  tagList(
    column(
      width = 6,
      numericInput(
        ns("concavity"),
        lblconvty,
        min = 0,
        max = 10,
        value = 2,
        step = 0.2
      )
    ),
    column(
      width = 6,
      numericInput(
        ns("length_threshold"),
        lblthresh,
        min = 0,
        max = NA,
        value = 0,
        step = 0.5
      )
    )
  )
}
    
#' concave_hull Server Functions
#'
#' @noRd 
mod_concave_hull_server <- function(id, dataset){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    reactive({
      req(dataset())
      req(inherits(dataset(), "sf"))
      req(input$concavity)
      req(input$length_threshold)
      
    try({
          
    concaveman::concaveman(
      dataset(),
      concavity = input$concavity,
      length_threshold = input$length_threshold
    )
    }, silent = TRUE)

    })
    
  })
}
    
## To be copied in the UI
# mod_concave_hull_ui("concave_hull_ui_1")
    
## To be copied in the server
# mod_concave_hull_server("concave_hull_ui_1")
