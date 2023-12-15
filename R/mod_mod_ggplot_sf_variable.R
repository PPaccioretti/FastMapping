#' mod_ggplot_sf_variable UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_mod_ggplot_sf_variable_ui <- function(id,
                                          lbltgt = 'Select fill variable'){
  ns <- NS(id)
  tagList(
    shiny::selectInput(
      ns('fill_color'),
      lbltgt,
      choices = "",
      multiple = FALSE,
      selected = NULL
    ),
    plotOutput(ns("clusterPlot"))
  )
}
    
#' mod_ggplot_sf_variable Server Functions
#'
#' @noRd 
mod_mod_ggplot_sf_variable_server <- function(id,
                                              dataset
                                              ){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    observeEvent(dataset(), {
      req(colnames(dataset()))
      
      myColnames <-  possibleTargetVariables <- find_vars(dataset(),
                                                          function(x) !is.list(x))
      shiny::updateSelectInput(
        'fill_color',
        choices = myColnames,
        selected = NULL,
        session = session
      )
    })
    
    output$clusterPlot <- renderPlot({
      req(dataset())
      req(input$fill_color)
      
      myData <- dataset()
      myCol <- input$fill_color
      makePlotCluster(myData, myCol)
      
    })
    
  })
}
    
## To be copied in the UI
# mod_mod_ggplot_sf_variable_ui("mod_ggplot_sf_variable_1")
    
## To be copied in the server
# mod_mod_ggplot_sf_variable_server("mod_ggplot_sf_variable_1")
