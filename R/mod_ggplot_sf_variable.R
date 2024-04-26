#' mod_ggplot_sf_variable UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ggplot_sf_variable_ui <- function(id,
                                          lbltgt = 'Select fill variable') {
  ns <- NS(id)
  tagList(bslib::card(
    full_screen = TRUE,
    bslib::card_header("Cluster plot"),
    bslib::layout_sidebar(
      border = FALSE,
      fillable = FALSE,
      sidebar = bslib::sidebar(
        shiny::selectInput(
          ns('fill_color'),
          label = lbltgt,
          choices = "",
          multiple = FALSE,
          selected = NULL
        )
      ),
      bslib::card_body(plotOutput(ns("clusterPlot")))
    )
  ))
}

#' mod_ggplot_sf_variable Server Functions
#'
#' @noRd 
mod_ggplot_sf_variable_server <- function(id,
                                              dataset,
                                              indices
                                              ){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    myBestCluster <- reactive({
      req(indices())
      myClustResults <- indices()
      
      myBestCluster <- myClustResults[which.min(myClustResults[['Summary Index']]), 'Num. Cluster']
      selected_col <- paste0('Cluster_', myBestCluster)
      selected_col
    })
   
    
    
    observeEvent(dataset(), {
      req(colnames(dataset()))
      
      myColnames <-  possibleTargetVariables <- find_vars(dataset(),
                                                          function(x) !is.list(x))
      myBestCluster <- myBestCluster()
      selected <- NULL
      if (myBestCluster %in% myColnames) {
        selected <- myBestCluster
      }
      shiny::updateSelectInput(
        'fill_color',
        choices = myColnames,
        selected = selected,
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
# mod_ggplot_sf_variable_ui("mod_ggplot_sf_variable_1")
    
## To be copied in the server
# mod_ggplot_sf_variable_server("mod_ggplot_sf_variable_1")
