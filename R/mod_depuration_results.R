#' depuration_results UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_depuration_results_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(id = ns("noDepurated"),
        p("No depuration process was made.")),
    div(
      id = ns("yesDepurated"),
      DT::dataTableOutput(ns("summaryResults")),
      selectInput(
        ns('colorplot'),
        "Color by",
        choices = NULL,
        selected = NULL,
        multiple = FALSE
      ),
      
      plotly::plotlyOutput(ns("DepuratedPlot")),
      shinyjs::hidden(downloadButton(
        ns("downloadDepurated"), "Download data with finally condition"
      ))
    )
  )
}

#' depuration_results Server Functions
#'
#' @noRd 
mod_depuration_results_server <- function(id,
                                          wasDepurated,
                                          dataset_withCond,
                                          depurated,
                                          summaryres) {
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(wasDepurated(),{
      if (wasDepurated()) {
        # shinyjs::show("downloadDepurated")
        shinyjs::hide("noDepurated")
        # shinyjs::show("yesDepurated")
      } else {
        shinyjs::hide("downloadDepurated")
        shinyjs::show("noDepurated")
        shinyjs::hide("yesDepurated")
      }
    })
    
    observeEvent(dataset_withCond(), {
      req(dataset_withCond())
      shinyjs::show("yesDepurated")
      shinyjs::show("downloadDepurated")
      choices <- colnames(sf::st_drop_geometry(dataset_withCond()))
      hasCond <- agrepl("condition", choices)
      if (any(hasCond)) {
        selected <- utils::head(choices[hasCond],1)
      } else {
        selected <- choices[1]
      }
      updateSelectInput(session,
                        'colorplot',
                        choices = choices,
                        selected = selected)
      
    })
    
    output$summaryResults <- DT::renderDataTable({
      summaryres()
    }, rownames = FALSE,
    options = list(
      autoWidth = TRUE,
      dom = 't',
      ordering = FALSE,
      paging = FALSE,
      searching = FALSE,
      columnDefs = list(list(width = '200px', targets = "_all"))
    ))
    
    output$DepuratedPlot <- plotly::renderPlotly({
      # build graph with ggplot syntax
      myDataset <- dataset_withCond()
      if (all(is.na(myDataset[[input$colorplot]]))) {
        myDataset[input$colorplot] <- "No Outlier"
      }
      
      p <- ggplot2::ggplot(myDataset, 
                           ggplot2::aes(
                             color = .data[[input$colorplot]], 
                             text = .data[[input$colorplot]])) +
        ggplot2::geom_sf()
      
      plotly::ggplotly(p) %>%
        plotly::layout(autosize = TRUE)
     
    })
    

    
    output$downloadDepurated <- downloadHandler(
      filename = function() {
        paste('DepuratedData-', Sys.Date(), '.gpkg', sep = '')
      },
      content = function(file) {
        req(dataset_withCond())
        sf::write_sf(
          dataset_withCond(),
          file)
      }
    )
    
    
  })
}
    
## To be copied in the UI
# mod_depuration_results_ui("depuration_results_ui_1")
    
## To be copied in the server
# mod_depuration_results_server("depuration_results_ui_1")
