#' depuration_results UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom magrittr %>%
mod_depuration_results_ui <- function(id) {
  ns <- NS(id)
  tagList(div(id = ns("noDepurated"),
              p("No depuration process was made.")),
          shinyjs::hidden(div(
            id = ns("yesDepurated"),
            shinycssloaders::withSpinner(
              tagList(
                splitLayout(
                  div(style = "width: 400px; margin: auto;",
                      DT::dataTableOutput(ns("summaryResults"))),
                  shinyjs::hidden(div(
                    style = "width: 90%; margin: auto;",
                    id = ns("download_btns"),
                    tagList(
                      downloadButton(ns("downloadDepurated_dep"),
                                     "Download depurated data"),
                      br(),
                      br(),
                      downloadButton(
                        ns("downloadDepurated_cond"),
                        "Download data with finally condition"
                      )
                    )
                  ))
                ),
                shinyjs::hidden(
                  selectInput(
                    ns('colorplot'),
                    "Color by",
                    choices = NULL,
                    selected = NULL,
                    multiple = FALSE
                  )
                ),
                
                plotly::plotlyOutput(ns("DepuratedPlot"),
                                     width = "90%",
                                     height = "500px")
              )
            )
          )))
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
        # shinyjs::show("downloadDepurated_cond")
        shinyjs::hide("noDepurated")
        # shinyjs::show("yesDepurated")
      } else {
        shinyjs::hide("download_btns")
        shinyjs::hide("colorplot")
        
        shinyjs::show("noDepurated")
        shinyjs::hide("yesDepurated")
      }
    })
    
    observeEvent(dataset_withCond(), {
      req(dataset_withCond())
      req(summaryres())
      shinyjs::show("yesDepurated")
      # shinyjs::show("download_btns")

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
      myTable <- summaryres()
      myTable[,3] <- round(myTable[,3], 3)
      myTable
    }, rownames = FALSE,
    options = list(
      autoWidth = TRUE,
      dom = 't',
      ordering = FALSE,
      paging = FALSE,
      searching = FALSE,
      select = FALSE,
      columnDefs = list(list(className = 'dt-left', 
                             targets = 2)),
      columns = list(
        list(title = 'Point Condition'),
        list(title = 'n'),
        list(title = '%')
      )
    ))
    
    makePlot <- reactive({
      
       on.exit({
        shinyjs::show("download_btns", 
                      anim = TRUE,
                      animType = "fade")
         
         shinyjs::show("colorplot", 
                       anim = TRUE,
                       animType = "fade")
      }, add = TRUE)
      
      myDataset <- dataset_withCond()
      if (all(is.na(myDataset[[input$colorplot]]))) {
        myDataset[input$colorplot] <- "No Outlier"
      }
      
      p <- ggplot2::ggplot(myDataset, 
                           ggplot2::aes(
                             color = .data[[input$colorplot]], 
                             text = .data[[input$colorplot]])) +
        ggplot2::geom_sf()
      
      plotly::ggplotly(p)
    })
    
    output$DepuratedPlot <- plotly::renderPlotly({
      # build graph with ggplot syntax

     
      makePlot() %>%
        plotly::layout(autosize = TRUE)
      
    })
    
    
    
    output$downloadDepurated_cond <- downloadHandler(
      filename = function() {
        paste('DepurationProcessDataCondition-',
              Sys.Date(),
              '.gpkg',
              sep = '')
      },
      content = function(file) {
        req(dataset_withCond())
        sf::write_sf(dataset_withCond(),
                     file)
      }
    )
    
    output$downloadDepurated_dep <- downloadHandler(
      filename = function() {
        paste('DepurationProcessDataDepurated-',
              Sys.Date(),
              '.gpkg',
              sep = '')
      },
      content = function(file) {
        req(depurated())
        sf::write_sf(depurated(),
                     file)
      }
    )
    
    
  })
}

## To be copied in the UI
# mod_depuration_results_ui("depuration_results_ui_1")

## To be copied in the server
# mod_depuration_results_server("depuration_results_ui_1")
