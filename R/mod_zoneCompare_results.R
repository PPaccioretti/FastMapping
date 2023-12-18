#' zoneCompare_results UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_zoneCompare_results_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(
      ns("clustervalidationTables")
    )
  )
}
    
#' zoneZompare_results Server Functions
#'
#' @noRd 
mod_zoneCompare_results_server <- function(id,
                                           zone_process) {
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$clustervalidationTables <- renderUI({
      
      req(zone_process()$differences)
      print(zone_process())
      
      mydiff <- zone_process()$differences
      LL <- vector("list", length(mydiff))
      
      for (i in seq_len(length(mydiff))) {
        LL[[i]] <-
          fluidRow(
            column(width = 12 / 3,
                   h3(names(mydiff)[i]),
                   DT::dataTableOutput(ns(
                     paste0("dt_", i)
                   ))),
            column(
              width = 12 / 4,
              br(),
              br(),
              shinycssloaders::withSpinner(plotOutput(ns(
                paste0("plot_", i)
              ), height = "190px"))
            ),
            br(),
            br()
            
          )
        
        
      }
      return(LL)
      
    })
    
    observeEvent(zone_process()$differences, {
      req(zone_process()$differences)
      print(zone_process())
      
      mydiff <- zone_process()$differences
      print(mydiff)
      sapply(seq_len(length(mydiff)), function(i) {
        id <- paste0("dt_", i)
        output[[id]] <- DT::renderDataTable(
          DT::datatable(
            mydiff[[i]][, c(1, 2, 4)],
            options = list(
              paging = FALSE,
              searching = FALSE,
              autoWidth = TRUE,
              info = FALSE
            ),
            rownames = FALSE,
            selection = 'none'
            
          )  %>% 
            DT::formatSignif(2, 4)
        )
        
        output[[paste0("plot_", i)]] <-
          renderPlot(makePlotClusterValid(mydiff[[i]]))
      })
      
    })
    
    
    
    
    
    
  })
}
    
## To be copied in the UI
# mod_zoneCompare_results_ui("zoneCompare_results_ui_1")
    
## To be copied in the server
# mod_zoneCompare_results_server("zoneCompare_results_ui_1")
