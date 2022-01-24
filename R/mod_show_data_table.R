#' show_data_table UI Function
#'
#' @description A shiny Module that show data in data.table format.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
mod_show_data_table_ui <- function(id) {
  ns <- NS(id)
  dataTableOutput(ns("table"))
}

#' show_data_table Server Functions
#'
#' @noRd
mod_show_data_table_server <- function(id, dataset) {
  stopifnot(is.reactive(dataset))
  
  moduleServer(id, function(input, output, session) {
    data_print <- reactive({
      req(dataset())
      myData <- dataset()
      if (inherits(myData, "sf")) {
        myData <- print_sf_as_df(myData)$data
      }
      myData
    })
    
    output$table <- renderDataTable({
      data_print()
    },
    options = list(
      pageLength = 5,
      scrollX = TRUE,
      lengthMenu = {
        req(dataset())
        myRowsToSelect <-
          unique(round(ceiling(seq(
            10, nrow(dataset()), length.out = 5
          )), -1))
        
        list(myRowsToSelect,
             c(myRowsToSelect[-length(myRowsToSelect)], 'All'))
      }
    ))
  })
  
}

## To be copied in the UI
# mod_show_data_table_ui("show_data_table_ui_1")

## To be copied in the server
# mod_show_data_table_server("show_data_table_ui_1")
