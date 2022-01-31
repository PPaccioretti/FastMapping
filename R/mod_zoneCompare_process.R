#' zoneCompare_process UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_zoneCompare_process_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' zoneCompare_process Server Functions
#'
#' @noRd 
mod_zoneCompare_process_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    
    paar::compare_zone()
  })
}
    
## To be copied in the UI
# mod_zoneCompare_process_ui("zoneCompare_process_ui_1")
    
## To be copied in the server
# mod_zoneCompare_process_server("zoneCompare_process_ui_1")
