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
mod_zoneCompare_process_server <- function(id,
                                           zoneCompare_param){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    reactive({
      req(zoneCompare_param())
      
      print("ZONE COMPARE PROCESS\n\n")
      zoneCompare_param <- zoneCompare_param()
      req(zoneCompare_param$zonesCol)
      req(zoneCompare_param$variable)
      
      print(zoneCompare_param)
      zc <- 
      paar::compare_zone(
        data = zoneCompare_param$data,
        variable = zoneCompare_param$variable,
        zonesCol = zoneCompare_param$zonesCol,
        alpha = zoneCompare_param$alpha
      )
      
      print(zc)
      zc
    })
    
    
  })
}
    
## To be copied in the UI
# mod_zoneCompare_process_ui("zoneCompare_process_ui_1")
    
## To be copied in the server
# mod_zoneCompare_process_server("zoneCompare_process_ui_1")
