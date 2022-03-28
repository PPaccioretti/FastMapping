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
                                           zoneCompare_param,
                                           button){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    # reactive({
      eventReactive(button(), {
      req(zoneCompare_param())
      
      
      id <-
        showNotification(loadingText("Comparing zones means..."),
                         duration = NULL,
                         closeButton = FALSE)
      on.exit(removeNotification(id), add = TRUE)
      
      zoneCompare_param <- zoneCompare_param()
      req(zoneCompare_param$zonesCol)
      req(zoneCompare_param$variable)


      paar::compare_zone(
        data = zoneCompare_param$data,
        variable = zoneCompare_param$variable,
        zonesCol = zoneCompare_param$zonesCol,
        alpha = zoneCompare_param$alpha
      )

    })
    
    
  })
}
    
## To be copied in the UI
# mod_zoneCompare_process_ui("zoneCompare_process_ui_1")
    
## To be copied in the server
# mod_zoneCompare_process_server("zoneCompare_process_ui_1")
