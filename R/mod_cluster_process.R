#' cluster_process UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cluster_process_ui <- function(id){
  ns <- NS(id)
  tagList(
    # DT::dataTableOutput(ns("summaryResults"))
  )
}
    
#' cluster_process Server Functions
#'
#' @noRd 
mod_cluster_process_server <- function(id,
                                       dataset,
                                       cluster_param,
                                       button) {
  moduleServer( id, function(input, output, session) {
    ns <- session$ns
        
    # output$summaryResults <- DT::renderDataTable({
    #   req(clusterResults())
    #   print(clusterResults())
    #   clusterResults()$summaryResults 
    # })
    
    clusterResults <- reactive({
      req(dataset())
      req(cluster_param())
      print("KMSPC")
      myParam <- cluster_param()

      paar::kmspc(
        dataset(),
        variables = myParam$variables,
        number_cluster =  myParam$number_cluster,
        explainedVariance =  myParam$explainedVariance,
        ldist =  myParam$ldist,
        udist =  myParam$udist,
        center =  myParam$center,
        fuzzyness = myParam$fuzzyness,
        distance =  myParam$distance,
        zero.policy =  myParam$zeroPolicy
      )
    })
    
    
    dataPlusCluster <- reactive({
      req(clusterResults())
      clusterResults <- clusterResults()
      cbind(dataset(), clusterResults)
    })
    
    variables <- reactive({
      req(cluster_param())
      
      myParam <- cluster_param()
      myParam$variables
    })
    
    list(
      cluster = clusterResults,
      data_and_cluster = dataPlusCluster,
      variablesUsed = variables
    )
  })
}
    
## To be copied in the UI
# mod_cluster_process_ui("cluster_process_ui_1")
    
## To be copied in the server
# mod_cluster_process_server("cluster_process_ui_1")
