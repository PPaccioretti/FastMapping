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
    
    clusterResults <- eventReactive(button(), {
      req(dataset(), cancelOutput = TRUE)
      req(cluster_param(), cancelOutput = TRUE)

      
      id <-
        showNotification(loadingText("Zoning..."),
                         duration = NULL,
                         closeButton = FALSE)
      on.exit(removeNotification(id), add = TRUE)
      
      
      myParam <- cluster_param()
      
      req(myParam$variables,
          myParam$number_cluster,
          myParam$explainedVariance,
          myParam$fuzzyness,
          myParam$distance, 
          cancelOutput = TRUE
          )
      
      kmspc_rep <- repeatable(paar::kmspc, seed = 169)
      fuzzy_k_means_rep <- repeatable(paar::fuzzy_k_means, seed = 169)
      # browser()
      golem::print_dev("Start zonification...")
      myResult <- tryCatch({
        if (length(myParam$variables) > 1) {
          myResult <- kmspc_rep(
            data = dataset(),
            variables = myParam$variables,
            number_cluster =  myParam$number_cluster,
            explainedVariance =  myParam$explainedVariance,
            ldist = myParam$ldist,
            udist = myParam$udist,
            center = myParam$center,
            fuzzyness = myParam$fuzzyness,
            distance = myParam$distance,
            zero.policy = myParam$zeroPolicy
          )
        } 
        if (length(myParam$variables) == 1) {
          myResult <- paar::fuzzy_k_means(
            data = dataset(),
            variables = myParam$variables,
            number_cluster =  myParam$number_cluster,
            fuzzyness = myParam$fuzzyness,
            distance = myParam$distance
          )
        } 
        
        golem::print_dev("End zonification...")
        myResult
      }, error = function(e) {
        golem::print_dev("Error in zonification...")
        if (grepl('Empty neighbour sets found', e)) {
          message <- paste('Empty neighbour sets found,',
                           'try with a bigger maximum value in',
                           'Distance between neighbors')
        } else {
          message <- e
        }
        shiny::showNotification(as.character(message),
                                duration = 15,
                                closeButton = FALSE,
                                type = 'error',
                                session = session,
                                id = 'errorKmsPC')
        NULL
       
      })
      myResult
    })
    
    
    dataPlusCluster <- reactive({
      req(clusterResults())
      req(dataset())
      clusterResults <- clusterResults()
      clusterResults <- clusterResults$cluster
      
      # req(nrow(dataset()) >= nrow(clusterResults))
      
      if (nrow(dataset()) > nrow(clusterResults)) {
        myNArows <- which(apply(dataset(), 1, function(x) {
          any(is.na(x))
        }))
        
        clusterResults <- insertRow(clusterResults, NA, myNArows)
      }

      tryCatch(cbind(dataset(), clusterResults),
               error = function(e) {
                 NULL
               })
    })
    
    variables <- reactive({
      req(clusterResults())
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
