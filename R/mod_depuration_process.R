#' depuration_process UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_depuration_process_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(id = ns("notification"))
    # dataTableOutput(ns("summaryResults"))
  )
}
    
#' depuration_process Server Functions
#'
#' @noRd 
mod_depuration_process_server <-
  function(id,
           dataset,
           targetVar,
           dep_param,
           myBoundary,
           button) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns

      depurationResults <- eventReactive(button(), {
        req(dataset())
        req(length(targetVar()) == 1)
        req(dep_param())
        req(myBoundary())
        
        id <-
          showNotification(loadingText("Cleaning data..."),
                           duration = NULL,
                           closeButton = FALSE)
        on.exit(removeNotification(id), add = TRUE)
        
        myParam <- dep_param()
        myBoundary <- myBoundary()
        dataset <- dataset()
        targetVar <- targetVar()
        tryCatch({
          paar::depurate(
            x = dataset,
            y = targetVar,
            toremove = myParam$toremove,
            buffer = myParam$buffer,
            ylimitmax = myParam$ylimitmax,
            ylimitmin = myParam$ylimitmin,
            sdout = myParam$sdout,
            ldist = myParam$ldist,
            udist = myParam$udist,
            criteria =  myParam$criteria,
            zero.policy = NULL,
            poly_border = myBoundary
          )
        }, error = function(e) {
          if (agrepl("try modifying the ldist or udist values", e)) {
            showNotification(
              paste(
                "Something went wrong while depurating.",
                "Try modifying Min or Max distance value"),
                             duration = 10,
                             closeButton = FALSE,
                             type = "error")
          }
          NULL
        })
       

      
      })
      
      
      # output$summaryResults <- renderDataTable({
      #   req(depurationResults())
      #   summary(depurationResults())
      # })
      
      summaryResults <- reactive({
        summary(depurationResults())
      })
      
      originalDtasetWithCondition <- reactive({
        req(depurationResults())
        req( dataset())
        cbind('condition' = depurationResults()$condition, 
              dataset())
      })
      
      list(
        'wasDepurated' = reactive({
          ifelse(is.null(dep_param()), FALSE, TRUE)
        }),
        'depurated' = reactive({
          req(depurationResults())
          depurationResults()$depurated
        }),
        'condition' = reactive({
          req(depurationResults())
          depurationResults()$condition
        }),
        'finalDataset' = reactive({
          if (is.null(dep_param())) {
            dataset()
          } else {
            depurationResults()$depurated
          }
        }),
        'datasetWithCondition' = originalDtasetWithCondition,
        'summaryres' = reactive(summaryResults())
      )
      
      
    })
  }
    
## To be copied in the UI
# mod_depuration_process_ui("depuration_process_ui_1")
    
## To be copied in the server
# mod_depuration_process_server("depuration_process_ui_1")
