#' select_variables UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_select_variables_ui <- function(id,
                                    lblx = 'X coordinate',
                                    lbly = 'Y coordinate',
                                    lbltgt = 'Target variable',
                                    onlyCoords = FALSE,
                                    multipleTgt = TRUE) {
  stopifnot(is.logical(onlyCoords))
  ns <- NS(id)
  tagList(
    shinyjs::hidden(shiny::selectInput(
      ns('xDataset'),
      lblx,
      choices = "",
      selected = NULL
    )),
    
    shinyjs::hidden(shiny::selectInput(
      ns('yDataset'),
      lbly,
      choices = "",
      selected = ""
    )),
    if (!onlyCoords) {
      shinyjs::hidden(shiny::selectInput(
        ns('targetVariable'),
        lbltgt,
        choices = "",
        multiple = multipleTgt,
        selected = NULL
      ))
    }
    
  )
}

#' select_variables Server Functions
#'
#' @noRd
mod_select_variables_server <-
  function(id,
           dataset,
           filter = is.numeric,
           onlyCoords = FALSE) {
    stopifnot(is.logical(onlyCoords))
    # req(dataset, cancelOutput = TRUE)
    stopifnot(is.reactive(dataset))
    
    moduleServer(id, function(input, output, session) {
      ns <- session$ns
      
      
      var_names <- reactive({
        req(dataset())
        req(find_vars(dataset(), filter))
        find_vars(dataset(), filter)
      })
      
      
      shiny::observeEvent(dataset(), {
        # ### INITIAL STATE
        # ## reset if dataset changed
        # shinyjs::hide("xDataset")
        # shinyjs::hide("yDataset")
        # shinyjs::hide("targetVariable")
        # shiny::updateSelectInput(
        #   'xDataset',
        #   choices = NULL,
        #   selected = NULL,
        #   session = session
        #   
        # )
        # 
        # shiny::updateSelectInput(
        #   'yDataset',
        #   choices = NULL,
        #   selected = NULL,
        #   session = session
        #   
        # )
        # 
        # shiny::updateSelectInput(
        #   'targetVariable',
        #   choices = NULL,
        #   selected = NULL,
        #   session = session
        # )
        # ### END INITIAL STATE
        
        
        if (!onlyCoords) {
          shinyjs::show('targetVariable')
          possibleTargetVariables <-
            var_names()[!(var_names() %in% c(input$xDataset, input$yDataset))]
          
          shiny::updateSelectInput(
            'targetVariable',
            choices = possibleTargetVariables,
            selected = NULL,
            session = session
          )
          
        }
        if (!inherits(dataset(), "sf")) {
          shinyjs::show('xDataset')
          shinyjs::show('yDataset')
          
          shiny::updateSelectInput(
            'xDataset',
            choices = var_names(),
            selected = var_names()[1],
            session = session
            
          )
          
          shiny::updateSelectInput(
            'yDataset',
            choices = var_names(),
            selected = var_names()[2],
            session = session
            
          )
          
        }
      })
      
      observeEvent(input$xDataset != 0 |
                     input$yDataset != 0 |
                     inherits(dataset(), "sf"),
                   {
                     !inherits(dataset(), "sf")
                     # is/are null x and/or y
                     # and is not sf
                     #or x and y are the same and
                     # is NOT sf
                     if (((is.null(input$xDataset) ||
                           is.null(input$yDataset)) &
                          !inherits(dataset(), "sf")) ||
                         (isTRUE(input$yDataset == input$xDataset) &
                          !inherits(dataset(), "sf"))) {
                       shinyjs::enable("targetVariable")
                       if (!onlyCoords) {
                         shinyjs::disable("targetVariable")
                       }
                       if (is.null(input$xDataset) ||
                           is.null(input$yDataset)) {
                         myMissing <- c()
                         if (is.null(input$xDataset)) {
                           myMissing <- c(myMissing, "X coordinate")
                         }
                         
                         if (is.null(input$yDataset)) {
                           myMissing <- c(myMissing, "Y coordinate")
                         }
                         
                         shiny::showNotification(
                           paste0("Select a value in: ",
                                  paste(myMissing, collapse = ", ")),
                           type = 'warning',
                           id = ns('msg_missing')
                         )
                       }
                       
                       if (isTRUE(input$yDataset == input$xDataset)) {
                         shiny::showNotification(
                           "Select different X and Y coordinates!",
                           type = 'warning',
                           id = ns("msg_dup")
                         )
                         
                       }
                       
                     } else {
                       if (!onlyCoords) {
                         possibleTargetVariables <-
                           var_names()[!(var_names() %in% c(input$xDataset, input$yDataset))]
                         
                         shiny::updateSelectInput(
                           'targetVariable',
                           choices = possibleTargetVariables,
                           selected = NULL,
                           session = session
                         )
                         shinyjs::enable("targetVariable")
                       }
                     }
                   }, ignoreInit = TRUE)
      
      list(
        coords = reactive({
          req(dataset())
          
          c(input$xDataset, input$yDataset)}),
        tgtvariable = reactive({
          req(dataset())
          input$targetVariable})
      )
      
    })
  }

## To be copied in the UI
# mod_select_variables_ui("select_variables_ui_1")
    
## To be copied in the server
# mod_select_variables_server("select_variables_ui_1")
