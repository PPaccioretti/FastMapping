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
      selected = NULL
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
        ## shinyjs::reset("variables_param", asis = TRUE)
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
          shinyjs::reset('targetVariable')
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
          # shinyjs::reset('xDataset')
          # shinyjs::reset('yDataset')
          shinyjs::show('xDataset')
          shinyjs::show('yDataset')
          
          # Try guess lat Column ----
          posible_lat <- grep('(?i)lat.*|^Y(?i)$|^ycoord(?i)$',
                              var_names(),
                              perl = TRUE)
          if (length(posible_lat) == 1) {
            lat_selected <- var_names()[posible_lat]
            
          } else {
            lat_selected <- var_names()[1]
          }
          
          shiny::updateSelectInput(
            'yDataset',
            choices = var_names(),
            selected = lat_selected,
            session = session
            
          )
          
          # Try guess long Column ----
          posible_long <- grep('(?i)long.*|^X(?i)$|^xcoord(?i)$',
                               var_names(),
                               perl = TRUE)
          if (length(posible_lat) == 1) {
            long_selected <- var_names()[posible_long]
          } else {
            long_selected <- var_names()[2]
          }
          
          shiny::updateSelectInput(
            'xDataset',
            choices = var_names(),
            selected = long_selected,
            session = session
            
          )
          
         
          
        } else {
          shinyjs::reset('xDataset')
          shinyjs::reset('yDataset')
          shinyjs::hide('xDataset')
          shinyjs::hide('yDataset')
        }
      })
      
      observeEvent(input$xDataset != 0 |
                     input$yDataset != 0 |
                     inherits(dataset(), "sf"),
                   {
                     # !inherits(dataset(), "sf")
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
                         # shiny::showNotification(
                         #   "Select different X and Y coordinates!",
                         #   type = 'error',
                         #   id = ns("msg_dup")
                         # )
                         return(NULL)
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
