#' zoneCompare_parameters UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_zoneCompare_parameters_ui <- function(id,
                                          lblUpload = "Upload file with new variable",
                                          lblfile = h4("New file:"),
                                          lbltgt = 'Variable to compare',
                                          multipleTgt = TRUE) {
  ns <- NS(id)
  tagList(
    checkboxInput(inputId = ns("hasfile"),
                  label = lblUpload),
    shinyjs::hidden(div(
      id = ns("read_boundary_content"),
      mod_read_boundary_fromfile_ui(ns("readnewfile"),
                                    lblfile = lblfile)
    )),
    shinyjs::hidden(
      shiny::selectInput(
        ns('targetVariable'),
        lbltgt,
        choices = "",
        multiple = multipleTgt,
        selected = NULL
      )
    ),
    shinyjs::hidden(div(
      id = ns("param_content"),
      numericInput(
        ns("alpha"),
        label = "Significance Level",
        min = 0,
        max = 1,
        value = 0.05,
        step = 0.05
      ),
      shiny::selectInput(
        ns('clusterToTest'),
        lbltgt,
        choices = "",
        multiple = multipleTgt,
        selected = NULL
      )
    )
    )
  )
}

#' zoneCompare_parameters Server Functions
#'
#' @noRd 
mod_zoneCompare_parameters_server <- function(id, 
                                              dataset,
                                              filter = is.numeric){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # observeEvent(dataset(), {
    #   req(dataset())
    #   shinyjs::show("targetVariable")
    # })
    
    
    observeEvent(input$hasfile, {
      req(dataset())
      if (input$hasfile) {
        shinyjs::show("read_boundary_content")
        # 
      } else {
        shinyjs::hide("read_boundary_content")
        shinyjs::show('targetVariable')
      }
      
    })
    observeEvent(dataset(), {
      req(dataset())
      possibleTargetVariables <- find_vars(dataset(), filter)
      
      shiny::updateSelectInput(
        'clusterToTest',
        choices = possibleTargetVariables,
        selected = NULL,
        session = session
      )
    })
    observeEvent(myData(), {
      req(myData())
      if (input$hasfile) { 
        shinyjs::show('targetVariable')
        
        
        }
    }, ignoreInit = TRUE)
    
    
    datasetWithVars <- reactive({
      dataset()
      if (input$hasfile) {
        req(input$targetVariable)
        myData()
      }
    })
    
    myData <- mod_read_boundary_fromfile_server("readnewfile")
    
    var_names <- reactive({
      req(datasetWithVars())
      req(find_vars(datasetWithVars(), filter))
      dataset <- datasetWithVars()
      if (inherits(dataset, "sf")) {
        dataset <- sf::st_drop_geometry(dataset)
      }
      find_vars(dataset, filter)
    })
    
    observeEvent(datasetWithVars(), {
      possibleTargetVariables <- var_names()
      
      shiny::updateSelectInput(
        'targetVariable',
        choices = possibleTargetVariables,
        selected = NULL,
        session = session
      )
      shinyjs::enable("targetVariable")
    })
    
    
    list(
      newData = reactive(input$hasfile),
      data = dataset,
      variable = reactive({
        input$targetVariable
        if (input$hasfile) {
          req(input$targetVariable)
          myData()[, input$targetVariable]
        }
      }),
      zonesCol = reactive({
        input$clusterToTest
      }),
      alpha = input$alpha
    )
    
  })
}
    
## To be copied in the UI
# mod_zoneCompare_parameters_ui("zoneCompare_parameters_ui_1")
    
## To be copied in the server
# mod_zoneCompare_parameters_server("zoneCompare_parameters_ui_1")
