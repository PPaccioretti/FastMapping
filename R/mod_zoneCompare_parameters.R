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
                                          lglClust = 'Cluster identifier',
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
        lglClust,
        choices = "",
        multiple = FALSE,
        selected = NULL
      )
    )
    ),
    div(style = "float: right;", 
        actionButton(ns("strtZoning"), label = "Start Zone validation!", class = "btn-warning")
  )
  )
}

#' zoneCompare_parameters Server Functions
#'
#' @noRd 
mod_zoneCompare_parameters_server <- function(id, 
                                              dataset,
                                              filterTgt = is.numeric,
                                              # Use !is.list to remove geometry column if is sf:
                                              filterCluster = function(x) {!is.list(x)}){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$hasfile == 2 | nrow(dataset()) > 2, {
      req(dataset())
      shinyjs::show('targetVariable')
      if (input$hasfile) {
        shinyjs::show("read_boundary_content")
        shinyjs::hide('targetVariable')
        # 
      } else {
        shinyjs::hide("read_boundary_content")
        
      }
    })
    
    observeEvent(input$targetVariable, {
      req(input$targetVariable)
      shinyjs::show("param_content")
    })
    
    
    observeEvent(dataset(), {
      req(dataset())
      possibleTargetVariables <- find_vars(dataset(), filterCluster)
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
    }, ignoreInit = FALSE)
    
    
    datasetWithVars <- reactive({
      # req(dataset())
      myDataVars <- dataset()
      if (input$hasfile) {
        myDataVars <- myData()
      }
      myDataVars
      
    })
    
    myData <- mod_read_boundary_fromfile_server("readnewfile")
    
    var_names <- reactive({
      req(datasetWithVars())
      req(find_vars(datasetWithVars(), filterTgt))
      
      dataset <- datasetWithVars()
      if (inherits(dataset, "sf")) {
        dataset <- sf::st_drop_geometry(dataset)
      }
      find_vars(dataset, filterTgt)
    })
    
    observeEvent(var_names(), {
      req(var_names())
      possibleTargetVariables <- var_names()
      
      shiny::updateSelectInput(
        'targetVariable',
        choices = possibleTargetVariables,
        selected = NULL,
        session = session
      )
      shinyjs::enable("targetVariable")
    })
    
    variableToUse <- reactive({
      # req(dataset())
        myVars <- input$targetVariable
        if (input$hasfile) {
          myVars <- datasetWithVars()[, input$targetVariable]
        }

        myVars
      })
    
   
      list(
        'btnStart' = reactive(input$strtZoning),
        'zoneCompare_param' = reactive({
          list(
            data = dataset(),
            variable = variableToUse(),
            zonesCol = input$clusterToTest,
            alpha = input$alpha
          )
        }),
        'has_newData' = reactive({input$hasfile})
      )
    
  })
}
    
## To be copied in the UI
# mod_zoneCompare_parameters_ui("zoneCompare_parameters_ui_1")
    
## To be copied in the server
# mod_zoneCompare_parameters_server("zoneCompare_parameters_ui_1")
