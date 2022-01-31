#' depuration_parameters UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_depuration_parameters_ui <- function(id){
  ns <- NS(id)
  tagList(
    
      h3("Data depuration parameters"),
      
      checkboxGroupInput(
        inputId = ns("automatic_dep"),
        label = h4("Data depuration"),
        choices = c(Automatic =
                      "automatic"),
        inline = TRUE,
        selected = "Automatic"
      ),
      div(style = "float: right;",
      actionButton(
        ns("resetState"),
        "Set default Options",
        icon = icon("trash-restore")
      )
      ),
      checkboxGroupInput(
        inputId = ns("mDepuration"),
        label = h4("Methods"),
        choices = list("Glogal Outliers" =
                         "outlier",
                       "Spatial Outliers" =
                         "inlier"),
        # inline = TRUE,
        width = "20%"
      # )
    ),
      fluidRow(
        column(
          width = 4,
          conditionalPanel(
            condition = "input.mDepuration.indexOf('outlier') > -1",
            ns = ns,
            h4("Global Outliers Options:"),
            helpText(
              "Upper and lower threshold boundaries to constrain data within a range of realistic values"
            ),
            fluidRow(
              column(
                width = 4,
                numericInput(
                  inputId = ns("ylimitmin"),
                  label =
                    "Min",
                  value = 0,
                  width = "100%"
                )
              ),
              
              column(
                width = 4,
                numericInput(
                  inputId = ns("ylimitmax"),
                  label =
                    "Max",
                  value = NA,
                  width = "100%"
                )
              ),
            ),
            helpText(
              "Removes all data points which are more than N times the standard deviation from the mean value"
            ),
            bootstrapPage(
              column(
                width = 6,
                numericInput(
                  inputId = ns("sd_out"),
                  label =
                    "Standard deviation",
                  value = 3
                )
              )
            )
          )
        ),
        
        column(
          width = 4,
          conditionalPanel(
            condition = "input.mDepuration.indexOf('inlier') > -1",
            ns = ns,
            h4("Spatial Outliers Options:"),
            helpText("Search radius to identify a local neighborhood for each data point"),
            
            fluidRow(
              column(
                width = 4,
                numericInput(
                  inputId = ns("neighbor_min_dist"),
                  label = "Min. distance",
                  value = 0,
                  width = "100%"
                )
              ),
              column(
                width = 4,
                numericInput(
                  inputId = ns("neighbor_max_dist"),
                  label = "Max. distance",
                  value = 20,
                  width = "100%"
                )
              )
            ),
            fluidRow(
              checkboxInput(ns("moranPlot"),
                            "Remove spatial outlier by Moran plot criteria")
            )
          )
        ),
        
        column(
          width = 4,
          conditionalPanel(
            condition = paste0("input.mDepuration.indexOf('inlier') > -1",
                               "||",
                               "input.mDepuration.indexOf('outlier') > -1"),
            ns = ns,
            h4("Border Effects:"),
            helpText("Remove data points for a given distance from field edges"),
            fluidRow(
              column(
                width = 6,
                checkboxInput(
                  inputId = ns("remove_boundary"),
                  label = "Remove borders",
                  value = FALSE,
                  width = "100%"
                )
              ),
              column(
                width = 6,
                conditionalPanel(
                  condition = "input.remove_boundary",
                  ns = ns,
                  numericInput(
                    inputId = ns("buffer"),
                    label = "Buffer",
                    value = -20,
                    width = "100%"
                  )
                  
                )
              )
            )
          )
        )
      )
  )
}
    
#' depuration_parameters Server Functions
#'
#' @noRd 
mod_depuration_parameters_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    toInitialState <- function() {
      updateCheckboxGroupInput(
        inputId = "automatic_dep", 
        choices = c(Automatic =
                      "automatic"),
        selected = NULL,
        session = session
      )
      
      updateCheckboxGroupInput(
        inputId = "mDepuration",
        choices = list("Glogal Outliers" =
                         "outlier",
                       "Spatial Outliers" =
                         "inlier"),
        selected = NULL,
        session = session
      )
      
      updateNumericInput(
        inputId = "ylimitmin", 
        value = 0,
        session = session
      )
      
      updateNumericInput(
        inputId = "ylimitmax", 
        value = NA,
        session = session
      )
      
      updateNumericInput(
        inputId = "sd_out", 
        value = 3,
        session = session
      )
      
      updateNumericInput(
        inputId = "neighbor_min_dist", 
        value = 0,
        session = session
      )
      
      updateNumericInput(
        inputId = "neighbor_max_dist", 
        value = 20,
        session = session
      )
      
      updateCheckboxInput(
        inputId = "moranPlot", 
        value = FALSE,
        session = session
      )
      
      updateCheckboxInput(
        inputId = "remove_boundary", 
        value = FALSE,
        session = session
      )
      
      updateNumericInput(
        inputId = "buffer", 
        value = -20,
        session = session
      )
    }
    
    observeEvent(input$automatic_dep, {
      if (input$automatic_dep == 'automatic') {
        updateCheckboxGroupInput(
          inputId = "mDepuration",
          choices = list("Glogal Outliers" =
                           "outlier",
                         "Spatial Outliers" =
                           "inlier"),
          selected = c("outlier",
                       "inlier"),
          session = session)
        
        updateNumericInput(
          inputId = "ylimitmin", 
          value = 0,
          session = session
        )

        updateNumericInput(
          inputId = "ylimitmax", 
          value = NA,
          session = session
        )
        
        updateNumericInput(
          inputId = "sd_out", 
          value = 3,
          session = session
        )
        
        updateNumericInput(
          inputId = "neighbor_min_dist", 
          value = 0,
          session = session
        )
        
        updateNumericInput(
          inputId = "neighbor_max_dist", 
          value = 20,
          session = session
        )
        
        updateCheckboxInput(
          inputId = "moranPlot", 
          value = TRUE,
          session = session
        )
        
        updateCheckboxInput(
          inputId = "remove_boundary", 
          value = TRUE,
          session = session
        )

        updateNumericInput(
          inputId = "buffer", 
          value = -20,
          session = session
        )
      } else {
        
        toInitialState()
      }
      })
      
      observeEvent(input$resetState, {
        toInitialState()
        updateCheckboxGroupInput(
          inputId = "automatic_dep",
          # label = h4("Data depuration"),
          choices = c(Automatic =
                        "automatic"),
          selected = NULL,
          session = session
        )
      })
     
      
      toRemove <- reactive({
        req(input$mDepuration)
        toRm <- c(input$mDepuration)
        if (input$remove_boundary) {
          toRm <- c(toRm, "edges")
        }
        toRm
          
      })
      
      reactive({
        if (is.null(input$mDepuration)) {
          return()
        }
        list(
          toremove = toRemove(),
          buffer = input$buffer,
          ylimitmax = input$ylimitmax,
          ylimitmin = input$ylimitmin,
          sdout = input$sd_out,
          ldist = input$neighbor_min_dist,
          udist = input$neighbor_max_dist
        )
      })
    
    
  })
}
    
## To be copied in the UI
# mod_depuration_parameters_ui("depuration_parameters_ui_1")
    
## To be copied in the server
# mod_depuration_parameters_server("depuration_parameters_ui_1")
