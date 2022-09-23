#' kriging_parameters UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
mod_kriging_parameters_ui <- function(id,
                                      possibleModels = modelsVariogram()) {
  ns <- NS(id)
  tagList(
    fluidRow(
    column(
      width = 3,
      h4("Select spatial model(s) to fit"),
      checkboxInput(ns('automatic'), 'Automatic', value = T),
      checkboxGroupInput(ns("ModelosA"), label = NULL, choices = possibleModels),
      checkboxInput(ns('cressie'), 'Robust variogram estimate', value =
                      FALSE)
      
    ),
    
    column(
      width = 4,
      radioButtons(
        ns("tKriging"),
        h4("Select kriging method for spatial interpolation"),
        choices = list(
          "Ordinary" = 1,
          "Universal Kriging" = 2
          # "Universal Kriging (First Order)" = 2#,
          # "Universal Kriging (Second Order)" = 3
        ),
        inline = F
      ),
      h4("Kriging options:"),
      helpText(
        "Local neighbourhood selections based on distance as radius (Max.Dist), number of data points (Max, Min), of nearest site of the target point"
      ),
      fluidRow(
        column(
          width = 4,
          numericInput(
            inputId = ns("nmin"),
            label =
              "Min. n",
            value =  7,
            width = "80%"
          )
        ),
        
        column(
          width = 4,
          numericInput(
            inputId = ns("nmax"),
            label = "Max. n",
            value = 25,
            width = "80%"
          )
        ),
        
        
        column(
          width = 4,
          numericInput(
            inputId = ns("distmax"),
            label =
              "Max. Dist.",
            value =  NA,
            width = "80%"
          )
        )
      ),
      fluidRow(
        column(
          width = 4,
          numericInput(
            inputId = ns("block"),
            label =
              "Block",
            min = 0,
            value = 0,
            width = "80%"
          )
        ),
        column(width = 4,
               numericInput(
                 ns("dimGrilla"),
                 "Cellsize",
                 value = 10,
                 min = 0.5
               ))
      )
    ),
    column(
      width = 4,
      h4("Output graphical options"),
      
      h5("Key Scale of predicted values"),
      fluidRow(
        column(
          width = 2,
          numericInput(
            inputId = ns("min"),
            label = "Min.",
            value = NULL,
            width = "100%"
          )
        ),
        column(
          width = 2,
          numericInput(
            inputId = ns("max"),
            label = "Max.",
            value = NULL,
            width = "100%"
          )
        )
      ),
      h5("Key Scale for prediction variance"),
      fluidRow(
        column(width = 2,
               numericInput(
                 ns("min_var"),
                 "Min.",
                 min = 0,
                 NULL,
                 width = "100%"
               )),
        column(width = 2,
               numericInput(ns("max_var"), "Max.", NULL, width = "100%"))
      )
    )
  ),
  div(style = "float: right;", 
      actionButton(ns("strtKrig"), label = "Start interpolation!", class = "btn-warning")
  )
  )
}

#' kriging_parameters Server Functions
#'
#' @noRd 
mod_kriging_parameters_server <-
  function(id, tgtVariable, possibleModels = modelsVariogram()) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns
      # uiOutput("ModelosA")
      observeEvent(input$automatic, {
        if (input$automatic) {
          updateCheckboxGroupInput(
            inputId = "ModelosA",
            label = NULL,
            choices = possibleModels,
            selected = c("Exp", "Sph", "Gau"),
            session = session
          )
        } else {
          updateCheckboxGroupInput(
            inputId = "ModelosA",
            label = NULL,
            choices = possibleModels,
            selected = character(0),
            session = session
          )
        }
        
      })
      
      max_dist <- reactive({
        ifelse(is.na(as.numeric(input$distmax)),
               Inf,
               as.numeric(input$distmax))
      })
      
      
      myForm <- reactive({
        req(tgtVariable())
        req(length(tgtVariable()) == 1)

        MiZ <- tgtVariable()
        MiX <- "x"
        MiY <- "y"
        
        if (input$tKriging == 1) {
          return(stats::as.formula(paste0(MiZ, "~1")))
        }
        if (input$tKriging == 2) {
          return(stats::as.formula(paste0(MiZ, "~", MiX, "+", MiY)))
        }
      if (input$tKriging == 3) {
        return(stats::as.formula(
          paste0(
            MiZ, "~", MiX, "+", MiY,
            "+I(", MiX, "^2)",
            "+I(", MiY, "^2)",
            "+I(", MiX, "*", MiY, ")"
          )
        )
        )
      }
      
    })
      
      
      list(
        'btnStart' = reactive(input$strtKrig),
        'kriging_param' = reactive({
        req(tgtVariable())
        req(length(tgtVariable()) == 1)
        list(
          selectedModels = input$ModelosA,
          cressie = input$cressie,
          input$tKriging,
          nmin = input$nmin,
          nmax = input$nmax,
          block = input$block,
          cellsize = input$dimGrilla,
          formula = myForm(),
          max_dist = max_dist(),
          myTgtVar = tgtVariable()
        )
      }),
      'kriging_plot' =
        reactive({
          list(input$min,
               input$max,
               input$min_var,
               input$max_var)
        }))
      
    })
}
    
## To be copied in the UI
# mod_kriging_parameters_ui("kriging_parameters_ui_1")
    
## To be copied in the server
# mod_kriging_parameters_server("kriging_parameters_ui_1")
