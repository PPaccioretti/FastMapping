#' cluster_parameters UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cluster_parameters_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
    # column(
    #   width = 12 / 3,
    #   h3("Variable selection process"),
    #   p(
    #     "A variable selection process is recommended to improve validation ",
    #     "of the delineated zones."
    #   ),
    #   checkboxInput(ns("makeSelectProces"), 
    #                 "Make variable selection process", 
    #                 value = FALSE),
    #     selectInput(
    #       ns("variable_selection_process"),
    #       "Variable selection process",
    #       choices = names(data())[!names(data()) %in% c(input$xmapa, input$ymapa)],
    #       multiple = TRUE
    #     ),
    #     numericInput(
    #       ns("alpha_level_Corr"),
    #       "Significance level",
    #       value = 0.15,
    #       min = 0,
    #       max = 1,
    #       step = 0.05
    #     )
    # ),
    column(
      width = 12 / 3,
      h3("Fuzzy k-means parameters"),
      sliderInput(
        ns("clusters"),
        "Number of Cluster to evaluate",
        min = 2,
        max = 15,
        value = c(2, 6)
      ),
      radioButtons(
        ns("distance"),
        "Distance",
        choices = c("Euclidean" = "euclidean", "Manhattan" = "manhattan")
      ), 
      # numericInput(
      #   "iteraciones",
      #   "Iterations",
      #   value = 1000,
      #   min = 1,
      #   step = 10
      # ),
      numericInput(ns("fuzzyness"), 
                   "Fuzzy exponent", 
                   value = 1.3,
                   min = 1,
                   step = 0.05)
    ), 
    column(
          width = 12 / 3,
    div(id = ns("multivariate"),
          h3("Spatial PCA parameters"),
          checkboxInput(ns("center"), "Centered variables", value = TRUE),
          sliderInput(
            ns("varexplicada"),
            "Explained variance (%)",
            min = 1,
            max = 100,
            value = 70
          ),
          
          h4("Neighborhood network"),
          checkboxInput(ns("vecindarionulo"), 
                        "Data with null neighbor", 
                        value = FALSE),
          sliderInput(
            ns("distanceNeighbors"),
            "Distance between neighbors",
            min = 0,
            max = 1000,
            value = c(0, 35)
          ),
          shinyjs::hidden(
            div(
              id = ns("moreDistance"),
              p(
                "It seems that you want more distance than 1000 m between neighbours...",
                "which value do you want?"
              ),
              numericInput(ns("maxdistNeigh"),
                           "Max distance",
                           value = 0)
            )
          )
        )
    )
  ),
  div(style = "float: right;", 
      actionButton(ns("strtClust"), label = "Start Process!", class = "btn-warning")
  )
  )
}

#' cluster_parameters Server Functions
#'
#' @noRd 
mod_cluster_parameters_server <- function(id,
                                          tgtVariable) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observeEvent(tgtVariable(), {
      if (length(tgtVariable()) == 1) {
         shinyjs::hide("multivariate")
      } else {
        shinyjs::show("multivariate")
      }
    })
    
    observeEvent(input$distanceNeighbors, {
      if (input$distanceNeighbors[2] >= 1000) {
        shinyjs::show("moreDistance")
        updateNumericInput(session,
                           "maxdistNeigh",
                           value = input$distanceNeighbors[2])
      } else {
        shinyjs::hide("moreDistance")
        shinyjs::reset("moreDistance")
      }

    }, ignoreInit = TRUE)
    
    observeEvent(input$maxdistNeigh, {
      updateSliderInput(session,
                        "distanceNeighbors",
                        max = max(c(1000, input$maxdistNeigh)))
    }, ignoreInit = TRUE)
    
    
    list(
      btnStart = reactive(input$strtClust),
      params = reactive({
        list(
          variables = tgtVariable(),
          number_cluster = seq(input$clusters[1],input$clusters[2], 1),
          explainedVariance = input$varexplicada,
          ldist = input$distanceNeighbors[1],
          udist = input$distanceNeighbors[2],
          center = input$center,
          fuzzyness = input$fuzzyness,
          distance = input$distance,
          zeroPolicy = input$vecindarionulo
        )
      })
    )
    
    
  })
}    
## To be copied in the UI
# mod_cluster_parameters_ui("cluster_parameters_ui_1")
    
## To be copied in the server
# mod_cluster_parameters_server("cluster_parameters_ui_1")
