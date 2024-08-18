#' ggplot_options UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ggplot_options_ui <- function(id, pallette_selected = 'viridis'){
  ns <- NS(id)
  tagList(
    h6("Key Scale of predicted values"),
    fluidRow(
      column(
        width = 6,
        numericInput(
          inputId = ns("min"),
          label = "Min.",
          value = NULL,
          width = "100%"
        )
      ),
      column(
        width = 6,
        numericInput(
          inputId = ns("max"),
          label = "Max.",
          value = NULL,
          width = "100%"
        )
      )
    ),
    selectInput(
      inputId = ns('pallette'),
      label = 'Pallette',
      choices = c(
        "Red-Yellow-Green" = 'ryg',
        "Cyan-Magenta" = 'cm',
        "Terrain" = 'terrain',
        "Viridis" = "viridis",
        "Magma" = "magma",
        "Inferno" = "inferno",
        "Plasma" = "plasma",
        "Cividis" = "cividis"
      ),
      selected = pallette_selected
    ), 
    checkboxGroupInput(
      inputId = ns("dir_pallette"),
      label = 'Reverse pallette',
      choices = c(Reverse = TRUE),
      inline = TRUE
    ),
    textInput(
      inputId = ns('label'),
      label = 'Label',
      value = 'Predicted values'
      # )
    )
  )
}
    
#' ggplot_options Server Functions
#'
#' @noRd 
mod_ggplot_options_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    my_pallete <- reactive({
      
      pallette <- input$pallette
      dir_pallette <- input$dir_pallette
      
      
      if (isTRUE(dir_pallette == 'TRUE')) {
        dir_pallette <- -1
      } else {
        dir_pallette <- 1
      }
      ryg <-  c('red', 'yellow', 'green')
# browser()
      my_pallette <- switch(
        pallette,
        "ryg" = if (dir_pallette == 1) {ryg} else {rev(ryg)},
        'cm' = cm.colors(10, rev = dir_pallette == -1),
        "terrain" = terrain.colors(10, rev = dir_pallette == -1),
        "viridis" = scales::viridis_pal(option = "viridis", direction = dir_pallette)(10),
        "magma" = scales::viridis_pal(option = "magma", direction = dir_pallette)(10),
        "inferno" = scales::viridis_pal(option = "inferno", direction = dir_pallette)(10),
        "plasma" = scales::viridis_pal(option = "plasma", direction = dir_pallette)(10),
        "cividis" = scales::viridis_pal(option = "cividis", direction = dir_pallette)(10)
        
      )
      my_pallette
    })
    
    reactive({
      list(
        min = input$min,
        max = input$max,
        pallette = my_pallete(),
        dir_pallette = input$dir_pallette,
        label = input$label
      )
    })
  })
}
    
## To be copied in the UI
# mod_ggplot_options_ui("ggplot_options_1")
    
## To be copied in the server
# mod_ggplot_options_server("ggplot_options_1")
