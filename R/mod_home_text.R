#' home_text UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_home_text_ui <- function(id){
  ns <- NS(id)
  tagList(
      h1('Welcome to FastMapping'),
      p('We are changing the User Interface and some other functionalities.',
        'If you detect some bug, please let us know!'),
      p('Now FastMapping can read spatial data!'),
      p(
        'If you have any question please write to fastmapping@agro.unc.edu.ar,',
        'brief tutorial is available in ',
        a("this link", href = "https://drive.google.com/open?id=1r2-tx35NGLzIjL0CLNR6E783ZRDsWQmf",  target = "_blank"),
        "."
      ),
      div(style = "text-align: center;",
          actionButton(
            ns("startApl"), 
            "Start!", 
            icon = icon("play-circle"),
            class = "btn-success",
            style = 'text-align: center; font-size:110%;'
          )),
      fluidRow(
        column(
          width = 6,
          h3("Tools for univariate analysis:"),
          tags$ul(
            tags$li("Depuration",
                    tags$ul(
                      tags$li("Global outliers"),
                      tags$li("Spatial outliers"),
                      tags$li("Border effects")
                    )),
            tags$li("Spatial interpolation",
                    tags$ul(
                      tags$li("Variogram fitting"),
                      tags$li("Kriging prediction")
                    )),
            tags$li("Classification",
                    tags$ul(tags$li(
                      "Fuzzy k-means cluster"
                    )))
            
          ),
          h4("Example dataset:"),
          tags$ul(tags$li(
            a("Mapping yield data in a barley field",
              href = "https://drive.google.com/uc?export=download&id=1ZzWDd9BHeZuebq_xPpNgSv9XsRmXEOTb")
          ),
          tags$li(
            a("Wheat raw data",
              href = "https://drive.google.com/uc?export=download&id=1bpCkvEoC7EvmycSFQwrtnzWCJ5WBDrIL")
          ))
          
        ),
        column(
          width = 6,
          h3("Tools for multivariate analysis:"),
          tags$ul(
            tags$li("Spatial Principal Components"),
            tags$li("Fuzzy k-means on spatial principal components (KM-sPC)")
          ),
          h4("Example dataset:"),
          tags$ul(tags$li(
            a("Zoning a field from yield and soil properties",
              href = "https://drive.google.com/uc?export=download&id=1SeJYNmzg-d26E_nydzKtcImshEYZB6UO"),
            "."
          )
          )
        )
      )
  )
}
    
#' home_text Server Functions
#'
#' @noRd 
mod_home_text_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    reactive(input$startApl)
  })
}
    
## To be copied in the UI
# mod_home_text_ui("home_text_ui_1")
    
## To be copied in the server
# mod_home_text_server("home_text_ui_1")
