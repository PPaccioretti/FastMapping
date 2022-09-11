#' home_text UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_home_text_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      id = 'content',
      h1(style = "text-align: center", 'Welcome to FastMapping'),
      img(
        id = 'logo',
        class = 'ribbon',
        height = 100,
        width = 'auto',
        alt = '',
        src = "www/hex-FastMapping.png"
      )
    ),
    
    div(
      style = "text-align: center",
      actionButton(
        ns("startApl"),
        "Start the App!",
        icon = icon("circle-play"),
        class = "btn-success",
        style = 'text-align: center; font-size:110%; margin: 5px;'
      )
    ),
    br(),
    fluidRow(
      div(
        id = 'contaccordion',
        shinyBS::bsButton(
          "actOne",
          label = "Tools",
          block = TRUE,
          class = "accordion"
        ),
        div(class = 'panel',
            fluidRow(
              column_md(
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
                h5("Example dataset:"),
                tags$ul(tags$li(
                  a("Mapping yield data in a barley field",
                    href = "https://drive.google.com/uc?export=download&id=1ZzWDd9BHeZuebq_xPpNgSv9XsRmXEOTb")
                ),
                tags$li(
                  a("Wheat raw data",
                    href = "https://drive.google.com/uc?export=download&id=1bpCkvEoC7EvmycSFQwrtnzWCJ5WBDrIL")
                ))
                
              ),
              column_md(
                width = 6,
                h3("Tools for multivariate analysis:"),
                tags$ul(
                  tags$li("Spatial Principal Components"),
                  tags$li("Fuzzy k-means on spatial principal components (KM-sPC)")
                ),
                h5("Example dataset:"),
                tags$ul(tags$li(
                  a("Zoning a field from yield and soil properties",
                    href = "https://drive.google.com/uc?export=download&id=1SeJYNmzg-d26E_nydzKtcImshEYZB6UO"),
                  "."
                ))
              )
            ))
      ),
      
      
      # ),
      
      # fluidRow(
      div(
        id = 'contaccordion',
        shinyBS::bsButton(
          "actOne",
          label = "News",
          block = TRUE,
          class = "accordion"
        ),
        div(
          class = 'panel',
          p(
            'We are changing the User Interface and some other functionalities.',
            'If you detect some bug, please let us know!'
          ),
          p(
            'FastMapping can read vector data! (.gpkg, .shp, and others files format)'
          ),
          p(
            "We have a new installer (only for windows) you can download it",
            "from",
            a('github page',
              href = 'https://github.com/PPaccioretti/FastMapping/releases',
              target = "_blank")
          )
        )
      ),
      div(
        id = 'contaccordion',
        shinyBS::bsButton(
          "actOne",
          label = "Contact",
          block = TRUE,
          class = "accordion"
        ),
        div(
          class = 'panel',
          p(
            'If you have any question please write to fastmapping@agro.unc.edu.ar,',
            'brief tutorial is available in ',
            a("this link",
              href = "https://drive.google.com/open?id=1r2-tx35NGLzIjL0CLNR6E783ZRDsWQmf",
              target = "_blank")
          ),
          p(
            'You can create an issue or a bug report at',
            a('github',
              href = ' https://github.com/PPaccioretti/FastMapping/issues',
              target = "_blank")
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
    
    observeEvent(input$startApl, {
      shinyjs::onclick(
        "showtxt", 
        shinyjs::runjs("gtag('event', 'StartApp', '1');"))
      
    })
    reactive(input$startApl)
  })
}
    
## To be copied in the UI
# mod_home_text_ui("home_text_ui_1")
    
## To be copied in the server
# mod_home_text_server("home_text_ui_1")
