#' kriging_results UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
mod_kriging_results_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
    id = ns("noInterpolated"),
    p("No interpolation process was made.")
  ),
  shinyjs::hidden(div(
    id = ns("yesInterpolated"),
    tagList(
      bslib::layout_columns(
        col_widths = c(4, 4, 4, -4, 8),
        row_heights =  bslib::breakpoints(
          sm = c('2fr', '2fr', '2fr', '0.75fr'),
          md = c('2fr','0.80fr'),
          lg = c('2fr','0.5fr')
        ),
        {
          
          bslib::card(
            full_screen = TRUE,
            bslib::card_header(
              "Variogram Plot"
            ),
            bslib::card_body(
              shinycssloaders::withSpinner(plotOutput(ns(
                "VariogramPlot"
              )))
            ),
            bslib::card_footer(
              btn_dwnd_centered(ns("download_variogram_plot"),
                                "Download Plot")
            )
          )
          
          
        },
        {
            bslib::navset_card_pill(
              bslib::nav_panel(title = "Predicted Plot", 
                               shinycssloaders::withSpinner(plotOutput(ns(
                                 "KrigingPlot"
                               )))
              ),
              bslib::nav_spacer(),
              bslib::nav_panel(
                shiny::icon("gear"),
                fillable = FALSE,
               mod_ggplot_options_ui(ns("ggplot_options_pred"))
              ),
              bslib::nav_item(btn_dwnd_centered(ns("download_predicted_plot"),
                                                "Download Plot")),
              placement = "below"
            )
          
        },
        {
          
          bslib::navset_card_pill(
            bslib::nav_panel(title = "Predicted variance", 
                             shinycssloaders::withSpinner(plotOutput(ns(
                               "VarKrigingPlot"
                             )))
            ),
            bslib::nav_spacer(),
            bslib::nav_panel(
              shiny::icon("gear"),
              fillable = FALSE,
              mod_ggplot_options_ui(ns("ggplot_options_var"),
                                    pallette_selected = 'cm')
            ),
            bslib::nav_item(btn_dwnd_centered(ns("download_variance_plot"),
                                              "Download Plot")),
            placement = "below"
          )
        },
        {
          
          bslib::card(
            full_screen = FALSE,
            bslib::card_header(
              "Download datasets"
            ),
            bslib::card_body(
              
              bslib::layout_columns(
                col_widths = c(-2, 4, 4, -2),
                btn_dwnd_centered(ns("download_pred_tiff"),
                                  "Download Tif",
                                  style = 'text-align: center; font-size:100%;'),
                btn_dwnd_centered(ns("download_pred_gpkg"),
                                  "Download vector data",
                                  style = 'text-align: center; font-size:100%;')
              )
            )
          )
          
        }
      )
    )
  )
  )
  )
}

#' kriging_results Server Functions
#'
#' @noRd 
mod_kriging_results_server <- function(id,
                                       variablesForVariogramPlot,
                                       kriging,
                                       variogram) {
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    myEvent <- bindEvent( reactive({
      variablesForVariogramPlot()
      kriging()
      variogram()
      }), {

      myRes <- try(kriging(), silent = T)
      if (inherits(myRes, "try-error") || is.null(myRes)) {
        shinyjs::show("noInterpolated")
        shinyjs::hide("yesInterpolated")
      } else {
        shinyjs::hide("noInterpolated")

      }
    }, ignoreNULL = FALSE)
    
    
    observeEvent(is.data.frame(kriging()) || is.null(variogram()), {
      
      shinyjs::show("yesInterpolated")
      myRes <- try(kriging(), silent = T)
      if (inherits(myRes, "try-error") || is.null(myRes)) {
        shinyjs::show("noInterpolated")
        shinyjs::hide("yesInterpolated")
      } else {
        shinyjs::hide("noInterpolated")
        shinyjs::show("yesInterpolated")
      }
      
    })
    
    
    
    raster_Pred <- reactive({ 
      stars::st_as_stars(kriging())
    })
    
    
    
    variogramPlot <- reactive({
      myEvent()
      req(variablesForVariogramPlot())
      mydata <- variablesForVariogramPlot()

      variogg <- ggplot2::ggplot(data = mydata$variogramline) +
        ggplot2::geom_point(data = mydata$variogramPoint,
                            ggplot2::aes(x = dist, y = gamma),
                            size = 2) +
        ggplot2::geom_line(ggplot2::aes(x = dist, y = gamma), color = "blue", size = 1.2) +
        ggplot2::xlab("Distance") +
        ggplot2::ylab("Semi-variance") +
        ggplot2::annotate(
          "text",
          label = mydata$parametros,
          x = Inf,
          y = -Inf,
          hjust = 1,
          vjust = -0.1,
          size = 4
        ) +
        ggplot2::scale_y_continuous(limits = c(0, NA)) +
        ggplot2::ggtitle("Experimental variogram and fitted variogram model")
      
      variogg
    })
    
    output$VariogramPlot <- renderPlot({
      variogramPlot()
    })
    pred_options <- mod_ggplot_options_server("ggplot_options_pred")
    var_options <- mod_ggplot_options_server("ggplot_options_var")
    
    krigingPlot <- reactive({
      myEvent()
      req(kriging())
      
      pred_options <- pred_options()
      if (pred_options$label == 'Asd') browser()
      zmin <- pred_options$min
      zmax <- pred_options$max
      
      pallette <- pred_options$pallette
     
      label_fill <- pred_options$label
      if (is.null(label_fill)) {
        label_fill <- "Predicted values"
      }
      
      
      krigingPlot <- 
      ggplot2::ggplot() + 
        stars::geom_stars(data = kriging(), 
                          ggplot2::aes(fill = var1.pred, x = x, y = y)) +
        ggplot2::scale_fill_gradientn(colours = pallette,
                                      na.value = "transparent",
                                      limits = c(zmin, zmax)) +
        ggplot2::labs(fill = label_fill) +
        ggplot2::theme(legend.position = "bottom") +
        ggplot2::guides(fill = ggplot2::guide_colourbar(
          barwidth = 17, 
          label.position = "bottom")) +
        ggplot2::coord_equal()
      krigingPlot
      
    })
    
    
    output$KrigingPlot <- renderPlot({
      req(krigingPlot())
      krigingPlot()
        
    })
    
    #VariogramPlot download
    output$download_variogram_plot <- downloadHandler(
      filename = function() {
        paste('VariogramPlot-', Sys.Date(), '.png', sep = '')
      },
      content = function(con) {
        req(variogramPlot())
        ggplot2::ggsave(con, plot = variogramPlot(), device = 'png')
      }
    )
    #KrigingmPlot download
    output$download_predicted_plot <- downloadHandler(
      filename = function() {
        paste('KrigingPlot-predicted-', Sys.Date(), '.png', sep = '')
      },
      content = function(con) {
        req(krigingPlot())
        ggplot2::ggsave(con, 
                        plot = krigingPlot(), 
                        width = 15,
                        height = 15,
                        units = "cm",
                        device = 'png')
      }
    )
    output$download_variance_plot <- downloadHandler(
      filename = function() {
        paste('KrigingPlot-variance-', Sys.Date(), '.png', sep = '')
      },
      content = function(con) {
        req(varkrigingPlot())
        ggplot2::ggsave(con, 
                        plot = varkrigingPlot(), 
                        width = 15,
                        height = 15,
                        units = "cm",
                        device = 'png')
      }
    )
    
    
    
    varkrigingPlot <- reactive({
      
      req(kriging())
      
      var_options <- var_options()
      if (var_options$label == 'Asd') browser()
      zmin <- var_options$min
      zmax <- var_options$max
      
      pallette <- var_options$pallette
      
      label_fill <- var_options$label
      if (is.null(label_fill)) {
        label_fill <- "Variance of prediction"
      }
      
      ggplot2::ggplot() + 
        stars::geom_stars(data = kriging(), 
                          ggplot2::aes(fill = var1.var, x = x, y = y)) +
        ggplot2::scale_fill_gradientn(colours = pallette,
                                      na.value = "transparent",
                                      limits = c(zmin, zmax)) +
        ggplot2::theme(legend.position = "bottom") +
        ggplot2::labs(fill = label_fill) +
        ggplot2::guides(fill = ggplot2::guide_colourbar(
          barwidth = 17, 
          label.position = "bottom")) +
        ggplot2::coord_equal()
      
    })
    
    output$VarKrigingPlot <- renderPlot({
      req(varkrigingPlot())
      varkrigingPlot()
    })
    
    
    #Descarga del GeoTiff
    output$download_pred_tiff <- downloadHandler(
      filename = function() {
        paste('Map-', Sys.Date(), '.nc', sep = '')
      },
      content = function(con) {
        Predicted_Tiff <- raster_Pred()
        stars::write_mdim(Predicted_Tiff, 
                           con)
        # ,layer = attributes(Predicted_Tiff)$names
      }
    )
    #Descarga del archivo vectorial gpkg
    output$download_pred_gpkg <- downloadHandler(
      filename = function() {
        paste('Map-', Sys.Date(), '.gpkg', sep = '')
      },
      content = function(con) {
        Predicted_sf <- sf::st_as_sf(raster_Pred())
        sf::write_sf(Predicted_sf, 
                     con)
      }
    )
    
  })
}
    
## To be copied in the UI
# mod_kriging_results_ui("kriging_results_ui_1")
    
## To be copied in the server
# mod_kriging_results_server("kriging_results_ui_1")
