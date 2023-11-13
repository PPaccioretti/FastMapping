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
  tagList(div(id = ns("noInterpolated"),
              p("No interpolation process was made.")),
          div(
            id = ns("yesInterpolated"),
            shinycssloaders::withSpinner(tagList(
              fluidRow(
                column(
                  12 / 3,
                  # fluidRow(
                  h4("Variogram Plot"),
                  shinycssloaders::withSpinner(plotOutput(ns("VariogramPlot"))),
                  btn_dwnd_centered(ns("download_variogram_plot"), "Download Plot")
                  # )
                ),
                
                column(
                  12 / 3,
                  h4("Predicted values"),
                  shinycssloaders::withSpinner(plotOutput(ns("KrigingPlot"))),
                  btn_dwnd_centered(ns("download_predicted_plot"), "Download Predicted Plot")
                  
                  # downloadButton(ns("download_pred_tiff"), "Download Tif")
                ),
                column(
                  12 / 3,
                  h4("Predicted variance values"),
                  shinycssloaders::withSpinner(plotOutput(ns("VarKrigingPlot"))),
                  btn_dwnd_centered(ns("download_variance_plot"), "Download Variance Plot")
                ),
                fluidRow(column(
                  12,
                  br(),
                  btn_dwnd_centered(ns("download_pred_tiff"),
                                    "Download Tif",
                                    style = 'text-align: center; font-size:100%;'),
                  br(),
                  btn_dwnd_centered(ns("download_pred_gpkg"),
                                    "Download vector data",
                                    style = 'text-align: center; font-size:100%;')
                ))
              )
            ))
          ))
}

#' kriging_results Server Functions
#'
#' @noRd 
mod_kriging_results_server <- function(id,
                                       variablesForVariogramPlot,
                                       kriging,
                                       kriging_plot,
                                       variogram){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    myEvent <- bindEvent( reactive({
      variablesForVariogramPlot()
      kriging()
      kriging_plot()
      variogram()
      }), {
      myRes <- try(kriging(), silent = T)
      if (inherits(myRes, "try-error")) {
        shinyjs::show("noInterpolated")
        shinyjs::hide("yesInterpolated")
      } else {
        shinyjs::hide("noInterpolated")
      }
    }, ignoreNULL = FALSE)
    
    
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
    
    krigingPlot <- reactive({
      myEvent()
      req(kriging())
      req(kriging_plot())
      
      krigin_plot <- kriging_plot()
      
      zmin <- krigin_plot$min
      zmax <- krigin_plot$max
      
      # if (is.na(zmin)) {
      zmin <- min(kriging()$var1.pred, na.rm = T)
      # }
      # if (is.na(zmax)) {
      zmax <- max(kriging()$var1.pred, na.rm = T)
      # }
      krigingPlot <- 
      ggplot2::ggplot() + 
        stars::geom_stars(data = kriging(), 
                          ggplot2::aes(fill = var1.pred, x = x, y = y)) +
        ggplot2::scale_fill_gradientn(colours = grDevices::terrain.colors(20),
                                      na.value = "transparent") +
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
      req(kriging_plot())
      
      krigin_plot <- kriging_plot()
      
      # zmin_var <- krigin_plot$min_var
      # zmax_var <- krigin_plot$max_var
      # # 
      # # outfile <- tempfile(fileext = '.png')
      # # png(outfile, width = 900, height = 900)
      # # 
      # print(zmin_var)
      # # if (is.na(zmin_var)) {
      #   zmin_var <- min(kriging()$var1.var, na.rm = T)
      # # }
      # # if (is.na(zmax_var)) {
      #   zmax_var <- max(kriging()$var1.var, na.rm = T)
      # }
      ggplot2::ggplot() + 
        stars::geom_stars(data = kriging(), 
                          ggplot2::aes(fill = var1.var, x = x, y = y)) +
        ggplot2::scale_fill_gradientn(colours = grDevices::cm.colors(20),
                                      na.value = "transparent") +
        ggplot2::theme(legend.position = "bottom") +
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
        paste('Map-', Sys.Date(), '.tif', sep = '')
      },
      content = function(con) {
        Predicted_Tiff <- raster_Pred()
        stars::write_stars(merge(Predicted_Tiff), 
                           con)
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
