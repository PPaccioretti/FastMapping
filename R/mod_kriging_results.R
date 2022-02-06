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
  tagList(fluidRow(
    column(
      12 / 3,
      h4("Variogram Plot"),
      shinycssloaders::withSpinner(plotOutput(ns("VariogramPlot")))
    ),
    column(
      12 / 3,
      h4("Predicted values"),
      shinycssloaders::withSpinner(plotOutput(ns("KrigingPlot"))),
      downloadButton(ns("download_pred_tiff"), "Download Tif")
    ),
    column(
      12 / 3,
      h4("Predicted variance values"),
      shinycssloaders::withSpinner(plotOutput(ns("varKrigingPlot")))
    )
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
    
    raster_Pred <- reactive({ 
      stars::st_as_stars(kriging())
    })
    
    output$VariogramPlot <- renderPlot({
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
          size = 3
        ) +
        ggplot2::scale_y_continuous(limits = c(0, NA)) +
        ggplot2::ggtitle("Experimental variogram and fitted variogram model")
      
      variogg
    })
    
    output$KrigingPlot <- renderPlot({
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
        ggplot2::ggplot() + 
          stars::geom_stars(data = kriging(), 
                            ggplot2::aes(fill = var1.pred, x = x, y = y)) +
          ggplot2::scale_fill_gradientn(colours = grDevices::terrain.colors(20)) +
          ggplot2::theme(legend.position = "bottom")
        
    })
    
    
    output$varKrigingPlot <- renderPlot({
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
          ggplot2::scale_fill_gradientn(colours = grDevices::cm.colors(20)) +
          ggplot2::theme(legend.position = "bottom")

    })
    
    # output$TiffPlot1 <- renderPlot({
    #   validate(need(input$file, 'Check input file!'))
    #   plot(raster_Pred(), col = terrain.colors(100))
    #   
    # }, width = 600, height = 600, res = 100)
    # 
    
    #Descarga del GeoTiff
    output$download_pred_tiff <- downloadHandler(
      filename = function() {
        paste('Map-', Sys.Date(), '.tif', sep = '')
      },
      content = function(con) {
        Predicted_Tiff <- raster_Pred()
        stars::write_stars(Predicted_Tiff, 
                           con, 
                           layer = attributes(Predicted_Tiff)$names)
      }
    )
    
    
    
  })
}
    
## To be copied in the UI
# mod_kriging_results_ui("kriging_results_ui_1")
    
## To be copied in the server
# mod_kriging_results_server("kriging_results_ui_1")
