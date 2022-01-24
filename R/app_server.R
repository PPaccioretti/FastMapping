#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  session$onSessionEnded(stopApp)
  options(shiny.maxRequestSize = 20 * 1024 ^ 2)
  
  observeEvent(input$start, {
    # shinyjs::showElement("navdataprep")
    shinyjs::show(selector = '#navbar li a[data-value="navdataprep"]')
    showTab(inputId = "navbar", target = "navdataprep")
    updateTabsetPanel(session, "navbar", selected = "navdataprep")
    
    
  })
  
  observeEvent(input$show, {
    # shinyjs::showElement("navdataprep")
    shinyjs::show(selector = '#navbar li a[data-value="navdataprep"]')
    shinyjs::show(selector = '#navbar li a[data-value="navdepparam"]')
    
  })
  
  observeEvent(input$hide, {
    # shinyjs::showElement("navdataprep")
    shinyjs::hide(selector = '#navbar li a[data-value="navdataprep"]')
    shinyjs::hide(selector = '#navbar li a[data-value="navdepparam"]')
    
  })
  
  myDataset <- mod_upload_file_server("dataset")
  
  mod_show_data_table_server("dataset_print",
                             myDataset)
  
  myVariables <-
    mod_select_variables_server("dataset_cols",
                                myDataset)
  
  myDatasetT <-
    mod_spatial_transformation_server("dataset_spatial_transf",
                                      myDataset,
                                      myVariables$coords,
                                      myVariables$tgtvariable)
  
  observeEvent(is.data.frame(myDatasetT()) | is.null(myVariables$tgtvariable()), {
    req(myVariables$tgtvariable())
    mod_visualize_spatial_data_server("mymap",
                                      myDatasetT,
                                      myVariables$tgtvariable)
    
    myBoundary <- mod_make_boundary_server("make_boundary",
                                           myDatasetT)
    
  })
  


  
  observeEvent(is.data.frame(myDatasetT()) | is.null(myBoundary()), {
    req(myDatasetT())
    req(myBoundary())
    mod_visualize_spatial_data_server("boundaryMap",
                                      myDatasetT,
                                      reactive(NULL),
                                      myBoundary)
  })
  
  
  output$myPlot <- renderPlot({
    req(myBoundary())
    req(myDatasetT())
    
    plot(myBoundary(), add = T)
  })

  output$map1 <- leaflet::renderLeaflet({
    leaflet::leaflet() %>% 
      leaflet::addTiles() %>% 
      leaflet::setView(-93.65, 42.0285, zoom = 17)
  })
  
  observeEvent(input$map1_marker_click, {
    leaflet::leafletProxy("map1", session) %>%
      leaflet::removeMarker(input$map1_marker_click$id)
  })

}
