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

  datasetTransf <-
    mod_spatial_transformation_server("dataset_spatial_transf",
                                      myDataset,
                                      myVariables$coords,
                                      myVariables$tgtvariable)
 
  mod_visualize_spatial_data_server("mymap",
                                    datasetTransf,
                                    myVariables$tgtvariable)

  field_boundary <- mod_make_boundary_server("make_boundary",
                                             datasetTransf)
  # 
  # 
  mod_visualize_spatial_data_server("boundaryMap",
                                    datasetTransf,
                                    reactive(NULL),
                                    field_boundary)

  # myDepParams <-
  #   mod_depuration_parameters_server("depuration_param")
  # 
  # myDepResults <-
  #   mod_depuration_process_server(
  #     "depuration_process",
  #     datasetTransf,
  #     myVariables$tgtvariable,
  #     myDepParams,
  #     field_boundary
  #   )
  # krigParams <-
  #   mod_kriging_parameters_server("kriging_param",
  #                                 myVariables$tgtvariable)
  # 
  # 
  # 
  # 
  # krigingProcess <-
  #   mod_kriging_process_server(
  #     "kriging_process",
  #     myDepResults$finalDataset,
  #     krigParams$kriging_param,
  #     field_boundary
  #   )
  # 
  # mod_kriging_results_server(
  #   "kriging_results",
  #   krigingProcess$variablesForVariogramPlot,
  #   krigingProcess$kriging,
  #   krigParams$kriging_plot,
  #   krigingProcess$variogram
  # )
    
}
