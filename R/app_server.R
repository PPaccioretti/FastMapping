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

  shinyjs::hide(selector = '#navbar li a[data-value="navdataprep"]')
  shinyjs::hide(selector = '#navbar li a[data-value="navallparam"]')
  shinyjs::hide(selector = '#navbar li a[data-value="navanalyresults"]')
  
  myStartBtn <- mod_home_text_server("start_aplication")
  
  observeEvent(myStartBtn(), {
    shinyjs::show(selector = '#navbar li a[data-value="navdataprep"]')
    # showTab(inputId = "navbar", target = "navdataprep")
    updateTabsetPanel(session, "navbar", selected = "navdataprep")
  }, ignoreInit = TRUE)
  
  observeEvent(myVariables$tgtvariable(), {
    tgtVarlgth <- length(myVariables$tgtvariable())
    
    if (tgtVarlgth >= 1) {
      shinyjs::show(selector = '#navbar li a[data-value="navdataprep"]')
      shinyjs::show(selector = '#navbar li a[data-value="navallparam"]')
      shinyjs::show(selector = '#navbar li a[data-value="navanalyresults"]')
    }
    
    if (tgtVarlgth == 1) {
      shinyjs::show(selector = '#navbar li a[data-value="navdepparam"]')
      shinyjs::show(selector = '#navbar li a[data-value="navkrigparam"]')
      shinyjs::show(selector = '#navbar li a[data-value="navclustparam"]')
      shinyjs::show(selector = '#navbar li a[data-value="navzonecompparam"]')
      
      
      bslib::nav_show("navdata", "navboundary")
      
      bslib::nav_show("navparam", "navdepparam")
      bslib::nav_show("navparam", "navkrigparam")
      
      bslib::nav_show("navresult", "navdepresults")
      bslib::nav_show("navresult", "navkrigresults")
    }
    
    if (tgtVarlgth > 1) {
      shinyjs::hide(selector = '#navbar li a[data-value="navdepparam"]')
      shinyjs::hide(selector = '#navbar li a[data-value="navkrigparam"]')
      
      bslib::nav_hide("navdata", "navboundary")
      
      bslib::nav_hide("navparam", "navdepparam")
      bslib::nav_hide("navparam", "navkrigparam")
      
      bslib::nav_hide("navresult", "navdepresults")
      bslib::nav_hide("navresult", "navkrigresults")
    }
    
    
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
  
  
  mod_visualize_spatial_data_server("boundaryMap",
                                    datasetTransf,
                                    reactive(NULL),
                                    field_boundary)

  myDepParams <-
    mod_depuration_parameters_server("depuration_param")

  myDepResults <-
    mod_depuration_process_server(
      "depuration_process",
      datasetTransf,
      myVariables$tgtvariable,
      myDepParams,
      field_boundary
    )
  
  mod_depuration_results_server("depuration_results",
                                myDepResults$wasDepurated,
                                myDepResults$datasetWithCondition,
                                myDepResults$depurated)

  
  krigParams <-
    mod_kriging_parameters_server("kriging_param",
                                  myVariables$tgtvariable)




  kriging_process <-
    mod_kriging_process_server(
      "kriging_process",
      myDepResults$finalDataset,
      krigParams$kriging_param,
      field_boundary
    )

  mod_kriging_results_server(
    "kriging_results",
    kriging_process$variablesForVariogramPlot,
    kriging_process$kriging,
    krigParams$kriging_plot,
    kriging_process$variogram
  )
  cluster_param <-
    mod_cluster_parameters_server("cluster_param",
                                  myVariables$tgtvariable)
  cluster_process <-
    mod_cluster_process_server("cluster_precess",
                               datasetTransf,
                               cluster_param,
                               reactive(input$startClustProcess))
  
  mod_cluster_results_server(
    "cluster_results",
    clusterResults = cluster_process$cluster,
    variablesUsed = myVariables$tgtvariable,
    data_and_cluster =  cluster_process$data_and_cluster
  )
  
  mod_zoneCompare_parameters_server("zone_param",
                                    myDepResults$finalDataset,
                                    )
  
  
}
