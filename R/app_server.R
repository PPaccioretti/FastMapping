#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  session$onSessionEnded(stopApp)
  options(shiny.maxRequestSize = 100 * 1024 ^ 2)

  shinyjs::hide(selector = '#navbar li a[data-value="navdataprep"]')
  shinyjs::hide(selector = '#navbar li a[data-value="navallparam"]')
  shinyjs::hide(selector = '#navbar li a[data-value="navanalyresults"]')
  
  myStartBtn <- mod_home_text_server("start_aplication")
  
  observeEvent(myStartBtn(), {
    shinyjs::show(selector = '#navbar li a[data-value="navdataprep"]')
    updateTabsetPanel(session, "navbar", selected = "navdataprep")
  }, ignoreInit = TRUE)
  
  observeEvent(input$zoneResults, {
       updateNavlistPanel(session, "navresult", selected = "navzonecompresults")
  }, ignoreInit = FALSE)
  
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
  
  observeEvent(input$navbar, {
    req(myVariables$tgtvariable())
    tgtVarlgth <- length(myVariables$tgtvariable())

    if (input$navbar == "navallparam") {
      if (tgtVarlgth > 1) {
        updateTabsetPanel(session, "navparam", selected = "navclustparam")
      }
      if (tgtVarlgth == 1) {
        updateTabsetPanel(session, "navparam", selected = "navdepparam")
      }
    }

    if (input$navbar == "navanalyresults") {
      if (tgtVarlgth > 1) {
        updateTabsetPanel(session, "navresult", selected = "navclustresults")
      }
      if (tgtVarlgth == 1) {
        updateTabsetPanel(session, "navresult", selected = "navdepresults")
      }
    }

  }, ignoreInit = TRUE)



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
      myDepParams$params,
      field_boundary,
      myDepParams$btnStart
    )
  
  mod_depuration_results_server("depuration_results",
                                myDepResults$wasDepurated,
                                myDepResults$datasetWithCondition,
                                myDepResults$depurated,
                                myDepResults$summaryres)

  
  krigParams <-
    mod_kriging_parameters_server("kriging_param",
                                  myVariables$tgtvariable)




  kriging_process <-
    mod_kriging_process_server(
      "kriging_process",
      myDepResults$finalDataset,
      krigParams$kriging_param,
      field_boundary,
      krigParams$btnStart
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
                               cluster_param$params,
                               cluster_param$btnStart)
  
  mod_cluster_results_server(
    "cluster_results",
    clusterResults = cluster_process$cluster,
    variablesUsed = myVariables$tgtvariable,
    data_and_cluster =  cluster_process$data_and_cluster
  )
  
  # It needs results from cluster process. This could be change to use other
  # results from FastMapping. Maybe with Observer over datasets? 
  zone_param <- 
    mod_zoneCompare_parameters_server("zone_param",
                                      cluster_process$data_and_cluster
                                      # myDepResults$finalDataset
    )
  
  zone_process <-
    mod_zoneCompare_process_server("zone_precess",
                                   zone_param$zoneCompare_param,
                                   zone_param$btnStart)
  
  mod_zoneCompare_results_server(
    "zone_results",
    zone_process
  )
  
  
}
