#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  # Leave this function for adding external resources
  tagList(
    golem_add_external_resources(),
    # Your application UI logic
    bslib::page_navbar(
      # navbarPage(
      title = "FastMapping",
      id = "navbar",
      theme = bslib::bs_theme(bootswatch = "united"),
      bslib::nav(
        title = "",
        icon = icon("home"),
        value = "navhome",
        mainPanel(
          style = "margin: 0 auto",
            mod_home_text_ui("start_aplication")
          )
      ),
      
      bslib::nav(
        title = "Data preparation",
        value = "navdataprep",
        bslib::navs_pill(
          id = "navdata",
          bslib::nav(
            title = "Dataset",
            value = "navdataset",
            sidebarLayout(
              sidebarPanel(
                mod_upload_file_ui("dataset", label = h4("Dataset file:")),
                mod_select_variables_ui("dataset_cols"),
                mod_spatial_transformation_ui("dataset_spatial_transf")
              ),
              mainPanel(
                mod_show_data_table_ui("dataset_print"),
                mod_visualize_spatial_data_ui("mymap")
              )
            )),
          bslib::nav(
            title = "Boundary",
            value = "navboundary",
            sidebarLayout(
              sidebarPanel(mod_make_boundary_ui("make_boundary")),
              mainPanel(
                mod_visualize_spatial_data_ui("boundaryMap")
              )
            )
          )
        )
      ),

      bslib::nav(
        title = "Parameters Specification",
        value = "navallparam",
        
        bslib::navs_pill(
          id = "navparam",
          bslib::nav(
            title = "Depuration Parameters",
            value = "navdepparam",
            mainPanel(mod_depuration_parameters_ui("depuration_param"), width = 12
                      )
          ),
          bslib::nav(
            title = "Kriging Parameters",
            value = "navkrigparam",
            mainPanel(mod_kriging_parameters_ui("kriging_param"), width = 12)
          ),
          bslib::nav(
            title = "Cluster Parameters",
            value = "navclustparam",
            mainPanel(mod_cluster_parameters_ui("cluster_param"), width = 12)
          )
          
        )
      ),
      bslib::nav(
        title = "Analysis Results",
        value = "navanalyresults",
        bslib::navs_pill( 
          id = "navresult",
          bslib::nav(
            title = "Depuration Results",
            value = "navdepresults",
            mainPanel(
              mod_depuration_process_ui("depuration_process"),
              mod_depuration_results_ui("depuration_results"),
              width = 12
              )
          ),
          bslib::nav(
            title = "Kriging Results",
            value = "navkrigresults",
            mainPanel(
              mod_kriging_process_ui("kriging_process"),
              mod_kriging_results_ui("kriging_results"), 
              width = 12
            )
          ),
          bslib::nav(
            title = "Cluster Results",
            value = "navclustresults",
            mainPanel(h5("Statistical Indices"),
                      mod_cluster_process_ui("cluster_precess"),
                      mod_cluster_results_ui("cluster_results"), 
                      width = 12
            )
          ),
          bslib::nav_menu(
            title = "Zone Validation",
            value = "navzoneval",
            bslib::nav(
              title = "Zone Compare Parameters",
              value = "navzonecompparam",
              mainPanel(mod_zoneCompare_parameters_ui("zone_param"),
                        div(style = "float: right;", 
                            actionButton("zoneResults", 
                                         label = "Go to results tab", 
                                         class = "btn-secondary")
                        ), width = 12)
            ),
            
            bslib::nav(
              title = "Zone Compare Results",
              value = "navzonecompresults",
              mainPanel(
                h5("Statistical Indices"),
                mod_zoneCompare_process_ui("zone_precess"),
                mod_zoneCompare_results_ui("zone_results"), 
                width = 12
              )
            )
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  golem::add_resource_path('www', app_sys('app/www'))
  
  tags$head(
    golem::favicon(ext = 'png'),
    golem::bundle_resources(
      path = app_sys('app/www'),
      app_title = 'FastMapping',
      app_builder = NULL
    ),
    # use_waiter(),
    shinyjs::useShinyjs(),
    waiter::useWaitress(),
    tags$link(rel = "stylesheet", type = "text/css", href = "www/mycssstyles.css")
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    
  )
}

