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
      title = "FastMapping",
      id = "navbar",
      theme = bslib::bs_theme(bootswatch = "united"),
      bslib::nav_panel(
        title = "",
        icon = icon("house"),
        value = "navhome",
        mainPanel(
          style = "margin: 0 auto",
            mod_home_text_ui("start_aplication")
          )
      ),
      
      bslib::nav_panel(
        title = "Data preparation",
        value = "navdataprep",
        bslib::navset_pill(
          id = "navdata",
          bslib::nav_panel(
            title = "Dataset",
            value = "navdataset",
            sidebarLayout(
              sidebarPanel(
              mod_upload_file_ui("dataset", label = h4("Dataset file:")),
              div(
                id = "variables_param",
                mod_select_variables_ui("dataset_cols"),
                mod_spatial_transformation_ui("dataset_spatial_transf")
              )
            ), 
              mainPanel(
                mod_show_data_table_ui("dataset_print"),
                mod_visualize_spatial_data_ui("mymap")
              )
            )),
          bslib::nav_panel(
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

      bslib::nav_panel(
        title = "Parameters Specification",
        value = "navallparam",
        
        bslib::navset_pill(
          id = "navparam",
          bslib::nav_panel(
            title = "Depuration Parameters",
            value = "navdepparam",
            mainPanel(mod_depuration_parameters_ui("depuration_param"), width = 12
                      )
          ),
          bslib::nav_panel(
            title = "Kriging Parameters",
            value = "navkrigparam",
            mainPanel(mod_kriging_parameters_ui("kriging_param"), width = 12)
          ),
          bslib::nav_panel(
            title = "Cluster Parameters",
            value = "navclustparam",
            mainPanel(mod_cluster_parameters_ui("cluster_param"), width = 12)
          )
          
        )
      ),
      bslib::nav_panel(
        title = "Analysis Results",
        value = "navanalyresults",
        bslib::navset_pill( 
          id = "navresult",
          bslib::nav_panel(
            title = "Depuration Results",
            value = "navdepresults",
            mainPanel(
              mod_depuration_process_ui("depuration_process"),
              mod_depuration_results_ui("depuration_results"),
              width = 12
              )
          ),
          bslib::nav_panel(
            title = "Kriging Results",
            value = "navkrigresults",
            bslib::page_fillable(
              mod_kriging_process_ui("kriging_process"),
              mod_kriging_results_ui("kriging_results")#,
              # width = 12
            )
          ),
          bslib::nav_panel(
            title = "Cluster Results",
            value = "navclustresults",
            mainPanel(
              mod_cluster_process_ui("cluster_precess"),
              mod_cluster_results_ui("cluster_results"), 
              width = 12
            )
          ),
          bslib::nav_menu(
            title = "Zone Validation",
            value = "navzoneval",
            bslib::nav_panel(
              title = "Zone Compare Parameters",
              value = "navzonecompparam",
              mainPanel(mod_zoneCompare_parameters_ui("zone_param"),
                        div(style = "float: right;", 
                            actionButton("zoneResults", 
                                         label = "Go to results tab", 
                                         class = "btn-secondary")
                        ), width = 12)
            ),
            
            bslib::nav_panel(
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
    shiny::tags$script(
      src = "https://www.googletagmanager.com/gtag/js?id=G-2XNDM9NJ7P",
      async = ""
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    
  )
}

