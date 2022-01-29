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
        mainPanel(h1("asdde"),
                  actionButton("start", "Start!"),
                  actionButton("show", "show!"),
                  actionButton("hide", "hide!"))
      ),
     
      bslib::nav(
        title = "Data preparation",
        value = "navdataprep",
        bslib::navs_pill(
          bslib::nav(
            title = "Dataset",
            value = "tpnldataset",
            sidebarLayout(
              sidebarPanel(
                h1("eee"),
                mod_upload_file_ui("dataset", label = h4("Dataset file:")),
                mod_select_variables_ui("dataset_cols"),
                mod_spatial_transformation_ui("dataset_spatial_transf")
              ),
              mainPanel(
                mod_show_data_table_ui("dataset_print"),
                mod_visualize_spatial_data_ui("mymap")
              )
            ))
            ,
          #
          bslib::nav(
          title = "Boundary",
          value = "tpnlboundary",
          sidebarLayout(
          sidebarPanel(mod_make_boundary_ui("make_boundary")),
          mainPanel(
                h1("asd"),
                mod_visualize_spatial_data_ui("boundaryMap")
                )
            )
          )
        )
        )
      )#,
      # bslib::nav(
      #   title = "Depuration Parameters",
      #   value = "navdepparam",
      #   mainPanel(mod_depuration_parameters_ui("depuration_param"))
      # ),
      # bslib::nav(
      #   title = "Kriging Parameters",
      #   value = "navkrigparam",
      #   mainPanel(mod_kriging_parameters_ui("kriging_param"))
      # ),
      # 
      # bslib::nav(
      #   title = "Depuration Results",
      #   value = "navdepresults",
      #   mainPanel(mod_depuration_process_ui("depuration_process"))
      # ),
      # bslib::nav(
      #   title = "Kriging Results",
      #   value = "navkrigparam",
      #   mainPanel(
      #     mod_kriging_process_ui("kriging_process")
      #             )
      # ),
      # bslib::nav(
      #   title = "otro Results",
      #   value = "otro",
      #   mainPanel(h1("que vemo?"),
      #                mod_kriging_results_ui("kriging_results")
      #             )
      # )
      # 
    # )
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
    shinyjs::useShinyjs()
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    
  )
}

