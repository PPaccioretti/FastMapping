#' cluster_results UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @importFrom magrittr %>%
mod_cluster_results_ui <- function(id) {
  
  ns <- NS(id)
  tagList(div(id = ns("noClustered"),
              p("No cluster process was made.")),
          shinyjs::hidden(div(
            id = ns("yesClustered"),
            shinycssloaders::withSpinner(tagList(fluidRow(
              bslib::card(
                full_screen = TRUE,
                bslib::card_header("Statistical Indices"),
                bslib::card_body(
                  min_height = 200,
                  class = "gap-2 container",
                  bslib::layout_columns(
                    col_widths = c(6, 6),
                    row_heights = c(1, 1),
                    plotly::plotlyOutput(ns("GraficoIndicesConglo")),
                    bslib::layout_columns(
                      col_widths = c(12, -3, 6, -3),
                      row_heights = c(0.7, 0.3),
                      DT::dataTableOutput(ns("TablaIndicesConglo")),
                      downloadButton(ns("downloadClasification"), "Download Clasification")
                    )
                  )
                )),
                
                mod_ggplot_sf_variable_ui(ns('cluster_plot'))
            )))
          )))
}

#' cluster_results Server Functions
#'
#' @noRd 
mod_cluster_results_server <- function(id,
                                       clusterResults = reactive(NULL),
                                       variablesUsed = reactive(NULL),
                                       data_and_cluster = reactive(NULL)) {
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    
    observeEvent(is.null(variablesUsed()), {
      shinyjs::show("noClustered")
      shinyjs::disable("navzoneval")
      shinyjs::hide("yesClustered") 
    },ignoreNULL = FALSE,
      ignoreInit = TRUE)
    # myEvent <- 
    observeEvent(is.null(clusterResults()) &
                   is.null(data_and_cluster()) ,
                 {
                   myRes <- try(clusterResults(), silent = T)
                   shinyjs::hide("yesClustered")
                   if (inherits(myRes, "try-error") || is.null(myRes)) {
                     shinyjs::show("noClustered")
                     shinyjs::disable("navzoneval")
                     shinyjs::hide("yesClustered")
                   } else {
                     shinyjs::hide("noClustered")
                     shinyjs::enable("navzoneval")
                     shinyjs::show("yesClustered")
                   }
                 },ignoreNULL = FALSE,
                 ignoreInit = FALSE)
    
    
    indices <- reactive({
      clusterResults()$indices
    })
    
    summaryResults <- reactive({
      clusterResults()$summaryResults
    })
    
    output$GraficoIndicesConglo <- plotly::renderPlotly({
      # myEvent()
      req(indices())
      golem::print_dev('Start GraficoIndicesConglo')
      indicesClust <- indices()
      dataIndicConglWide <-
        data.frame(
          "Cluster" = indicesClust[, 1],
          scale(indicesClust[, -1]),
          check.names = FALSE
        )
      
      dataIndicesConglomLong <- stats::reshape(
        dataIndicConglWide,
        idvar = "Cluster",
        times = names(dataIndicConglWide)[-1],
        timevar = "Index",
        v.names = "Value",
        varying = list(names(dataIndicConglWide)[-1]),
        direction = "long"
      )
      dataIndicesConglomLong$Index <-
        factor(
          dataIndicesConglomLong$Index,
          labels = unique(dataIndicesConglomLong$Index),
          levels = unique(dataIndicesConglomLong$Index)
        )
      
      dataIndicesConglomLong$isSummary <-
        as.numeric(dataIndicesConglomLong$Index == "Summary Index") + 1
      
      ggplotCongl <-  
        ggplot2::ggplot(dataIndicesConglomLong,
                        ggplot2::aes(x = Cluster, y = Value, color = Index)) +
        ggplot2::geom_point() +
        ggplot2::geom_line(linetype = dataIndicesConglomLong$isSummary) +
        ggplot2::labs(y = "Standardized value") 
      
      plotly::ggplotly(ggplotCongl) %>%
        plotly::layout(autosize = TRUE)
      
    })
    
    mod_ggplot_sf_variable_server("cluster_plot",
      dataset = data_and_cluster,
      indices = indices
    )
    
    # mod_visualize_spatial_data_server("plotclusters",
    #                                   dataset = data_and_cluster,
    #                                   vars = clusterResults,
    #                                   maxMarkerToShow = reactive(1000000)
    # )
    
    output$downloadClasification <- downloadHandler(
      filename = function() {
        paste('Clasification-', Sys.Date(), '.gpkg', sep = '')
      },
      content = function(file) {
        req(data_and_cluster())
        sf::write_sf(
          data_and_cluster(),
          file)
      }
    )
    
    
    # output$TablaResultadosConglom <- DT::renderDataTable({
    #   DT::datatable(
    #     Clasificacion()$ResultadosConglom,
    #     rownames = FALSE,
    #     options = list(searching = FALSE,
    #                    paging = FALSE)
    #   )
    # })
    
    output$TablaIndicesConglo <- DT::renderDataTable({
      req(indices())
      indicesClust <- indices()
      DT::datatable(
        indicesClust,
        rownames = FALSE,
        options = list(searching = FALSE,
                       paging = FALSE,
                       info = FALSE)
      ) %>% 
        DT::formatSignif(-1, 4)
    })
    
    
    
  })
}
    
## To be copied in the UI
# mod_cluster_results_ui("cluster_results_ui_1")
    
## To be copied in the server
# mod_cluster_results_server("cluster_results_ui_1")
