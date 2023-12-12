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
            shinycssloaders::withSpinner(tagList(
              fluidRow(
                h5("Statistical Indices"),
                column_md(width = 5,
                          plotly::plotlyOutput(ns(
                            "GraficoIndicesConglo"
                          ))),
                column_md(width = 7,
                          DT::dataTableOutput(ns(
                            "TablaIndicesConglo"
                          )),
                          
                          div(class = "text-center py-5",
                                 downloadButton(
                                   ns("downloadClasification"), "Download Clasification"
                                 ))
                          )
              ),
              hr(style = "border-top: 1px solid #000000;"),
              fluidRow(
                # column_md(
                  # width = 12,
                class = "m-md-5",
                  mod_visualize_spatial_data_ui(ns("plotclusters"),
                                                lblToPlot = "Cluster to plot")
                # ),
                
                
                
              )
              
            ))
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
 
    
    observeEvent(is.null(variablesUsed()) , {
      shinyjs::show("noClustered")
      shinyjs::hide("yesClustered") 
    },ignoreNULL = FALSE,
      ignoreInit = TRUE)
    # myEvent <- 
    observeEvent(is.null(clusterResults()) &
                   is.null(data_and_cluster()) ,
                 {
                   myRes <- try(clusterResults(), silent = T)
                   shinyjs::hide("yesClustered")
                   if (inherits(myRes, "try-error")) {
                     shinyjs::show("noClustered")
                     shinyjs::hide("yesClustered")
                   } else {
                     shinyjs::hide("noClustered")
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
    
    mod_visualize_spatial_data_server("plotclusters",
                                      dataset = data_and_cluster,
                                      vars = clusterResults
    )
    
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
