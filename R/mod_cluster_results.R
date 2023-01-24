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
  tagList(
    div(id = ns("noClustered"),
        p("No cluster process was made.")
    ),
    div(
      id = ns("yesClustered"),
      tagList(
        fluidRow(
          h5("Statistical Indices"),
          column(
            width = 8,
            shinycssloaders::withSpinner(plotly::plotlyOutput(ns(
              "GraficoIndicesConglo"
            )))
          )#,
          # column(width = 4,
          #        shinycssloaders::withSpinner(plotOutput(
          #          ns("corrPlotClasif")
          #        )))
        ),
        fluidRow(column(width = 8,
                        DT::dataTableOutput(
                          ns("TablaIndicesConglo")
                        ))),
        fluidRow(
          column(width = 4,
                 downloadButton(
                   ns("downloadClasification"), "Download Clasification"
                 ))
        )#,
        
        
        # fluidPage(fluidRow(
        #   column(width = 12 / 4,
        #          selectInput(ns('NumClust'),
        #                      'Clusters',
        #                      choices = NULL,
        #                      selected = NULL)#SelectBestCluster()
        #          # ListaChoices <-
        #            # colnames(Clasificacion()$DatosConCongl)[!colnames(Clasificacion()$DatosConCongl) %in% c(input$xmapa, input$ymapa)]
        #      ),
        #   column(width = 12 - 12 / 4,
        #          fluidPage(
        #            shinycssloaders::withSpinner(plotly::plotlyOutput(ns(
        #              'ClasificationPlot'
        #            ),
        #            height = "600px"))
        #          ))
        # ),
        # fluidRow(column(
        #   width = 12,
        #   fluidPage(shinycssloaders::withSpinner(plotOutput(
        #     ns('ClasifMatrCorr')
        #   )))
        # )))
        
        # ,shinycssloaders::withSpinner(uiOutput(ns("clustervalidationTables")))
        
      )
    )
  )
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
 
    myEvent <- bindEvent( reactive({
      clusterResults()
      variablesUsed()
      data_and_cluster()
    }), {
      myRes <- try(clusterResults(), silent = T)
      if (inherits(myRes, "try-error")) {
        shinyjs::show("noClustered")
        shinyjs::hide("yesClustered")
      } else {
        shinyjs::hide("noClustered")
      }
    }, ignoreNULL = FALSE)
    
    
    indices <- reactive({
      clusterResults()$indices
    })
    
    summaryResults <- reactive({
      clusterResults()$summaryResults
    })
    
    output$GraficoIndicesConglo <- plotly::renderPlotly({
      myEvent()
      req(indices())
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
    
    
    # output$corrPlotClasif <- renderPlot({
    #   if (length(Clasificacion()$VariablesUsedForCluster) == 1) {
    #     plotClasifVar <-
    #       ggplot(Clasificacion()$DatosConCongl, aes_string(Clasificacion()$VariablesUsedForCluster)) +
    #       geom_density()
    #   } else {
    #     
    #     if (length(Clasificacion()$VariablesUsedForCluster) < 10) {
    #       plotClasifVar <- ggpairs(Clasificacion()$DatosConCongl,
    #                                columns = Clasificacion()$VariablesUsedForCluster,
    #                                progress = FALSE)
    #       
    #     } else {
    #       plotClasifVar <- ggpairs(
    #         Clasificacion()$DatosConCongl,
    #         columns = Clasificacion()$VariablesUsedForCluster,
    #         lower = "blank",
    #         progress = TRUE
    #       )
    #       
    #     }
    #     
    #   }
    #   ValoresOutput$corrVariables <- plotClasifVar
    #   
    #   print(plotClasifVar)
    # })
    
    
    
    
    
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
                       paging = FALSE)
      )
    })
    
    
    
    
    
    
    
    
    
    
    # output$clustervalidationTables <- renderUI({
    #   req(clusterDiff())
    #   LL <- vector("list", length(clusterDiff()$Diferencias))
    #   for (i in seq_len(length(clusterDiff()$Diferencias))) {
    #     LL[[i]] <-
    #       fluidRow(
    #         column(width = 12 / 3,
    #                h3(names(
    #                  clusterDiff()$Diferencias
    #                )[i]),
    #                DT::dataTableOutput(ns(paste0("dt_", i)))),
    #         column(width = 12 / 4,
    #                br(),
    #                br(),
    #                plotOutput(
    #                  ns(paste0("plot_", i)), height = "190px"
    #                )),
    #         br(),
    #         br()
    #         
    #       )
    #   }
    #   return(LL)
    #   
    # })
    
    
    
    
    
  })
}
    
## To be copied in the UI
# mod_cluster_results_ui("cluster_results_ui_1")
    
## To be copied in the server
# mod_cluster_results_server("cluster_results_ui_1")
