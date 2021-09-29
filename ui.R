# Copyright 2019 Pablo Paccioretti
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.



fluidPage(
  title = "FastMapping",
  theme = bslib::bs_theme(bootswatch = "united"),
  lang = 'en',
  useShinyjs(),
  #Added this js
  extendShinyjs(text = jscode, functions = c()),
  use_waiter(),
  mainPanel(
    h2("FastMapping"),
    width = 10,
    # tags$head(
    #   tags$style(type = 'text/css',
    #              ".nav-pills {font-size: 18px} "),
    #   tags$style(
    #     HTML(
    #       '
    #                           .modal.in .modal-dialog{
    #                                     width:100%;
    #                                     height:100%;
    #                                     margin:0px;
    #                                     }
    #
    #                                     .modal-content{
    #                                     width:100%;
    #                                     height:100%;
    #                                     }
    #                                     '
    #     )
    #   )
    # ),
    tabsetPanel(
      id = "PanelTabSet",
      type = "pills",
      
      tabPanel(
        "Home",
        mainPanel(
          h1('Welcome to FastMapping'),
          p(
            'If you have any question please write to fastmapping@agro.unc.edu.ar, brief tutorial is available in ',
            a("this link", href = "https://drive.google.com/open?id=1r2-tx35NGLzIjL0CLNR6E783ZRDsWQmf",  target = "_blank"),
            "."
          ),
          fluidRow(
            column(
              width = 6,
              h3("Tools for univariate analysis:"),
              tags$ul(
                tags$li("Depuration",
                        tags$ul(
                          tags$li("Global outliers"),
                          tags$li("Spatial outliers"),
                          tags$li("Border effects")
                        )),
                tags$li("Spatial interpolation",
                        tags$ul(
                          tags$li("Variogram fitting"),
                          tags$li("Kriging prediction")
                        )),
                tags$li("Classification",
                        tags$ul(tags$li(
                          "Fuzzy k-means cluster"
                        )))
                
              ),
              h4("Example dataset:"),
              tags$ul(tags$li(
                a("Mapping yield data in a barley field",
                  href = "https://drive.google.com/uc?export=download&id=1ZzWDd9BHeZuebq_xPpNgSv9XsRmXEOTb")
              ),
              tags$li(
                a("Wheat raw data",
                  href = "https://drive.google.com/uc?export=download&id=1bpCkvEoC7EvmycSFQwrtnzWCJ5WBDrIL")
              ))
              
            ),
            column(
              width = 6,
              h3("Tools for multivariate analysis:"),
              tags$ul(
                tags$li("Spatial Principal Components"),
                tags$li("Fuzzy k-means on spatial principal components (KM-sPC)")
              ),
              h4("Example dataset:"),
              tags$ul(tags$li(
                a("Zoning a field from yield and soil properties",
                  href = "https://drive.google.com/uc?export=download&id=1SeJYNmzg-d26E_nydzKtcImshEYZB6UO"),
                "."
              )),
              br(),
              br(),
              div(style = "text-align: center;",
                  actionButton(
                    "startApl", "Start!", 
                    icon = icon("play-circle"),
                    class = "btn-success",
                    style = 'text-align: center; font-size:180%;'
                  ))
            )
          ),
          
          
        )
      ),
      
      
      tabPanel("Dataset",
               value = "DatasetTab",
               sidebarLayout(
                 sidebarPanel(
                   fileInput(
                     "file",
                     label = h4(
                       "Dataset ",
                       tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                       bsButton(
                         "q1",
                         label = "",
                         icon = icon("question"),
                         style = "info",
                         size = "extra-small"
                       )
                     ),
                     multiple = FALSE,
                     accept = c(
                       ".txt",
                       "text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv"
                     )
                   ),
                   bsPopover(
                     id = "q1",
                     title = NULL,
                     content = paste0(datasetHelp, br(),
                                      "Maximum file size 20MB"),
                     placement = "right",
                     trigger = "focus",
                     options = list(container = "body")
                   ),
                   # helpText("Maximum file size 20MB"),
                   tableOutput("Mensaje"),
                   tags$hr(),
                   checkboxInput('edges', 'Optional: upload edges file', value = FALSE),
                   uiOutput('bordesFile'),
                   uiOutput("ui_data_param")

                 ),
                 mainPanel(uiOutput("tabpanel_data"))
                 
               )),
      tabPanel(
        "Depuration",
        value = "DepurationTab",
        checkboxGroupInput(
          inputId = "AutomaticDep",
          label = h4("Data depuration"),
          choices = c(Automatic =
                        "Automatico"),
          inline = TRUE,
          selected = "Automatic"
        ),
        bsTooltip(
          "AutomaticDep",
          HTML(AutomaticDepHelp),
          placement = "bottom",
          trigger = "hover",
          options = NULL
        ) ,
        
        conditionalPanel(
          condition = "input.AutomaticDep != 'Automatico' ",
          checkboxGroupInput(
            inputId = "mDepuration",
            label = h4("Methods"),
            choices = list("Glogal Outliers" =
                             "Outliers",
                           "Spatial Outliers" =
                             "Inliers"),
            # inline = TRUE,
            width = "20%"
          ),
          fluidRow(
            column(
              width = 4,
              conditionalPanel(
                condition = "input.mDepuration.indexOf('Outliers') > -1",
                h4("Global Outliers Options:"),
                helpText(
                  "Upper and lower threshold boundaries to constrain data within a range of realistic values"
                ),
                fluidRow(
                  column(
                    width = 4,
                    numericInput(
                      inputId = "ylimitmin",
                      label =
                        "Min",
                      value = 0,
                      width = "100%"
                    ),
                    bsTooltip(
                      "ylimitmin",
                      yLimMinDepHelp,
                      placement = "bottom",
                      trigger = "hover",
                      options = NULL
                    )
                  ),
                  
                  column(
                    width = 4,
                    numericInput(
                      inputId = "ylimitmax",
                      label =
                        "Max",
                      value = NA,
                      width = "100%"
                    ),
                    bsTooltip(
                      "ylimitmax",
                      yLimMaxDepHelp,
                      placement = "bottom",
                      trigger = "hover",
                      options = NULL
                    )
                  ),
                ),
                helpText(
                  "Removes all data points which are more than N times the standard deviation from the mean value"
                ),
                bootstrapPage(
                  column(
                    width = 6,
                    numericInput(
                      inputId = "DEOut",
                      label =
                        "Standard deviation",
                      value = 3
                    )
                  ),
                  bsTooltip(
                    "DEOut",
                    DEOutDepHelp,
                    placement = "bottom",
                    trigger = "hover",
                    options = NULL
                  )
                )
              )
            ),
            
            column(
              width = 4,
              conditionalPanel(
                condition = "input.mDepuration.indexOf('Inliers') > -1",
                h4("Spatial Outliers Options:"),
                helpText("Search radius to identify a local neighborhood for each data point"),
                
                fluidRow(
                  column(
                    width = 4,
                    numericInput(
                      inputId = "VecinosMin",
                      label = "Min. distance",
                      value = 0,
                      width = "100%"
                    )
                  ),
                  column(
                    width = 4,
                    numericInput(
                      inputId = "VecinosMax",
                      label = "Max. distance",
                      value = 20,
                      width = "100%"
                    )
                  )
                ),
                fluidRow(
                  checkboxInput("moranPlot",
                                "Remove spatial outlier by Moran plot criteria")
                )
              )
            ),
            
            column(
              width = 4,
              conditionalPanel(
                condition = "input.mDepuration.indexOf('Inliers') > -1 || input.mDepuration.indexOf('Outliers') > -1",
                h4("Border Effects:"),
                helpText("Remove data points for a given distance from field edges"),
                fluidRow(
                  column(
                    width = 6,
                    checkboxInput(
                      inputId = "SacoBordes",
                      label = "Remove borders",
                      value = 0.0,
                      width = "100%"
                    )
                  ),
                  column(
                    width = 6,
                    conditionalPanel(
                      condition = "input.SacoBordes",
                      numericInput(
                        inputId = "Buffer",
                        label = "Buffer",
                        value = -20,
                        width = "100%"
                      )
                      
                    )
                  )
                )
              )
            )
          )
        )
      ),
      
      
      tabPanel("Prediction",
               value = "PredictionTab",
               fluidPage(fluidRow(
                 column(
                   width = 2,
                   h4("Select spatial model(s) to fit"),
                   checkboxInput('bar', 'Automatic', value =
                                   T),
                   bsTooltip(
                     "bar",
                     AutomaticHelp,
                     placement = "bottom",
                     trigger = "hover",
                     options = NULL
                   ),
                   
                   uiOutput("ModelosA"),
                   checkboxInput('cressie', 'Robust variogram estimate', value =
                                   FALSE),
                   bsTooltip(
                     "cressie",
                     CressieHelp,
                     placement = "bottom",
                     trigger = "hover",
                     options = NULL
                   )
                   
                 ),
                 
                 column(
                   width = 4,
                   radioButtons(
                     "tKriging",
                     h4("Select kriging method for spatial interpolation"),
                     choices = list(
                       "Ordinary" = 1,
                       "Universal Kriging (First Order)" = 2,
                       "Universal Kriging (Second Order)" = 3
                     ),
                     inline = F
                   ),
                   h4("Kriging options:"),
                   helpText(
                     "Local neighbourhood selections based on distance as radius (Max.Dist), number of data points (Max, Min), of nearest site of the target point"
                   ),
                   fluidRow(
                     column(
                       width = 4,
                       numericInput(
                         inputId = "nmin",
                         label =
                           "Min. n",
                         value =  7,
                         width = "80%"
                       ),
                       bsTooltip(
                         "nmin",
                         nminKriginHelp,
                         placement = "bottom",
                         trigger = "hover",
                         options = NULL
                       )
                     ),
                     
                     column(
                       width = 4,
                       numericInput(
                         inputId = "nmax",
                         label = "Max. n",
                         value = 25,
                         width = "80%"
                       ),
                       bsTooltip(
                         "nmax",
                         nmaxKriginHelp,
                         placement = "bottom",
                         trigger = "hover",
                         options = NULL
                       )
                     ),
                     
                     
                     column(
                       width = 4,
                       numericInput(
                         inputId = "distmax",
                         label =
                           "Max. Dist.",
                         value =  NA,
                         width = "80%"
                       ),
                       bsTooltip(
                         "distmax",
                         distmaxKriginHelp,
                         placement = "bottom",
                         trigger = "hover",
                         options = NULL
                       )
                     )
                   ),
                   fluidRow(
                     column(
                       width = 4,
                       numericInput(
                         inputId = "block",
                         label =
                           "Block",
                         min = 0,
                         value = 0,
                         width = "80%"
                       ),
                       bsTooltip(
                         "block",
                         blockKriginHelp,
                         placement = "bottom",
                         trigger = "hover",
                         options = NULL
                       )
                     ),
                     column(width = 4,
                            numericInput(
                              "dimGrilla",
                              "Cellsize",
                              value = 10,
                              min = 0.5
                            )),
                     bsTooltip(
                       "dimGrilla",
                       dimGrillaKriginHelp,
                       placement = "bottom",
                       trigger = "hover",
                       options = NULL
                     )
                     
                   )
                 ),
                 column(
                   width = 6,
                   h4("Output graphical options"),
                   
                   h5("Key Scale of predicted values"),
                   fluidRow(
                     column(
                       width = 2,
                       numericInput(
                         inputId = "min",
                         label = "Min.",
                         value = NULL,
                         width = "100%"
                       ),
                       bsTooltip(
                         "min",
                         minPredScaleKriginHelp,
                         placement = "bottom",
                         trigger = "hover",
                         options = NULL
                       )
                     ),
                     column(
                       width = 2,
                       numericInput(
                         inputId = "max",
                         label = "Max.",
                         value = NULL,
                         width = "100%"
                       ),
                       bsTooltip(
                         "max",
                         maxPredScaleKriginHelp,
                         placement = "bottom",
                         trigger = "hover",
                         options = NULL
                       )
                     )
                   ),
                   h5("Key Scale for prediction variance"),
                   fluidRow(
                     column(
                       width = 2,
                       
                       numericInput("min_var",
                                    "Min.",
                                    min = 0, NULL, width = "100%")
                       ,
                       bsTooltip(
                         "min_var",
                         min_varScaleKriginHelp,
                         placement = "bottom",
                         trigger = "hover",
                         options = NULL
                       )
                     ),
                     column(
                       width = 2,
                       
                       numericInput("max_var", "Max.", NULL, width = "100%"),
                       bsTooltip(
                         "max_var",
                         max_varScaleKriginHelp,
                         placement = "bottom",
                         trigger = "hover",
                         options = NULL
                       )
                     )
                   )
                   
                 )
               )))
      ,
      tabPanel("Results",
               value = "ResultsTab",
               uiOutput("resultados")),
      
      
      tabPanel(
        title = textOutput("tabPanel_title"),
        value = "ClusterTab",
        tabsetPanel(
          id = "ClustersTabs",
          tabPanel(
            title = "Parameters for KM-sPC classification",
            value = "ClasifParameters",
            fluidRow(
              column(
                width = 12 / 3,
                h3("Variable selection process"),
                p(
                  "A variable selection process is recommended to improve validation of the delineated zones."
                ),
                checkboxInput("makeSelectProces", "Make variable selection process", value = FALSE),
                
                
                uiOutput("SelectProcessUI"),
                bsTooltip(
                  "makeSelectProces",
                  makeSelectProcesHelp,
                  placement = "bottom",
                  trigger = "hover",
                  options = NULL
                )
              ),
              
              uiOutput("ui_multivariate_params"),
              
              column(
                width = 12 / 3,
                h3("Fuzzy k-means parameters"),
                sliderInput(
                  "clusters",
                  "Number of Cluster to evaluate",
                  min = 2,
                  max = 15,
                  value = c(2, 6)
                ),
                radioButtons(
                  "distancia",
                  "Distance",
                  choices = c("Euclidean" = "euclidean", "Manhattan" = "manhattan")
                ),
                numericInput(
                  "iteraciones",
                  "Iterations",
                  value = 1000,
                  min = 1,
                  step = 10
                ),
                numericInput("ExpDif", "Fuzzy exponent", value =
                               1.3, step = 0.05),
                
                bsTooltip(
                  "clusters",
                  clustersClusterHelp,
                  placement = "bottom",
                  trigger = "hover",
                  options = NULL
                ),
                bsTooltip(
                  "distancia",
                  distanciaClusterHelp,
                  placement = "bottom",
                  trigger = "hover",
                  options = NULL
                ),
                bsTooltip(
                  "iteraciones",
                  iteracionesClusterHelp,
                  placement = "bottom",
                  trigger = "hover",
                  options = NULL
                ),
                bsTooltip(
                  "ExpDif",
                  ExpDifClusterHelp,
                  placement = "bottom",
                  trigger = "hover",
                  options = NULL
                )
                
              )
            )
          ),
          tabPanel(
            "Classification results",
            fluidPage(
              fluidRow(column(width = 8,
                              withSpinner(
                                plotlyOutput("GraficoIndicesConglo")
                              )),
                       column(width = 4,
                              withSpinner(
                                plotOutput("corrPlotClasif")
                              ))),
              fluidRow(column(
                width = 8,
                DT::dataTableOutput("TablaIndicesConglo")
              )),
              fluidRow(
                column(width = 6,
                       DT::dataTableOutput("TablaResultadosConglom")),
                column(
                  width = 4,
                  downloadButton("downloadClasification", "Download Clasification")
                )
              )
            )
          ),
          tabPanel("Cluster Plot",
                   fluidPage(
                     fluidRow(
                       column(width = 12 / 4,
                              fluidPage(uiOutput("SelectorCong"))),
                       column(width = 12 - 12 / 4,
                              fluidPage(withSpinner(
                                plotlyOutput('ClasificationPlot',
                                             height = "600px")
                              )))
                     ),
                     fluidRow(column(width = 12,
                                     fluidPage(
                                       withSpinner(plotOutput('ClasifMatrCorr'))
                                     )))
                   )),
          tabPanel("Validation",
                   fluidPage(
                     uiOutput(
                     "clustervalidationTables"
                   )
                   )
                   )
          
        )
        
      ),
      
      
      tabPanel(
        "Report",
        value = "ReportTab",
        fluidPage(
          p(
            "This section is being developed. It may contain problems when the report is generated. Please, if it happens, contact",
            a("fastmapping@agro.unc.edu.ar", href = "mailto:fastmapping@agro.unc.edu.ar")
          ),
          radioButtons('format',
                       'Document format',
                       'HTML',  #c('PDF', 'HTML', 'Word'),
                       inline = TRUE),
          downloadButton("report", "Generate report")
        )
      )
      
      
    )
  )
  ,
  waiter::waiter_show_on_load(
    html = tagList(waiter::spin_pulsar(),
                   br(),
                   br(),
                   "Loading..."),
    color = "#999999"
  )
)
