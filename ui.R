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
suppressPackageStartupMessages({
  require(shiny)
  # require(shinythemes)
  library(plotly)
  library(waiter)
})

jscode <- "
shinyjs.init = function() {
    $('#PanelTabSet li a[data-value=\"DatasetTab\"]').hide();
  $('#PanelTabSet li a[data-value=\"DepurationTab\"]').hide();
  $('#PanelTabSet li a[data-value=\"PredictionTab\"]').hide();
  $('#PanelTabSet li a[data-value=\"ResultsTab\"]').hide();
  $('#PanelTabSet li a[data-value=\"ClusterTab\"]').hide();
  $('#PanelTabSet li a[data-value=\"ReportTab\"]').hide();
}"

shinyUI(
  fluidPage(
    title = "FastMapping",
    theme = bslib::bs_theme(bootswatch = "united"),
    useShinyjs(),
    #Added this js
    extendShinyjs(script = "src/MyFunctJS.js", text = jscode, functions = c()),
    use_waiter(),
    mainPanel(
      h2("FastMapping"),
      width = 10,
      tags$head(
        tags$style(type = 'text/css',
                   ".nav-pills {font-size: 18px} "),
        tags$style(
          HTML(
            '
                              .modal.in .modal-dialog{
                                        width:100%;
                                        height:100%;
                                        margin:0px;
                                        }

                                        .modal-content{
                                        width:100%;
                                        height:100%;
                                        }
                                        '
          )
        )
      ),
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
            h4("Example dataset:",
               tags$ul(tags$li(
                 a("Mapping yield data in a barley field",
                   href = "https://drive.google.com/uc?export=download&id=1ZzWDd9BHeZuebq_xPpNgSv9XsRmXEOTb")
               ),
               tags$li(
                 a("Wheat raw data",
                   href = "https://drive.google.com/uc?export=download&id=1bpCkvEoC7EvmycSFQwrtnzWCJ5WBDrIL")
               ))),
            
            h3("Tools for multivariate analysis:"),
            tags$ul(
              tags$li("Spatial Principal Components"),
              tags$li("Fuzzy k-means on spatial principal components (KM-sPC)")
            ),
            h4("Example dataset:",
               tags$ul(tags$li(
                 a("Zoning a field from yield and soil properties",
                   href = "https://drive.google.com/uc?export=download&id=1SeJYNmzg-d26E_nydzKtcImshEYZB6UO"),
                 "."
               ))),
            actionButton("startApl", "Start!", icon = icon("play-circle"))
            
            
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
                     tags$hr(),
                     h5("Reading file options:"),
                     checkboxInput(
                       'header',
                       'The file contains the names of the variables as its first line.',
                       value = TRUE
                     ),
                     br()
                     # ,uiOutput("SepData")
                     ,
                     wellPanel(
                       uiOutput("BaseColx", inline = TRUE)
                       ,
                       uiOutput("BaseColy", inline = TRUE)
                       ,
                       uiOutput("BaseColRend", inline = TRUE)
                       ,
                       uiOutput("TextoAviso", inline = TRUE)
                     )
                     ,
                     wellPanel(
                       uiOutput("Hemisf", inline = TRUE)
                       ,
                       uiOutput("Zona", inline = TRUE)
                     )
                     
                   ),
                   mainPanel(uiOutput("tb"))
                   
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
            selected = "Automatic",
            width = "10%"
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
            # checkboxInput(inputId="SacoBordes2", label="Remove borders", value = 0.0, width = "100%")
            # ),
            # bsTooltip("mDepuration", MethodsDepHelp,
            #           placement = "rigth", trigger = "hover",
            #           options = NULL),
            
            column(
              width = 4,
              conditionalPanel(
                condition = "input.mDepuration.indexOf('Outliers') > -1",
                h4("Global Outliers Options:"),
                helpText(
                  "Upper and lower threshold boundaries to constrain data within a range of realistic values"
                ),
                bootstrapPage(
                  column(width = 4,
                         div(
                           style = "display:inline-block",
                           numericInput(
                             inputId = "ylimitmin",
                             label =
                               "Min",
                             value = 0,
                             width = "100%"
                           )
                         )),
                  bsTooltip(
                    "ylimitmin",
                    yLimMinDepHelp,
                    placement = "bottom",
                    trigger = "hover",
                    options = NULL
                  ),
                  
                  column(width =
                           4,
                         div(
                           style = "display:inline-block",
                           numericInput(
                             inputId = "ylimitmax",
                             label =
                               "Max",
                             value = NA,
                             width = "100%"
                           )
                         )),
                  bsTooltip(
                    "ylimitmax",
                    yLimMaxDepHelp,
                    placement = "bottom",
                    trigger = "hover",
                    options = NULL
                  )
                  ,
                  tags$br()
                  ,
                  tags$br()
                  ,
                  tags$br()
                  ,
                  tags$br()
                ),
                # h4("Standard deviation:"),
                helpText(
                  "Removes all data points which are more than N times the standard deviation from the mean value"
                ),
                bootstrapPage(
                  column(width = 5, div(
                    style = "display:inline-block",
                    numericInput(
                      inputId = "DEOut",
                      label =
                        "Standard deviation",
                      value = 3,
                      width = "100%"
                    )
                  )),
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
                
                # column(width =4,div(
                #   style="display:inline-block",
                #   numericInput(inputId="ValorVecindario",
                #                label="Neighbors",
                #                value = 0.0,
                #                width = "100%"))),
                
                bootstrapPage(
                  fluidRow(
                  column(width = 4,
                         div(
                           style = "display:inline-block",
                           numericInput(
                             inputId = "VecinosMin",
                             label = "Min. distance",
                             value = 0,
                             width = "100%"
                           )
                         )),
                  column(width =
                           4, div(
                             style = "display:inline-block",
                             numericInput(
                               inputId = "VecinosMax",
                               label = "Max. distance",
                               value = 20,
                               width = "100%"
                             )
                           ))),
                  fluidRow(
                    checkboxInput("moranPlot",
                                  "Remove spatial outlier by Moran plot criteria")
                  )
                  
                  ,
                  tags$br()
                  ,
                  tags$br()
                  ,
                  tags$br()
                  ,
                  tags$br()
                  
                )
              )
            )
            
            ,
            column(
              width = 4,
              conditionalPanel(
                condition = "input.mDepuration.indexOf('Inliers') > -1 || input.mDepuration.indexOf('Outliers') > -1",
                h4("Border Effects:"),
                helpText("Remove data points for a given distance from field edges"),
                bootstrapPage(
                  column(width = 4, div(
                    style = "display:inline-block",
                    checkboxInput(
                      inputId = "SacoBordes",
                      label = "Remove borders",
                      value = 0.0,
                      width = "100%"
                    )
                  )),
                  
                  conditionalPanel(condition = "input.SacoBordes",
                                   column(
                                     width = 4, div(
                                       style = "display:inline-block",
                                       numericInput(
                                         inputId = "Buffer",
                                         label = "Buffer",
                                         value = -20,
                                         width = "100%"
                                       )
                                     )
                                   ))
                  ,
                  tags$br()
                  ,
                  tags$br()
                  ,
                  tags$br()
                  ,
                  tags$br()
                  
                )
              )
            )
          )
        ),
        
        
        tabPanel("Prediction",
                 value = "PredictionTab",
                 fluidPage(
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
                     fluidPage(
                       column(
                         width = 4,
                         div(
                           style = "display:inline-block",
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
                           
                         )
                       )
                       ,
                       column(
                         width = 4,
                         div(
                           style = "display:inline-block",
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
                         )
                       )
                       
                       ,
                       column(
                         width = 4,
                         div(
                           style = "display:inline-block",
                           numericInput(
                             inputId = "distmax",
                             label =
                               "Max. Dist.",
                             value =  1000,
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
                       )
                       ,
                       column(
                         width = 4,
                         div(
                           style = "display:inline-block",
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
                         )
                       ),
                       column(width = 3,
                              div(
                                style = "display:inline-block",
                                numericInput(
                                  "dimGrilla",
                                  "Grid dimentions",
                                  value = 10,
                                  min =
                                    0,
                                  width = "100%"
                                )
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
                     fluidPage(
                       fluidRow(
                         h5("Key Scale of predicted values"),
                         column(
                           width = 2,
                           div(
                             style = "display:inline-block",
                             numericInput(
                               inputId = "min",
                               label = "Min.",
                               value = NULL,
                               width = "100%"
                             )
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
                           div(
                             style = "display:inline-block",
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
                         )
                       ),
                       fluidRow(
                         h5("Key Scale for prediction variance"),
                         column(
                           width = 2,
                           div(
                             style = "display:inline-block",
                             numericInput("min_var", "Min.", min =
                                            0, NULL, width = "100%")
                           ),
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
                           div(style = "display:inline-block",
                               numericInput("max_var", "Max.", NULL, width = "100%")),
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
                   )
                 ))
        ,
        tabPanel("Results",
                 value = "ResultsTab",
                 uiOutput("resultados"))
        
        ,
        tabPanel(
          title = textOutput("tabPanel_title"),
          # "Cluster", #
          value = "ClusterTab",
          # uiOutput("Clasificador")
          tabsetPanel(
            id = "ClustersTabs",
            tabPanel(
              title = "Parameters for KM-sPC classification",
              value = "ClasifParameters",
              
              
              column(
                width = 12 / 3,
                h3("Variable selection process"),
                p("A variable selection process is recommended to improve validation of the delineated zones."),
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
              
              
              column(
                width = 12 / 3,
                h3("Spatial PCA parameters"),
                # br(),
                checkboxInput("centrado", "Centered variables", value = TRUE),
                sliderInput(
                  "varexplicada",
                  "Explained variance (%)",
                  min = 0,
                  max = 100,
                  value = 70
                ),
                
                h4("Neighborhood network"),
                checkboxInput("vecindarionulo", "Data with null neighbor", value = FALSE),
                sliderInput(
                  "distanciavecino",
                  "Distance between neighbors",
                  min = 0,
                  max = 1000,
                  value = c(0, 35)
                ),
                uiOutput("changedistanciavecinomax"),
                
                
                bsTooltip(
                  "centrado",
                  centradoClusterHelp,
                  placement = "bottom",
                  trigger = "hover",
                  options = NULL
                ),
                bsTooltip(
                  "varexplicada",
                  varexplicadaClusterHelp,
                  placement = "bottom",
                  trigger = "hover",
                  options = NULL
                ),
                bsTooltip(
                  "vecindarionulo",
                  vecindarionuloClusterHelp,
                  placement = "bottom",
                  trigger = "hover",
                  options = NULL
                ),
                # ),
                # column(width = 12/4,
                bsTooltip(
                  "distanciavecino",
                  distanciavecinoClusterHelp,
                  placement = "bottom",
                  trigger = "hover",
                  options = NULL
                )
                
                
              ),
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
              
              
              
              
            ),
            tabPanel(
              "Classification results",
              fluidPage(
                fluidRow(
                  column(
                    width = 8,
                    plotlyOutput("GraficoIndicesConglo") %>%
                      withSpinner()
                  ),
                  column(width = 4,
                         plotOutput("corrPlotClasif") %>%
                           withSpinner())
                ),
                fluidRow(column(
                  width = 8,
                  DT::dataTableOutput("TablaIndicesConglo")
                )),
                ##
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
                         column(width = 12 - 12 /
                                  4,
                                fluidPage(
                                  plotlyOutput('ClasificationPlot', height = "600px")
                                ) %>%
                                  withSpinner())
                       ),
                       fluidRow(column(
                         width = 12,
                         fluidPage(plotOutput('ClasifMatrCorr') %>%
                                     withSpinner())
                       ))
                     )),
            tabPanel("Validation",
                     fluidPage(fluidRow(
                       uiOutput("clustervalidationTables")
                     )))
            
            #   ,tabPanel("Plot Classification",
            #            sidebarPanel(width = 3,
            #                         # selectInput('x', 'X', choices = nombresCol(), selected = nombresCol()[1]),
            #                         # selectInput('y', 'Y', choices = nombresCol(), selected = nombresCol()[2]),
            #                         selectInput('NumClust', 'Clusters',choices = colnames(Clasificacion()$DatosConCongl), selected = NULL)#[NROW(nombresCol())])#,
            #                         # , textOutput("Mensaje")
            #
            #            ),
            #            mainPanel(width = 9,
            #                      plotlyOutput('ClasificationPlot', height = "600px"))
            #
            #
            # )
          )
          
          
          
        )
        
        ,
        tabPanel(
          "Report",
          value = "ReportTab",
          fluidPage(
            p(
              "This section is being developed. It may contain problems when the report is generated. Please, if it happens, contact",
              a("fastmapping@agro.unc.edu.ar", href = "mailto:fastmapping@agro.unc.edu.ar")
            ),
            radioButtons('format', 'Document format', 'HTML',  #c('PDF', 'HTML', 'Word'),
                         inline = TRUE),
            downloadButton("report", "Generate report")
          )
        )
        
        
      )
    )
    ,
    waiter_show_on_load(
      html = tagList(spin_pulsar(),
                     br(),
                     br(),
                     "Loading..."),
      color = "#999999"
    )
  )
)
