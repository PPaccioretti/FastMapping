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

require(shiny)
require(shinythemes)
library(plotly)
shinyUI(
  fluidPage(title="FastMapping",
            theme = shinytheme("united"),
            mainPanel(h2("FastMapping"),
                      width = 10,
                      tags$head(
                        tags$style(type='text/css', 
                                   ".nav-pills {font-size: 18px} "),
                        tags$style(HTML('
                              .modal.in .modal-dialog{
                                        width:100%;
                                        height:100%;
                                        margin:0px;
                                        }
                                        
                                        .modal-content{
                                        width:100%;
                                        height:100%;
                                        }
                                        '))),
                      tabsetPanel(
                        id="PanelTabSet",
                        type = "pills",
                        tabPanel("Dataset",
                                 sidebarLayout(
                                   sidebarPanel(
                                     fileInput("file","Dataset", multiple = TRUE, accept = c(".txt",
                                                                                             "text/csv",
                                                                                             "text/comma-separated-values,text/plain",
                                                                                             ".csv")),
                                     helpText("Maximum file size 20MB"),
                                     tableOutput("Mensaje"),
                                     tags$hr(),
                                     checkboxInput('edges', 'Optional: upload edges file', value = FALSE),
                                     uiOutput('bordesFile'),
                                     tags$hr(),
                                     h5("Reading file options:"),
                                     checkboxInput('header', 'The file contains the names of the variables as its first line.', value = TRUE),
                                     br(),
                                     uiOutput("SepData")
                                     # radioButtons('sep', 'Separator character', choices = MyChoiceSepdata, selected = MyChoiceSepdata[1])
                                     ,wellPanel(uiOutput("BaseColx", inline = TRUE) 
                                                ,uiOutput("BaseColy", inline = TRUE)
                                                ,uiOutput("BaseColRend", inline = TRUE)
                                                ,uiOutput("TextoAviso", inline = TRUE))
                                     ,wellPanel(uiOutput("Hemisf", inline = TRUE)
                                                ,uiOutput("Zona", inline = TRUE))
                                     
                                   ),
                                   mainPanel(
                                     uiOutput("tb")
                                   )
                                   
                                 )),
                        
                        
                        tabPanel("Depuration",
                                 
                                 checkboxGroupInput(inputId="AutomaticDep", label=h4("Data depuration"), choices = c(Automatic="Automatico"), inline = TRUE, selected="Automatic"),
                                 conditionalPanel(condition = "input.AutomaticDep != 'Automatico' ",
                                                  checkboxGroupInput("mDepuration", h4("Methods"), choices = c("Glogal Outliers"="Outliers", "Spatial Outliers"="Inliers"), inline = TRUE),
                                                  
                                                  column(width =4,
                                                         conditionalPanel(
                                                           condition = "input.mDepuration.indexOf('Outliers') > -1",
                                                           h4("Global Outliers Options:"),
                                                           helpText("Upper and lower threshold boundaries to constrain data within a range of realistic values"),
                                                           bootstrapPage(
                                                             column(width =4,div(
                                                               style="display:inline-block", numericInput(inputId="ylimitmin", label="Min", value = 0, width = "100%"))),
                                                             column(width =4,div(
                                                               style="display:inline-block", numericInput(inputId="ylimitmax", label="Max", value = NA, width = "100%")))
                                                             ,tags$br()
                                                             ,tags$br()
                                                             ,tags$br()
                                                             ,tags$br()
                                                           ),
                                                           # h4("Standard deviation:"),
                                                           helpText("Removes all data points which are more than N times the standard deviation from the mean value"),
                                                           bootstrapPage(
                                                             column(width =5,div(
                                                               style="display:inline-block", numericInput(inputId="DEOut", label="Standard deviation", value = 3, width = "100%"))))
                                                         )),
                                                  
                                                  column(width = 4,
                                                         conditionalPanel(
                                                           condition = "input.mDepuration.indexOf('Inliers') > -1",
                                                           h4("Spatial Outliers Options:"),
                                                           helpText("Search radius to identify a local neighborhood for each data point"),
                                                           
                                                           column(width =4,div(
                                                             style="display:inline-block", numericInput(inputId="ValorVecindario", label="Neighbors", value = 0.0, width = "100%"))),
                                                           
                                                           bootstrapPage(
                                                             column(width =4,
                                                                    div(
                                                                      style="display:inline-block", numericInput(inputId="VecinosMin", label="Min. distance", value = 0, width = "100%"))),
                                                             column(width =4,div(
                                                               style="display:inline-block", numericInput(inputId="VecinosMax", label="Max. distance", value = 20, width = "100%")))
                                                             ,tags$br()
                                                             ,tags$br()
                                                             ,tags$br()
                                                             ,tags$br()
                                                             
                                                           )
                                                         )
                                                  )
                                                  
                                                  ,column(width =4,
                                                          conditionalPanel(
                                                            condition = "input.mDepuration.indexOf('Inliers') > -1 || input.mDepuration.indexOf('Outliers') > -1",
                                                            h4("Border Effects:"),
                                                            helpText("Remove data points for a given distance from field edges"),
                                                            bootstrapPage(
                                                              column(width =4,div(
                                                                style="display:inline-block", checkboxInput(inputId="SacoBordes", label="Remove borders", value = 0.0, width = "100%"))),
                                                              
                                                              conditionalPanel(
                                                                condition = "input.SacoBordes",
                                                                column(width =4,div(
                                                                  style="display:inline-block", numericInput(inputId="Buffer", label="Buffer", value = -20, width = "100%"))))
                                                              ,tags$br()
                                                              ,tags$br()
                                                              ,tags$br()
                                                              ,tags$br()
                                                              
                                                            )
                                                          )
                                                  )
                                 )
                        ),
                        
                        
                        tabPanel("Adjustments",
                                 
                                 column(width = 2,titlePanel(h4("Fitted models")),
                                        checkboxInput('bar', 'Automatic',value=T),
                                        uiOutput("ModelosA")
                                 ),
                                 
                                 column(width = 4,
                                        radioButtons("tKriging", h4("Methods"), choices = list("Ordinary"=1, "Universal Kriging (First Order)" = 2, "Universal Kriging (Second Order)" = 3), inline = F),
                                        h4("Kriging options:"),
                                        helpText("Local neighbourhood selections based on distance as radius (Max.Dist), number of data points (Max, Min), of nearest site of the target point"),
                                        bootstrapPage(
                                          column(width =4,
                                                 div(
                                                   style="display:inline-block", numericInput(inputId="nmin", label="Min. n", value =  7, width = "80%"))),
                                          column(width =4,div(
                                            style="display:inline-block", numericInput(inputId="nmax", label="Max. n", value = 25, width = "80%")))
                                          
                                          ,  column(width =4,
                                                    div(style="display:inline-block",
                                                        numericInput(inputId="distmax", label="Max. Dist.", value =  1000, width = "80%"))),
                                          column(width =4,div(style="display:inline-block",
                                                              numericInput(inputId="block", label="Block", value = 0, width = "80%")))
                                          
                                        )),
                                 
                                 
                                 column(width = 6,
                                        h4("Prediction Options"),
                                        bootstrapPage(
                                          column(width =3,
                                                 div(style="display:inline-block",
                                                     numericInput("dimGrilla","Grid dimentions",value= 10, min=0, width = "100%"))),
                                          # column(width =3,
                                          #        div(style="display:inline-block",
                                          #            selectInput("hemisferio", "Hemisphere", choices = list("North" = 1, "South" = 2), selected = 2, width = "100%"))),
                                          # column(width =3,
                                          #        div(style="display:inline-block",
                                          #            numericInput("zona","Area", 20 ,width = "100%", min = 1, max = 60, step = 1))),
                                          tags$br(),
                                          tags$br(),
                                          tags$br(),
                                          tags$br(),
                                          tags$br(),
                                          h4("Prediction scale"),
                                          column(width =2,
                                                 div(style="display:inline-block",
                                                     numericInput("min","Min.", NULL,width = "100%"))),
                                          
                                          column(width =2,
                                                 div(style="display:inline-block",
                                                     numericInput("max","Max.", NULL, width = "100%"))),
                                          tags$br(),
                                          tags$br(),
                                          tags$br(),
                                          tags$br(),
                                          tags$br(),
                                          h4("Predicted variance scale"),
                                          column(width =2,
                                                 div(style="display:inline-block",
                                                     numericInput("min_var","Min.", min=0, NULL,width = "100%"))),
                                          column(width =2,
                                                 div(style="display:inline-block",
                                                     numericInput("max_var","Max.", NULL, width = "100%")))
                                        )
                                 )
                        )
                        
                        ,tabPanel("Results",
                                  uiOutput("resultados"))
                        
                        ,tabPanel(title =  textOutput("tabPanel_title"), #"Cluster",#
                                  # uiOutput("Clasificador")
                                  tabsetPanel(
                                    tabPanel("Parameters for KM-sPC classification",
                                             column(width = 12/4,
                                                    checkboxInput("centrado", "Centered", value = TRUE),
                                                    checkboxInput("vecindarionulo", "Data with null neighbor", value = FALSE),
                                                    radioButtons("distancia", "Distance", choices= c("Euclidean"= "euclidean", "Manhattan"="manhattan"))
                                             ),
                                             column(width = 12/4,
                                                    sliderInput("clusters","Number of Cluster to evaluate", min=2, max=15, value = c(2,6)),
                                                    numericInput("iteraciones","Iterations", value=1000, min=1, step=10),
                                                    numericInput("ExpDif", "Degree of fuzzification", value=1.3, step=0.05)
                                             ),
                                             column(width = 12/4,
                                                    h3("Neighborhood network"),
                                                    sliderInput("distanciavecino","Distance between neighbors", min=0, max=1000, value = c(0,35)),
                                                    sliderInput("varexplicada", "Explained variance (%)", min=0, max=100, value = 70)
                                                    
                                                    
                                             )), 
                                    tabPanel("Classification results",
                                             fluidPage(
                                               fluidRow(column( width = 8,dataTableOutput("TablaIndicesConglo")) ),##
                                               fluidRow(column( width = 6,dataTableOutput("TablaResultadosConglom"))  ) )
                                    ),
                                    tabPanel("Cluster Plot",
                                             fluidPage(
                                               column(width = 12/4,
                                                      fluidPage(uiOutput("SelectorCong"))),
                                               column(width = 12-12/4,
                                                      fluidPage(plotlyOutput('ClasificationPlot', height = "600px")))))
                                    
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
                        
                        ,tabPanel("Report",
                                  p("This section is being developed. It may contain problems when the report is generated. Please, if it happens, contact", a("pablopaccioretti@agro.unc.edu.ar", href = "mailto:pablopaccioretti@agro.unc.edu.ar")),
                                  radioButtons('format', 'Document format','HTML',  #c('PDF', 'HTML', 'Word'),
                                               inline = TRUE),
                                  downloadButton("report", "Generate report"))
                        
                        
                      )
            )
  )
)


