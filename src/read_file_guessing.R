#' Read file guessing the extension. Used for Shiny applications.
#'
#' @description Try to read files guessing the extensions. 
#'   If cant read, an error arise. 
#'
#' @param datapath  \code{character} vector of length one specifying the file 
#'   datapath
#' @param name \code{character} vector of length one specifying the file 
#'   name
#'
#' @return an object of different class depending the function used to read the file
#' @export
#'
#' @examples
read_file_guessing <- function(datapath, name) {
  
  # datapath <- upload$datapath
  # name <- upload$name
  
  ext <- tolower(tools::file_ext(datapath))
  
  if (length(ext) > 1 &
      !all(c("shp", "shx", "dbf") %in% ext)) {
    shiny::showNotification(
      paste(
        "Multiple files were uploaded,",
        "this is only valid for shp files",
        "with at least shp, dbf AND shx"
      ),
      type = 'error'
    )
    shiny::validate("Invalid files uploaded. Only one file is allowed but for shp files.")
    
  }
  
  if (length(ext) == 1 &&
      ext %in% c("txt", "csv")) {
    myData <- tryCatch({
      data.table::fread(
        datapath,
        dec = ".",
        data.table = FALSE,
        check.names = TRUE
      )
    },
    error = function(e) {
      data.table::fread(
        datapath,
        dec = ".",
        data.table = FALSE,
        check.names = TRUE
      )
    },
    error = function(e) {
      shiny::showNotification(paste("Invalid file format, please check!"),
                              type = 'error')
      return(NULL)
    })
    if (is.null(myData)) {
      shiny::validate(
        paste(
          "Something went wrong while reaading file.\n",
          "Invalid file format, please check!"
        )
      )
    }
  }
  
  if (length(ext) == 1 &&
      ext %in% c("xls", "xlsx")) {
    myData <-  readxl::read_excel(datapath,
                                  .name_repair = make.names)
    
  }
  
  if (!any(ext %in% c("xls", "xlsx", "txt", "csv"))) {
    myData <-   tryCatch({
      # If multiple files were uploaded change the names to match all files with shp.
      if (length(ext) > 1) {
        from <- normalizePath(datapath)
        to <- file.path(dirname(from), basename(name))
        to <- normalizePath(to)
        file.rename(from, to)
        sf::read_sf(unique(dirname(from)))
      } else {
        sf::read_sf(datapath)
      }
    },
    error = function(e) {
      #Last check before arise an error
      supressWarnings(myData <- data.table::fread(
        datapath,
        data.table = FALSE,
        check.names = TRUE
      ))
    },
    # error = function(e) {
    #   stars::read_stars(datapath)
    # },
    error = function(e) {
      shiny::validate("Invalid file type.")
    })
  }
  myData
}

print_sf_as_df <- function(sf_data) {
  sf_data <- sf_to_point(sf_data)
  df_data <- data.frame(sf::st_coordinates(sf_data),
                        sf::st_drop_geometry(sf_data))
  geometry <- sf::st_geometry(sf_data)
  
  list(data = df_data,
       geometry = geometry)
}


sf_to_point <- function(sf_data) {
  if (all(sf::st_geometry_type(sf_data) == "POINT")) {
    return(sf_data)
  }
  
  if (any(sf::st_geometry_type(sf_data) != "POINT")) {
    shiny::showNotification(
      paste("Centroid of Polygons are shown as coordinates"),
      type = 'warning',
      id = "warning_centroid"
    )
    sf_data <- suppressWarnings(sf::st_centroid(sf_data))
    return(sf_data)
  }
  
}




# 
# 
# library(shiny)
ui <- fluidPage(
  tabPanel("Dataset",
           value = "DatasetTab",
           sidebarLayout(
             sidebarPanel(
               fileInput(
                 "database_upload",
                 label = h4(
                   "Dataset "
                 ),
                 multiple = TRUE
               ),
               numericInput("n", "Rows", value = 5, min = 1, step = 1),
               # helpText("Maximum file size 20MB"),
               tableOutput("Mensaje"),
               checkboxInput('boundary', 
                             'Optional: upload boundary file', 
                             value = FALSE),
               uiOutput("ui_data_param")
               
             ),
             mainPanel(tableOutput("head"))
             
           ))

  
)
# 
server <- function(input, output, session) {
  data <- reactive({
    req(input$database_upload)
    read_file_guessing(input$database_upload$datapath, input$database_upload$name)
  })

  data_print <- reactive({
    req(input$database_upload)
    myData <- data()
    if (inherits(myData, "sf")) {
      myData <- print_sf_as_df(myData)$data
    }
    myData
  })

  output$head <- renderTable({
    head(data_print(), input$n)
  })
  
  
  output$tabpanel_data <- renderUI({
    validate(need(data(), ''))
    
    return(
      tabsetPanel(
        tabPanel("Data", DT::dataTableOutput("table")),
        tabPanel("Edges", 
                 conditionalPanel(
                   condition = "input.rto != undefined && input.rto.length < 2",    
                   fluidPage(
                     DT::dataTableOutput("edgesTable"),
                     uiOutput("ui_edge_param")
                   )
                 )
        )
        
      ))
    
  })
  
  output$ui_data_param <- renderUI({
    validate(need(data(), ""))
    tagList(
      hr(),
      h5("Reading file options:"),
    
      wellPanel(
        uiOutput("BaseColx", inline = TRUE),
        uiOutput("BaseColy", inline = TRUE),
        uiOutput("BaseColRend", inline = TRUE),
        uiOutput("TextoAviso", inline = TRUE)
      ),
      br(),
      uiOutput("ui_epsg"),
      uiOutput("ui_utm_param")
    )
  })
  
  
}

# 
shiny::runApp(list(ui = ui, server = server))
