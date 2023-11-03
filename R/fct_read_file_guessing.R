#' Read file guessing the extension. Used for Shiny applications.
#'
#' @description Try to read files guessing the extensions.
#'   If cant read, an error arise.
#'
#' @param datapath  \code{character} specifying the file/s datapath
#' @param name \code{character} vector specifying the file/s name
#' @param session Used for shiny
#' 
#' @return an object of different class depending the function used to read the file
#'
read_file_guessing <- function(datapath, name, session = session) {
  
  # datapath <- upload$datapath
  # name <- upload$name
  ext <- tolower(tools::file_ext(datapath))
  if (is.function(session[['ns']])) {
    ns <- session$ns
  }
  
  if (length(ext) == 1 && ext %in% c("shp", "shx", "dbf")) {
    shiny::showNotification(
      paste(
        "Only a file with extension", ext, 
        "was uploaded. This file is associated with",
        "shp files, please upload all associated files.",
        "(At LEAST shp, dbf, AND shx)"
      ),
      type = 'error',
      id = ns("sigle-shp-file")
    )
    shiny::validate("Invalid file uploaded. For upload shp files, please select at least 'shp', 'shx' AND 'dbf' files.")
  }
  
  if (length(ext) > 1) {
    if (!all(c("shp", "shx", "dbf") %in% ext)) {
      shiny::showNotification(
        paste(
          "Multiple files were uploaded,",
          "this is only valid for shp files",
          "with at least shp, dbf AND shx"
        ),
        type = 'error',
        id = ns("multiple-files")
      )
      shiny::validate("Invalid files uploaded. Only one file is allowed but for shp files, where at least 'shp', 'shx', 'dbf' files are needed.")
      
    }
    if (all(c("shp", "shx", "dbf") %in% ext & !'prj' %in% ext)) {
      shiny::showNotification(
        paste(
          "Multiple files were uploaded,",
          "but non prj file is present.",
          "You will have to add manually origin and target EPSG code."
        ),
        duration = 10,
        type = 'warning',
        id = ns("no-prj")
      )
    }
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
        dec = ",",
        data.table = FALSE,
        check.names = TRUE
      )
    },
    error = function(e) {
      data.table::fread(
        datapath,
        dec = ",",
        data.table = FALSE,
        check.names = TRUE,
        encoding = 'Latin-1'
      )
    },
    error = function(e) {
      shiny::showNotification(paste("Invalid file format, please check!"),
                              type = 'error',
                              id = ns("invalid-files"))
      return(NULL)
    })

    if (is.null(myData)) {
      
      text_cvs <- ext %in% c("txt", "csv")
      if (text_cvs) {
        text_comma <- "Be careful with comma as decimal separator if csv was uploaded"
      } else {
        text_comma <- ''
      }
      shiny::validate(
        paste0(
          "Something went wrong while reading file.\n",
          "Invalid file format, please check!\n",
          text_comma
        )
      )
    }
  }
  
  if (length(ext) == 1 &&
      ext %in% c("xls", "xlsx")) {
    myData <-  readxl::read_excel(datapath,
                                  .name_repair = make.names)
    myData <- data.frame(myData)
    
  }
  
  if (!any(ext %in% c("xls", "xlsx", "txt", "csv"))) {
    myData <-   tryCatch({
      # If multiple files were uploaded change the names to match all files with shp.
      if (length(ext) > 1) {
        suppressWarnings(from <- normalizePath(datapath))
        to <- file.path(dirname(from), basename(name))
        suppressWarnings(to <- normalizePath(to))
        file.rename(from, to)
        myData_sf <- sf::read_sf(unique(dirname(from)), 
                                 as_tibble = FALSE)
        myData_sf <- sf::st_zm(myData_sf)
        myData_sf
      } else {
        myData_sf <- sf::read_sf(datapath, 
                                 as_tibble = FALSE)
        
        myData_sf <- sf::st_zm(myData_sf)
        myData_sf
      }
    },
    error = function(e) {
      #Last check before arise an error
      suppressWarnings(myData <- data.table::fread(
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
