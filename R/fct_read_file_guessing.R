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
#' @export
#'
#' @examples
read_file_guessing <- function(datapath, name, session = session) {
  
  # datapath <- upload$datapath
  # name <- upload$name
  
  ext <- tolower(tools::file_ext(datapath))
  
  if (length(ext) > 1 &
      !all(c("shp", "shx", "dbf") %in% ext)) {
    if (!is.na(session)) {
      ns <- session$ns
    } 
    
    shiny::showNotification(
      paste(
        "Multiple files were uploaded,",
        "this is only valid for shp files",
        "with at least shp, dbf AND shx"
      ),
      type = 'error',
      id = ns("multiple-files")
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
        dec = ",",
        data.table = FALSE,
        check.names = TRUE
      )
    },
    error = function(e) {
      shiny::showNotification(paste("Invalid file format, please check!"),
                              type = 'error',
                              id = ns("invalid-files"))
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
    myData <- data.frame(myData)
    
  }
  
  if (!any(ext %in% c("xls", "xlsx", "txt", "csv"))) {
    myData <-   tryCatch({
      # If multiple files were uploaded change the names to match all files with shp.
      if (length(ext) > 1) {
        suppressWarnings(from <- normalizePath(datapath))
        to <- file.path(dirname(from), basename(name))
        to <- normalizePath(to)
        file.rename(from, to)
        sf::read_sf(unique(dirname(from)), 
                    as_tibble = FALSE)
      } else {
        sf::read_sf(datapath, 
                    as_tibble = FALSE)
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
