# Module UI


#' @title mod_upload_file_ui and mod_upload_file_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_upload_file_module
#' @noRd
#'
#' @keywords internal
#' @importFrom shiny NS tagList
mod_upload_file_ui <-
  function(id,
           label = h4("Dataset "),
           multiple = TRUE) {
    ns <- NS(id)
    tagList(fileInput(
      ns("database_upload"),
      label = label,
      multiple = multiple
    ))
  }

# Module Server

#' @rdname mod_upload_file_module
#' @noRd
#' @keywords internal

mod_upload_file_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    myDf <- reactive({
      req(input$database_upload)
      id <-
        showNotification(loadingText("Reading data..."),
                         duration = NULL,
                         closeButton = FALSE)
      on.exit(removeNotification(id), add = TRUE)
      myTable <- read_file_guessing(input$database_upload$datapath,
                                    input$database_upload$name)
      myTable
    })
  })
}
