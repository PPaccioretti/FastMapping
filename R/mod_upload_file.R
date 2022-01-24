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
#'
#' @keywords internal
#' @export
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
#' @export
#' @keywords internal

mod_upload_file_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    myDf <- reactive({
      req(input$database_upload)
      myTable <- read_file_guessing(input$database_upload$datapath,
                                    input$database_upload$name)
      myTable
    })
  })
}
