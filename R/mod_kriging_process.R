#' kriging_process UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_kriging_process_ui <- function(id){
  ns <- NS(id)
  tagList(
    # tableOutput(ns("TModel"))
  )
}
    
#' kriging_process Server Functions
#'
#' @noRd 
mod_kriging_process_server <- function(id,
                                       dataset,
                                       kriging_param,
                                       boundary_poly,
                                       button) {
  
  moduleServer( id, function(input, output, session) {
    ns <- session$ns

    
    MiKrige <- reactive({
      req(dataset())
      req(kriging_param())

      file <- dataset()
      
      if (nrow(file) > 20000) {
        file <- file[sample(nrow(file), 20000), ]
      }
      
      file <- sf::as_Spatial(file)
      myParam <- kriging_param()
      
      testMultipleModelsKrige(myParam$formula,
                              file,
                              myParam$selectedModels,
                              myParam$nmax, 
                              myParam$nmin, 
                              myParam$max_dist,
                              myParam$cressie,
                              session = session)
      
    })
    
    #
    MejorModelo <- reactive({

      ValidationTable = MiKrige()
      tryCatch({
        MyBestModels = names(which.min(apply(ValidationTable[8,], 2, as.numeric)))
        return(MyBestModels)
      }, error = function(e) {
        MyBestModels = colnames(ValidationTable)[1]
        return(MyBestModels)
      })
    })
    
    variablesForVariogramPlot <- reactive({
      myKrige <- MiKrige()
      
      # req(MiKrige())
      # req(MejorModelo())
      # req(variogram())
      # req(kriging_param())

      myParam <- kriging_param()
      req(myParam$myTgtVar)
      myKrige <- MiKrige()
      myBestModel <- MejorModelo()
      myVariogram <- variogram()
      
      mo <- cbind(myVariogram$var_model)
      nug <- mo[1, 2]
      mod <- cbind(myVariogram$var_model[, 1:4], myVariogram$sserr)
      names(mod) = c("Model", "Parcial Sill", "Range", "Kappa", "SCE")
      Nugget <- mod[1, 2]
      Modelo <- mod[2, ]
      if (Modelo$Kappa == 0.5) {
        Modelo <- mod[2, -4]
      }
      Modelo <- cbind(Modelo, Nugget)
      
      row.names(Modelo) = NULL
      RMSE = myKrige[8, myBestModel][[1]]
      RMSEp = RMSE / mean(dataset()[[myParam$myTgtVar]], na.rm = TRUE) * 100
      
      
      Modelo = data.frame(
        Modelo,
        "RMSE" = RMSE,
        "Error (%)" = RMSEp,
        check.names = FALSE
      )
      suppressWarnings({
        Parametros <- paste(
          c("Model", paste(utils::stack(Modelo[-ncol(Modelo)])[, 2]), "Error (%)"),
          c(as.character(Modelo[1, 1]), paste(round(
            utils::stack(Modelo)[, 1], 1
          ))),
          sep = ": ",
          collapse = "\n"
        )
      })
      
      VariogramData <-
        gstat::variogramLine(myVariogram$var_model, max(myVariogram$exp_var$dist))
      myVariogram <- myVariogram$exp_var
      
      list(
        'variogramline' = VariogramData,
        'variogramPoint' = myVariogram,
        'modelSummary' = Modelo,
        'parametros' = Parametros
      )
    })
    
    variogram <- reactive({
      # req(dataset())
      # req(kriging_param())
      # req(MejorModelo())

      file <- dataset()
      file <- sf::as_Spatial(file)
      
      myParam <- kriging_param()
      varioagramModel <- 
        automap::autofitVariogram(
          myParam$formula,
          file,
          model = MejorModelo(),
          cutoff = 10000,
          cressie = myParam$cressie
        )
      
      # ValoresOutput$Variograma <- varioagramModel
      varioagramModel
    })
    
    
    Mygr <- reactive({
      req(dataset())
      req(kriging_param())
      req(boundary_poly())

      file <- dataset()
      myParam <- kriging_param()
      sf::st_bbox(file) %>%
        stars::st_as_stars(dx = myParam$cellsize) %>%
        sf::st_crop(boundary_poly())
    })
    
    
    kriging <- eventReactive(button(),{
      req(dataset())
      req(kriging_param())
      req(Mygr())
      

      file <- dataset()
      myParam <- kriging_param()
      Mygr <- Mygr()
      Modelo <- MejorModelo()
      
      gstat::krige(
        formula = myParam$formula,
        locations = file,
        newdata = Mygr,
        variogram()$var_model,
        nmax = myParam$nmax,
        nmin = myParam$nmin,
        maxdist = myParam$max_dist,
        block = c(myParam$block, myParam$block)
      )
      # ValoresOutput$kriging <- krigingfit$krige_output
    })
    
    # output$TModel <- renderTable({
    #   req(MiKrige())
    #   miKrige <- as.data.frame(MiKrige())
    # })
    
    
    list(
      kriging = kriging,
      grid = Mygr,
      variogram = variogram,
      allModels = MiKrige,
      variablesForVariogramPlot = variablesForVariogramPlot
    )
  })
}
    
## To be copied in the UI
# mod_kriging_process_ui("kriging_process_ui_1")
    
## To be copied in the server
# mod_kriging_process_server("kriging_process_ui_1")
