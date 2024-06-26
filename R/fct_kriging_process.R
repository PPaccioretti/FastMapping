#' kriging_process 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

removeSpatialDuplicated <- function(file, 
                                    process = 'cross-validation',
                                    session = session) {
  completefile <- file

  if(inherits(file, "sf")) {
    removedfile <- sf:::distinct.sf(completefile, 
                                    geometry,
                                    .keep_all = TRUE)
  } else {removedfile <- sp::remove.duplicates(file)}

  if (nrow(completefile) > nrow(removedfile)) {
    difRow <- nrow(completefile) - nrow(removedfile)
    if (difRow == 1) {
      mensajeElim <-
        paste0(
          "To perform ", process, ',',
          difRow,
          " point pair with equal spatial coordinate was removed"
        )
    } else {
      mensajeElim <-
        paste0(
          "To perform ", process, ',',
          difRow,
          " point pairs with equal spatial coordinates were removed"
        )
    }
    
    showNotification(mensajeElim ,
                     type = "default",
                     duration = 3,
                     session = session)
  }
  removedfile
}



testMultipleModelsKrige <- function(formula,
                                    file,
                                    selectedModels,
                                    nmax, 
                                    nmin, 
                                    max_dist,
                                    cressie,
                                    session = session) {
  withProgress(message = 'Cross validation:',
               detail = 'This may take a while...',
               value = 0,
               session = session,
               {
                 
                 myformula <- stats::as.formula(formula)
                 myDataNoDup <- removeSpatialDuplicated(file, 
                                                        session = session)
                 modelsSelected <- selectedModels
                 
                 MyMod = list()
                 for (model in modelsSelected) {
                   # autoKrige.cv command does not take in account the 
                   # blocks of your data. It performs the cross-validation 
                   # point-by-point and not by blocks.
                   #
                   # Cross validation takes in account the accuracy of the 
                   # estimates of the interpolation (or prediction) for 
                   # POINTS while block kriging is a smoothing method that 
                   # divides the whole area into several BLOCKS and 
                   # calculate the local average of your estimations 
                   # for each of those area. In other words, for the 
                   # area 'block' you don't have a 'value' to compare 
                   # your estimation made by kriging
                   
                   
                   MyAK = tryCatch({
                     automap::autoKrige.cv(
                       myformula,
                       myDataNoDup,
                       model = model,
                       nfold = 10,
                       nmax = nmax,
                       nmin = nmin,
                       maxdist = max_dist,
                       miscFitOptions = list(cressie = cressie)
                     )
                   },
                   error = function(e)
                     print(e))
                   
                   MyMod[[model]] = MyAK
                   incProgress(1 / length(modelsSelected),
                               detail = paste("Validating",
                                              names(modelsSelected)[modelsSelected == model],
                                              "model"),
                               session = session)
                   
                 }
                 
                 
                 #Esta funcion si no puede usar la funcion compare.cv, calcula 
                 # el RMSE a mano y lo repite 11 vecces
                 ModList = lapply(MyMod, function(x) {
                   tryCatch({
                     automap::compare.cv(x)},
                     error = function(e) {
                       cat("Calculating CV by hand\n")
                       
                       showNotification(
                         paste("Something went wrong while cross-validation"),
                         type = "warning",
                         duration = 5,
                         id = session$ns("Aviso"),
                         session = session
                       )
                       
                       if (any(c("simpleError", "error", "condition") %in% class(e))) {
                         return(matrix(NA, nrow = length(modelsSelected)))
                       }
                       
                       matrix(sqrt(
                         sum(x$krige.cv_output$residual ^ 2, na.rm = TRUE) / 
                           sum(stats::complete.cases(x$krige.cv_output$residual))
                       ), nrow = length(modelsSelected))
                     }
                   )})
                 modelsTested = do.call("cbind", ModList)
                 colnames(modelsTested) = modelsSelected
                 # ValoresOutput$MiKrige <- modelsTested
                 return(modelsTested)
               })
}