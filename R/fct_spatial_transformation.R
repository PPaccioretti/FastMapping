#' spatial_transformation 
#'
#' @param dataset 
#' @param coords 
#' @param orgn_epsg 
#' @param tgt_epsg 
#'
#' @description Create an sf drom data and coord and epgs
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
 
spatial_transformation <-
  function(dataset, coords, orgn_epsg = NULL, tgt_epsg) {
    # If is not sf
    if (!inherits(dataset, "sf")) {
      if (is.null(orgn_epsg)) {
        stop("Original CRS must be specified")
      }
      if (any(coords == "") | any(is.na(coords))) {
        stop("Both coords must be specified")
      }
      if (length(unique(coords)) == 1) {
        stop("Coords must be different columns")
      }
      dataset <- sf::st_as_sf(dataset, coords = coords,
                              crs = orgn_epsg)
    } 
    # If its sf
    if (inherits(dataset, "sf")) {
      
      if (is.na(sf::st_crs(dataset)) & is.null(orgn_epsg)) {
        stop("Original CRS must be specified")
      }
      
      if (is.na(sf::st_crs(dataset))) {
        dataset <- sf::st_zm(dataset)
        dataset <- sf::st_set_crs(dataset, orgn_epsg)
      }
    }
    
    # After add crs or is sf do:
    if (isTRUE(sf::st_crs(dataset) != sf::st_crs(tgt_epsg))) {
      req(inherits(dataset, "sf"))
      sf::st_transform(dataset, tgt_epsg)
    } else {
      dataset
      }
    
  }


test_latlong <- function(epsg) {
  isTRUE(sf::st_is_longlat(sf::st_crs(epsg)))
}



guess_utm <- function(data) {
  # Adivina la zona UTM ----
  long <- sf::st_coordinates(data)[, 'X']
  ## Se calcula el quatile por si cae en dos zonas
  zone <- stats::quantile(floor((long + 180) / 6) + 1, 0.90)
  
  long <- sf::st_coordinates(data)[, 'Y']
  hemisphere <- '7'
  if (all(long > 0)) {
    hemisphere <- '6'
  }
  
  epsg <- as.numeric(paste0("32", hemisphere, zone))
  epsg
}
