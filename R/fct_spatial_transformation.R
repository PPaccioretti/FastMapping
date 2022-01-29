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
      dataset <-   sf::st_as_sf(dataset, coords = coords,
                                crs = orgn_epsg)
    } 
    # If its sf
    if (inherits(dataset, "sf")) {
      
      if (is.na(sf::st_crs(dataset)) & is.null(orgn_epsg)) {
        stop("Original CRS must be specified")
      }
      
      if (is.na(sf::st_crs(dataset))) {
        dataset <- sf::st_crs(dataset, orign_epsg)
      }
    }
    
    # After add crs or be sf do:
    if (isTRUE(sf::st_crs(dataset) != sf::st_crs(tgt_epsg))) {
      req(inherits(dataset, "sf"))
      sf::st_transform(dataset, tgt_epsg)
    } else {
      dataset
      }
    
  }


test_latlong <- function(epsg) {
  suppressWarnings(sf::st_is_longlat(sf::st_crs(epsg)))
}    
