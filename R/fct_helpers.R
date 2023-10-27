#' helpers
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
print_sf_as_df <-
  function(sf_data,
           toPoint = TRUE,
           addCoords = FALSE,
           session) {
    if (toPoint) {
      sf_data <- sf_to_point(sf_data, session)
    }
    
    if (addCoords) {
      df_data <- data.frame(sf::st_drop_geometry(sf_data),
                            sf::st_coordinates(sf_data))
    } else {
      df_data <- data.frame(sf::st_drop_geometry(sf_data))
    }
    geometry <- sf::st_geometry(sf_data)
    
    list(data = df_data,
         geometry = geometry)
  }


sf_to_point <- function(sf_data, session) {
  if (all(sf::st_geometry_type(sf_data) == "POINT")) {
    return(sf_data)
  }
  
  if (any(sf::st_geometry_type(sf_data) != "POINT")) {
    if (!missing(session)) {
      ns <- session$ns
    } else {
      ns <- function(x) {
        x
      }
    }
   if (has_sf_polygon(sf_data)) {
     shiny::showNotification(
       paste("Centroid of Polygons are shown as coordinates"),
       type = 'warning',
       id = ns("warning_centroid")
     )
     sf_data <- suppressWarnings(sf::st_centroid(sf_data))
   }
    return(sf_data)
  }
  
}

select_sf_points <- function(sf_data) {
  sf_data[sf::st_geometry_type(sf_data) == "POINT",]
}

select_sf_multipoints <- function(sf_data) {
  sf_data[sf::st_geometry_type(sf_data) == "MULTIPOINT",]
}
select_sf_polygon <- function(sf_data) {
  sf_data[sf::st_geometry_type(sf_data) == "POLYGON",]
}

has_sf_points <- function(sf_data) {
 any(sf::st_geometry_type(sf_data) == "POINT")
}

has_sf_multipoints <- function(sf_data) {
  any(sf::st_geometry_type(sf_data) == "MULTIPOINT")
}


has_sf_polygon <- function(sf_data) {
  any(sf::st_geometry_type(sf_data) == "POLYGON")
}


find_vars <- function(data, filter) {
  names(data)[vapply(data, filter, logical(1))]
}


firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

modelsVariogram <- function() {
  myLabels <- regmatches(
  gstat::vgm()$long,
  gregexpr("(?<=\\().*?(?=\\))", gstat::vgm()$long, perl = T))
  
  myModels <- as.list(as.character(gstat::vgm()$short))
  myLabels <- lapply(myLabels, firstup)
  names(myModels) <- myLabels
  myModels
}



makePlotClusterValid <- function(data) {
  myNames <- colnames(data)
  ggplot2::ggplot(data,
                  ggplot2::aes(x = .data[[myNames[1]]], y = .data[[myNames[2]]])) +
    ggplot2::geom_col(width = 0.25) +
    ggplot2::geom_text(ggplot2::aes(label = .data[[myNames[4]]],  vjust = -0.5))
  
  
}


loadingText <- function(text) {
      div(
        div(style = "display: inline-block;vertical-align:center; ",
            p(text)),
        div(
          style = "display: inline-block;vertical-align:center; ",
          div(class = "cssload-loader",
              div(),
              div(),
              div(),
              div(),
              div())
        )
      )
  
}



column_md <- function(width, ..., offset = 0) 
{
  if (!is.numeric(width) || (width < 1) || (width > 12)) 
    stop("column width must be between 1 and 12")
  colClass <- paste0("col-md-", width)
  if (offset > 0) {
    colClass <- paste0(colClass, " offset-md-", offset, " col-sm-offset-", 
                       offset)
  }
  div(class = colClass, ...)
}
