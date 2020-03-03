# FastMapping
## A Shinny App for spatial analysis
FastMapping implements statistical tools for spatial data depuration, quasi automatic variogram fitting and select the 
best geostatistical model for kriging prediction by crossvalidation. As aid of result interpretation, the program allows 
for prediction error statistics to be calculated and visualized graphically. Input databases, should be composed of 
bidimensional coordinates and the variable to be mapped.


You can run the Application locally in R:


```r

list_of_packages <- c(
  "shiny",
  "shinythemes",
  "shinyBS",
  "shinyjs",
  "shinycssloaders",
  "waiter",
  "DT",
  "data.table",
  "dplyr",
  "ggplot2",
  "plotly",
  "GGally",
  "RColorBrewer",
  "cowplot",
  "knitr",
  "rmarkdown",
  "V8",
  "geoR",
  "automap",
  "fields",
  "raster",
  "sp",
  "rgeos",
  "gstat",
  "e1071",
  "spdep",
  "ade4",
  "adespatial"
)


invisible(lapply(list_of_packages,
                 function(x)
                   if (!require(x, character.only = TRUE))
                   {
                     install.packages(x)
                     library(x, character.only = TRUE)
                   }))

shiny::runGitHub("FastMapping", "PPaccioretti")

```
