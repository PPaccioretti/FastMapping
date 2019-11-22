# FastMapping
## A Shinny App for spatial analysis
FastMapping implements statistical tools for spatial data depuration, quasi automatic variogram fitting and select the 
best geostatistical model for kriging prediction by crossvalidation. As aid of result interpretation, the program allows 
for prediction error statistics to be calculated and visualized graphically. Input databases, should be composed of 
bidimensional coordinates and the variable to be mapped.


You can run the Application locally in R:


```r
list_of_packages <- c("shiny",
                      "shinythemes",                    
                      "geoR",
                      "automap",
                      "fields",
                      "raster",
                      "sp",
                      "ggplot2",
                      "rgeos",
                      "gstat",
                      "e1071",
                      "spdep",
                      "DT",
                      "ade4",
                      "V8",
                      "knitr",
                      "rmarkdown",
                      "plotly", 
                      "data.table",
                      "RColorBrewer",
                      "cowplot")


invisible(lapply(list_of_packages,
                 function(x) if(!require(x,character.only = TRUE)) install.packages(x)))

shiny::runGitHub( "FastMapping", "PPaccioretti")
```
