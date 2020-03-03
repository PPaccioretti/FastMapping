list_of_packages <- c("shiny",
                      "shinythemes", 
                      "shinyBS",
                      "shinyjs",
                      "DT",
                      "data.table",
                      "dplyr",
                      "ggplot2",
                      "plotly",
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
                      "adespatial")


invisible(lapply(list_of_packages,
                 function(x) if(!require(x,character.only = TRUE)) install.packages(x)))


#suppressPackageStartupMessages(library(shiny))
#suppressPackageStartupMessages(library(shinythemes)) 
#suppressPackageStartupMessages(library(shinyBS))
#suppressPackageStartupMessages(library(shinyjs))
#suppressPackageStartupMessages(library(DT))
#suppressPackageStartupMessages(library(data.table))
#suppressPackageStartupMessages(library(dplyr))
#suppressPackageStartupMessages(library(ggplot2))
#suppressPackageStartupMessages(library(plotly))
#suppressPackageStartupMessages(library(RColorBrewer))
#suppressPackageStartupMessages(library(cowplot))
#suppressPackageStartupMessages(library(knitr))
#suppressPackageStartupMessages(library(rmarkdown))
#library(V8)
#suppressPackageStartupMessages(library(geoR))
#suppressPackageStartupMessages(library(automap))
#suppressPackageStartupMessages(library(fields))
#suppressPackageStartupMessages(library(raster))
#suppressPackageStartupMessages(library(sp))
#suppressPackageStartupMessages(library(rgeos))
#suppressPackageStartupMessages(library(gstat))
#suppressPackageStartupMessages(library(e1071))
#suppressPackageStartupMessages(library(spdep))
#suppressPackageStartupMessages(library(ade4))
#suppressPackageStartupMessages(library(adespatial))