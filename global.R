suppressPackageStartupMessages({
  library(shiny)
  # library(shinythemes)
  library(shinyBS)
  library(shinyjs)
  library(shinycssloaders)
})
# library(shinycssloaders)
# library(data.table)
# library(plotly)
# library(geoR)
# library(automap)
# library(fields)
# library(spdep)
# library(raster)
# library(sp)
# library(ggplot2)
# library(rgeos)
# library(gstat)
# library(e1071)
# library(spdep)
# library(ade4)
# library(rmarkdown)
# library(shinyjs)
source("src/Functions.R")

library(shinycssloaders)
options(spinner.color = "#e95420",
        spinner.type = 6)

options(shiny.sanitize.errors = FALSE)
options(shiny.maxRequestSize = 20*1024^2)


myChoice <- list(Exponential ="Exp",
                 Shperical = "Sph",
                 Gaussian = "Gau",
                 Matern = "Mat",
                 "M. Stein's" ="Ste",
                 Circular = "Cir",
                 Linear = "Lin",
                 Power = "Pow",
                 Wave = "Wav",
                 Pentaspherical = "Pen",
                 Hole = "Hol")

#### Help Strings
datasetHelp <- "File must contain two columns for coordinates and at least one target variable"
tagetVariableHelp <- "If only one variable is selected, FastMapping will run depuration and spatial interpolation tools. More than one should be selected for multivariate analysis"

AutomaticDepHelp <- "Global outliers (Values lower than 0 and outside mean &plusmn; 3 SD), spatial outliers and borders data (20 m from each polygon side) will be removed"
# MethodsDepHelp <- "Ayuda Metodos Depuración"

yLimMinDepHelp <- "Lower threshold boundaries to constrain data within a range of realistic values"
yLimMaxDepHelp <- "Upper threshold boundaries to constrain data within a range of realistic values"
DEOutDepHelp <- "Removes all data points which are more than N times the standard deviation from the mean value"

AutomaticHelp <- "All available models are tested"
nminKriginHelp <- "Minimum number of nearest observations that should be used for a kriging prediction"
nmaxKriginHelp <- "Number of nearest observations that should be used for a kriging prediction or simulation, where nearest is defined in terms of the space of the spatial locations. By default, all observations are used"
distmaxKriginHelp <- "Local neighborhood selections based on distance as radius"
blockKriginHelp <- "block size. Block will not be taken into account for cross-validation. Cross-validation will be performed point-by-point"
dimGrillaKriginHelp <- "Cell size in the prediction grid (in meter)"

minPredScaleKriginHelp <- "Minimum value for which colors should be plotted in predicted plot"
maxPredScaleKriginHelp <- "Maximum value for which colors should be plotted in predicted plot"

min_varScaleKriginHelp <- "Minimum value for which colors should be plotted in variance of predicted values plot"
max_varScaleKriginHelp <- "Maximum value for which colors should be plotted in variance of predicted values plot"


centradoClusterHelp <- "Centring by the mean"
vecindarionuloClusterHelp <- "Create a Weights Matrix allowing for no-neighbour areas"
distanciaClusterHelp <- ""

clustersClusterHelp <- ""
iteracionesClusterHelp <- "Maximum number of iterations"
ExpDifClusterHelp <- ""

distanciavecinoClusterHelp <- ""
varexplicadaClusterHelp <- ""

CressieHelp <- " if selected, use Cressie”s robust variogram estimate; if not use the classical method of moments variogram estimate"

### Validation correlation 

# Variable_verify_differences_Help <- ""
makeSelectProcesHelp <- "This keep for clustering only the variables that are most correlated with those selected in 'Variable selection process'" 
VariableSelectionProcessHelp <- "Removes variables that are not correlated with this variable(s)"
alpha_corr_Help <- "Significance level to remove variables"
