# Copyright 2019 Pablo Paccioretti
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
###### FUNCIONES #####



# list_of_packages <- c("shiny" ,
#                       "shinythemes" ,
#                       "knitr" ,
#                       "ggplot2" ,
#                       "geoR" ,
#                       "plotly" ,
#                       "automap" ,
#                       "fields" ,
#                       "spdep" ,
#                       "raster" ,
#                       "sp" ,
#                       "rgeos" ,
#                       "gstat" ,
#                       "e1071" ,
#                       "ade4" ,
#                       "rmarkdown")
# 
# 
# invisible(lapply(list_of_packages, 
#                  function(x) if(!require(x,character.only = TRUE)) install.packages(x)))




library(shiny)
library(shinythemes)
library(data.table)
library(plotly)
#
# # Moran Plot
# moran.plot1 <-function (x, listw, zero.policy = NULL, spChk = NULL, labels = NULL,
#                         xlab = NULL, ylab = NULL, quiet = NULL, ...){
#   # browser()
#   if (!inherits(listw, "listw"))
#     stop(paste(deparse(substitute(listw)), "is not a listw object"))
#   if (is.null(quiet))
#     quiet <- !get("verbose", envir = .spdepOptions)
#   stopifnot(is.vector(x))
#   stopifnot(is.logical(quiet))
#   if (is.null(zero.policy))
#     zero.policy <- get("zeroPolicy", envir = .spdepOptions)
#   stopifnot(is.logical(zero.policy))
#   xname <- deparse(substitute(x))
#   if (!is.numeric(x))
#     stop(paste(xname, "is not a numeric vector"))
#   if (any(is.na(x)))
#     stop("NA in X")
#   n <- length(listw$neighbours)
#   if (n != length(x))
#     stop("objects of different length")
#   if (is.null(spChk))
#     spChk <- get.spChkOption()
#   if (spChk && !chkIDs(x, listw))
#     stop("Check of data and weights ID integrity failed")
#   labs <- TRUE
#   if (is.logical(labels) && !labels)
#     labs <- FALSE
#   if (is.null(labels) || length(labels) != n)
#     labels <- as.character(attr(listw, "region.id"))
#   wx <- lag.listw(listw, x, zero.policy = zero.policy)
#   if (is.null(xlab))
#     xlab <- xname
#   if (is.null(ylab))
#     ylab <- paste("spatially lagged", xname)
#   if (zero.policy) {
#     n0 <- wx == 0
#     if (any(n0)) {
#       symbols(x[n0], wx[n0], inches = FALSE, circles = rep(diff(range(x))/50,
#                                                            length(which(n0))), bg = "grey", add = TRUE)
#     }
#   }
#   xwx.lm <- lm(wx ~ x)
#   infl.xwx <- influence.measures(xwx.lm)
#   is.inf <- which(apply(infl.xwx$is.inf, 1, any))
#   if (!quiet)
#     summary(infl.xwx)
#   invisible(infl.xwx)
# }
# 
###### #####



options(shiny.sanitize.errors = FALSE)
options(shiny.maxRequestSize = 20*1024^2)
shinyServer(function(input, output, session) {
  
  library(plotly)
  
  histdata <- rnorm(2)
  
  observeEvent(once = TRUE,ignoreNULL = FALSE, ignoreInit = FALSE, eventExpr = histdata, {
    # event will be called when histdata changes, which only happens once, when it is initially calculated
    showModal(modalDialog(
      title = "FastMapping", 
      h1('Welcome to FastMapping'),
      p('If you have any question please write to marianoacba@agro.unc.edu.ar or pablopaccioretti@agro.unc.edu.ar, brief tutorial is available in ',
        a("this link", href = "https://drive.google.com/open?id=1r2-tx35NGLzIjL0CLNR6E783ZRDsWQmf",  target = "_blank"),"."),
      h2("Example datasets:"),
      p("Spatial variability mapping: ",
        a("Yield data in a barley field",
          href = "https://drive.google.com/uc?export=download&id=10TP3taI-BtkeCi59o48Fwypq3mmRSWvN"),"."),
      p("Delineation of homogeneous zones: ",
        a("Yield and soil proprieties within a wheat field",
          href = "https://drive.google.com/uc?export=download&id=1sNR0dl__1V8aKKbq9yUNDobvapz7MD9a"),"."),
      h4("Software is uploading packages. Please wait..."),
      p(strong("This is a beta version."))
      , footer = NULL
    ))
    
    hideTab(inputId = "PanelTabSet", target = "Depuration")
    hideTab(inputId = "PanelTabSet", target = "Adjustments")
    hideTab(inputId = "PanelTabSet", target = "Results")
    hideTab(inputId = "PanelTabSet", target = "Report")
    hideTab(inputId = "PanelTabSet", target = "Multivariate")
    
  })
  
  
  observeEvent(once = TRUE,ignoreNULL = FALSE, ignoreInit = FALSE, eventExpr = histdata, {
    
    progress <- Progress$new(session, min = 1, max = 15)
    on.exit(progress$close())
    progress$set(message = 'Attaching packages',
                 detail = 'This may take a while...')

    
    progress$set(value = 1)
    library(geoR)
    progress$set(value = 2)
    library(automap)
    progress$set(value = 3)
    library(fields)
    progress$set(value = 4)
    library(spdep)
    progress$set(value = 5)
    library(raster)
    progress$set(value = 6)
    library(sp)
    progress$set(value = 7)
    library(ggplot2)
    progress$set(value = 8)
    library(rgeos)
    progress$set(value = 9)
    library(gstat)
    progress$set(value = 10)
    library(e1071)
    progress$set(value = 11)
    library(spdep)
    progress$set(value = 12)
    library(ade4)
    progress$set(value = 13)
    library(rmarkdown)
    library(shinyjs)
    progress$set(value = 14)
    source("Functions.R")
    progress$set(value = 15)
    removeModal()
    
    showModal(modalDialog(
      title = "FastMapping", 
      h1('Welcome to FastMapping'),
      p('If you have any question please write to marianoacba@agro.unc.edu.ar or pablopaccioretti@agro.unc.edu.ar, brief tutorial is available in ',
        a("this link", href = "https://drive.google.com/open?id=1r2-tx35NGLzIjL0CLNR6E783ZRDsWQmf",  target = "_blank"),"."),
      h2("Example datasets:"),
      p("Spatial variability mapping: ",
        a("Yield data in a barley field",
          href = "https://drive.google.com/uc?export=download&id=10TP3taI-BtkeCi59o48Fwypq3mmRSWvN"),"."),
      p("Delineation of homogeneous zones: ",
        a("Yield and soil proprieties within a wheat field",
          href = "https://drive.google.com/uc?export=download&id=1sNR0dl__1V8aKKbq9yUNDobvapz7MD9a"),"."),
      p(strong("This is a beta version."))
      
    ))
    
  })
  
  
  
  
  session$onSessionEnded(stopApp)
  
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
  
  MyChoiceSepdata <- c(Semicolon =';',Tabulator='\t', Space=' ',Comma=',')
  
  output$SepData <- renderUI({
    validate(
      need(input$file,""))
    # browser()
    if(!is.null(input$file)) {
      File <- input$file
      i <- 1
      # if(is.null(input$sep)) {input$sep<-MyChoiceSepdata[1]}
      MiTabla <- tryCatch({
        list("Tabla" = fread(file = File$datapath),
             "Decimal" = ".")
      },error = function(e) {
        list("Tabla" = fread(file = File$datapath, dec = ","),
             "Decimal" = ",")
      })
      
      MiTabla_df <- data.frame(MiTabla$Tabla)  
      
        
      MisCol <- ncol(MiTabla$Tabla)
      while (MisCol == 1 && !is.null(MisCol)) {
        i <- i + 1 
        MisCol <- ncol(read.table(file = File$datapath, sep = MyChoiceSepdata[i], header = input$header, dec = MiTabla$Decimal))
        next}
    }
    radioButtons('sep', 'Separator character', choices = MyChoiceSepdata, selected = MyChoiceSepdata[i])
    
  })
  
  output$ModelosA <- renderUI({
    if(input$bar){return( )
    } else{
      checkboxGroupInput("ModelosA",label=NULL, choices = myChoice)}
  })
  
  SelectedModels <- reactive({
    Mdl<- if(input$bar){myChoice} else{input$ModelosA}
    ValoresOutput$SelectedMdls <- Mdl
    return(Mdl)
  })
  
  data <- reactive({#browser()
    File <- input$file
    if(is.null(File)){return()}
    if(is.null(input$sep)){return()}
    
    # MiTabla<-tryCatch({
    #   read.table(file = File$datapath, sep = input$sep, header = input$header)
    # },error = function(e) {
    #   read.table(file = File$datapath, sep = input$sep, header = input$header, dec=",")
    # })
    MiTabla<-data.frame(fread(File$datapath))
    if(sum(sapply(MiTabla, is.numeric))<= 1) {
      MiTabla<-try(read.table(file = File$datapath, sep = input$sep, header = input$header, dec=","))
    }
    ValoresOutput$Tabla<-MiTabla
    MiTabla
  })
  
  ##### TRANSFORMACION DE COORDENADAS
  TransfCoord<- reactive({
    # browser()
    MyFile <- data()
    MyFile <- data()[,c(input$xmapa, input$ymapa, input$rto)]
    MyFile <- data()[complete.cases(MyFile[,1:2]),]
    if(quantile(MyFile[,1],0.5)<0 &  quantile(MyFile[,2],0.5)<0) {
      if(input$hemisferio==1) {Hemisfer<- " +north"} else {Hemisfer<-" +south"}
      cordsist <- paste0("+proj=utm +zone=",input$zona,Hemisfer ," +ellps=WGS84 +datum=WGS84")
      coordinates (MyFile) <- c(1,2)
      proj4string(MyFile) <- CRS("+proj=longlat + datum=dat")
      Mydata_t <- spTransform(MyFile, CRS(cordsist))
      Mydata_t <- as.data.frame(Mydata_t)[,c(input$xmapa, input$ymapa, input$rto)]
      MyFile<-Mydata_t
    }
    ValoresOutput$TablaCoordTrans<-MyFile
    return(MyFile)
  })

  output$table <- renderDataTable({
    
    if(is.null(data())){return()}
    # if(ncol(data())==1) {return()}
    validate(
      need(ncol(data())!=1, "Please check Separator character"))
    ### AQUI DEBERIA VER SI TIENE COMA COMO DECIMAL, A LO MEJOR DESPUES DEL TYRCATCH
    
    tryCatch(
      error= function (e) {data()[,c(input$xmapa, input$ymapa, input$rto)]}, 
      error= function (e) {data()})
    
  }, options = list(pageLength = 5,lengthMenu = list(c(seq(10, nrow(data()), length.out = 5), -1)[-5], c(seq(10, nrow(data()), length.out = 5)[-5], 'All'))))#   seq(10, nrow(data()), length.out = 5)))
  # initComplete = I("function(settings, json) {alert('Done.');}")))#paging = FALSE))
  
  
  output$tb <- renderUI({
    
    if(is.null(data())){return ()}
    else {
      tabsetPanel(tabPanel("Data", dataTableOutput("table")))}
  })
  
  output$bordesFile <- renderUI({
    if(input$edges == FALSE){return()} else{ fileInput('bordes', 'Edges')}
  })
  
  
  Bordes <- reactive({
    File <- input$bordes
    if(is.null(File)){return()} 
    if(is.null(input$sep)){return()}
    
    MiTabla<-fread(File$datapath, data.table=FALSE)
    # MiTabla<-read.table(file = File$datapath, sep = input$sep, header = input$header)
    
    if(sum(sapply(MiTabla, is.numeric))<= 1) {
      MiTabla<-try(read.table(file = File$datapath, sep = input$sep, header = input$header, dec=","))
    }
    MiTabla
    
  })
  
  #####################  #####################  #####################  #####################  #####################  #####################  #####################  #####################  #####################  #####################  #####################  #####################  #####################  ##################### 
  MyFile <- reactive({
    # browser()
    progress <- Progress$new(session, min = 1, max = 15)
    on.exit(progress$close())
    progress$set(message = 'Depuration in progress',
                 detail = 'This may take a while...')
    MyFile <- TransfCoord()                      ####  #####################  #####################  #####################  ############
    DatosCrudos <- MyFile[,c(input$xmapa, input$ymapa, input$rto)]
    MyFile <- MyFile[complete.cases(MyFile),c(input$xmapa, input$ymapa, input$rto)]
    
    # MyFile <- data()
    # MyFile <- data()[,c(input$xmapa, input$ymapa, input$rto)]
    
    if (is.null(MyFile)) {return()}
    colnames(MyFile)[1] <- paste("X")
    colnames(MyFile)[2] <- paste("Y")
    colnames(MyFile)[3] <- paste("Z")
    
    mapa <- MyFile[,c(1:3)]
    mapa$Filas <- 1:nrow(mapa)
    
    DatosOrig <- mapa
    
    # DatoFaltante <- sum(!complete.cases(MyFile))
    # sum(is.na(mapa$Z))
    
    Duplicados <- duplicated(mapa[,c(1:3)])
    condicion <- data.frame("Filas" = mapa$Filas, "Duplicated" = Duplicados)
    mapa <- mapa[!Duplicados,]
    progress$set(value = 1)
    
    Logi0 <- TRUE %in% c(input$AutomaticDep == 'Automatico',  input$mDepuration %in% c("Outliers", "Inliers"))
    if (as.logical(Logi0))  {
      if (input$SacoBordes) {
        borde <- mapa[chull(mapa[,1:2]),1:2]
        pol <- Polygons(list(Polygon(borde)),as.character(dim(borde)[1]))
        sr <- SpatialPolygons(list(pol))
        bufferValue <- -abs(as.numeric(as.character(input$Buffer)))
        buff <- buffer(sr, width = bufferValue, dissolve = T)
        
        Border <- SpatialPoints(mapa[,1:2]) %over% buff
        datos <- mapa[complete.cases(Border),]
        
        Border <- data.frame("Filas" = names(Border), Border)
        Border[,2] <- addNA(Border[,2])
        levels(Border[,2]) <- c("FALSE", "TRUE")
        condicion <- merge(condicion,Border, all.x = TRUE)
       
        condicion <- unique(condicion)
        
        progress$set(value = 2)
      } else {datos <- mapa}
      
      progress$set(value = 3)
      
      Logi <- TRUE %in% c(input$mDepuration == "Outliers",  input$AutomaticDep == 'Automatico')
      if (as.logical(Logi)) {
        if (is.na(input$ylimitmax) | is.na(input$ylimitmin)) {
          if (is.na(input$ylimitmax)) {
            GlobalMin <- mapa$Z <= input$ylimitmin
            Limitado <- data.frame("Filas" = mapa$Filas, GlobalMin)
            datos <- subset(datos, datos$Z > input$ylimitmin)
          }
          if (is.na(input$ylimitmin)) {
            GlobalMax <- mapa$Z >= input$ylimitmax
            Limitado <- data.frame("Filas" = mapa$Filas, GlobalMax)
            datos <- subset(datos, mapa$Z < input$ylimitmax)
          }
        } else {
          GlobalMin <- mapa$Z <= input$ylimitmin
          GlobalMax <- mapa$Z >= input$ylimitmax
          Limitado <- data.frame("Filas" = mapa$Filas, GlobalMin, GlobalMax)
          datos <- subset(datos, datos$Z > input$ylimitmin &  datos$Z < input$ylimitmax)
        }
        progress$set(value = 4)

        LI <- mean(datos$Z, na.rm = TRUE) - input$DEOut * sqrt(var(datos$Z, na.rm = TRUE))
        LS <- mean(datos$Z, na.rm = TRUE) + input$DEOut * sqrt(var(datos$Z, na.rm = TRUE))
        Outlier <- data.frame("Filas" = datos$Filas, "Outlier" = datos$Z <= LI | datos$Z >= LS)
        
        datos <- subset(datos, datos$Z > LI & datos$Z < LS)
        
        progress$set(value = 5)
        Raritos <- data.frame(tryCatch(merge(Limitado,Outlier, all = TRUE), error = function(e) {Outlier}))
        condicion <- merge(condicion, Raritos, all.x = TRUE) 
        condicion <- unique(condicion)
        progress$set(value = 6)
      }
      progress$set(value = 7)
      
      Logi2 <- TRUE %in% c(input$mDepuration %in% c("Inliers"), input$AutomaticDep == 'Automatico')
      if (as.logical(Logi2)) {
        
        cord <- coordinates(datos[,1:2])
        
        progress$set(value = 8)
        gri <- dnearneigh(cord, input$VecinosMin, input$VecinosMax)
        lw <- try(nb2listw(gri, style = "W"))
        
        progress$set(value = 9)
        
        umin <- input$VecinosMax
        # if (TRUE %in% agrepl(lw[1],"Error in nb2listw(gri, style = W) : Empty neighbour sets found")) {
        if (any(class(lw) == "try-error")) {
          repeat { umin = umin + 10; gri <- dnearneigh(cord, input$VecinosMin, umin); lw <- try(nb2listw(gri, style = "W"));
          if (all(class(lw) != "try-error")) break}}
        progress$set(value = 10)
        LM <- localmoran(datos$Z,lw,p.adjust.method = "bonferroni",alternative = "less")
        progress$set(value = 12)
        datos_LM <- data.frame(datos,LM)
        
        # browser()
        MP <- moran.plot1(datos_LM$Z, lw,quiet = TRUE,labels = FALSE,
                          col = 3, xlab = "Variable", ylab = "Variable Spatially Lagged", zero.policy = FALSE)
        Influ <- MP$is.inf #Influyentes por MoranPlot
        Influ <- data.frame("IdFila" = rownames(Influ),Influ)
        
        
        MyYdepIn <- data.frame(datos_LM,Influ) #Inlier por Moran Local e Influyentes por Moran Plot
        
        # MyYdepIn <- merge(datos_LM, Influ,by.x = "Filas", by.y = "IdFila", all = TRUE, sort = FALSE)#[,c(-1)]
        
        
        # MyInl[,5][MyInl[,5] < 0 & MyInl[,9] < 0.05] <-"Inlier"
        # MyInl[,5][MyInl[,5] !="Inlier"] <- NA
        # 
        # MyInl[,6][MyInl$dfb.1_ == T | MyInl$dfb.x == T | MyInl$dffit == T | MyInl$cov.r == T | MyInl$cook.d  == T | MyInl$hat == T] <- "Inlier"
        # MyInl[,6][MyInl[,6] !="Inlier"] <- NA
        
        # MyYdepIn1 <- subset(MyYdepIn,MyYdepIn[,"Ii"] > 0 | MyYdepIn[,"Pr.z...0."] > 0.05 )
 
        # MyYdepIn2 <- MyYdepIn1[MyYdepIn1$dfb.1_ == FALSE & MyYdepIn1$dfb.x == FALSE & MyYdepIn1$dffit == FALSE
        #                        & MyYdepIn1$cov.r == FALSE & MyYdepIn1$cook.d  == FALSE & MyYdepIn1$hat == FALSE, ]
        
        
        # Mydatafinal <- data.frame(MyYdepIn2[,1:3])   ### Defined not Used         ### Defined not Used
        # MyYdep_LM_Influ <- data.frame(MyYdepIn)[,-c(4)]    ### Defined not Used        ### Defined not Used
        # SpMP <- !(MyYdepIn1$dfb.1_ == FALSE & MyYdepIn1$dfb.x == FALSE & MyYdepIn1$dffit == FALSE
        #         & MyYdepIn1$cov.r == FALSE & MyYdepIn1$cook.d  == FALSE & MyYdepIn1$hat == FALSE)
        
        SpMP <- !(MyYdepIn$dfb.1_ == FALSE & MyYdepIn$dfb.x == FALSE & MyYdepIn$dffit == FALSE
                  & MyYdepIn$cov.r == FALSE & MyYdepIn$cook.d  == FALSE & MyYdepIn$hat == FALSE)
        
        datos <- subset(MyYdepIn,MyYdepIn$Ii > 0 | MyYdepIn$Pr.z...0. > 0.05 | !SpMP)[, c("X","Y","Z")]
        # datos <- datos[,c(1:3)]
        # browser()
        Inliers <- data.frame("Filas" = MyYdepIn$Filas, 
                             "SpatialOutlier" = (MyYdepIn$Ii <= 0 | MyYdepIn$Pr.z...0. <= 0.05), 
                             "SpatialOutlier_MoranPlot" = SpMP)
        condicion <- merge(condicion, Inliers, all.x = TRUE)
        condicion <- unique(condicion)
      }
    } else {datos <- mapa}
    
    progress$set(value = 13)
    condicion <- unique(condicion)
    
    
    # browser()
    
    MisVerd <- which(condicion[,-1, drop=FALSE] == TRUE, arr.ind = TRUE)
    # nrow(MisVerd)
    MisVerd <- MisVerd[!duplicated(MisVerd[,"row"]), ]
    
    CondicFinal <- tryCatch({
      MiCondic <- apply(MisVerd,1,function(x) {
        NombreCol <- colnames(condicion[,-1])
        data.frame("Filas" = x[1],"Condition" = NombreCol[x[2]])
      })
      progress$set(value = 14)
      CondicFinal <- do.call(rbind,MiCondic)
      CondicFinal <- unique(merge(DatosOrig,CondicFinal, all.x = TRUE))[,-1]
      colnames(CondicFinal)[1:3] <- colnames(data()[,c(input$xmapa, input$ymapa, input$rto)])
      CondicFinal
      
    }, error = function(e) {
      
      CondicFinal <- data.frame("X" = NA,"Y" = NA,"Z" = NA, CondFinal = NA)
      colnames(CondicFinal)[1:3] <- colnames(data()[,c(input$xmapa, input$ymapa, input$rto)])
      CondicFinal
    
    })
    
    DatosProcedimientoOriginales <- DatosOrig[,seq_along(c(input$xmapa, input$ymapa, input$rto))]
    colnames(DatosProcedimientoOriginales)[seq_along(c(input$xmapa, input$ymapa, input$rto))] <- c(input$xmapa, input$ymapa, input$rto)

    DatosUtilizadosProcedSinOutliers_Inlierns <- CondicFinal[is.na(CondicFinal[4]),c(input$xmapa, input$ymapa, input$rto)]
    colnames(DatosUtilizadosProcedSinOutliers_Inlierns) <- colnames(data()[,c(input$xmapa, input$ymapa, input$rto)])

    progress$set(value = 15)
    if(all(is.na(DatosUtilizadosProcedSinOutliers_Inlierns))) {
      DatosProcedimiento <- DatosProcedimientoOriginales
    } else {
      DatosProcedimiento <- DatosUtilizadosProcedSinOutliers_Inlierns
    }
    # browser()

        return(list("Datos" = DatosProcedimiento, #DatosUtilizadosProcedSinOutliers_Inlierns,#DatosProcedimientoOriginales, 
                "CondicionDeDepuracion" = CondicFinal, 
                "UtilizadosDep" = DatosUtilizadosProcedSinOutliers_Inlierns,
                "DatosOriginales"= DatosCrudos   ))
    
    
  })
  #####################  #####################  #####################  #####################  #####################  #####################  #####################  #####################  #####################  #####################  #####################  #####################  #####################  ##################### 
  output$BaseColx<-renderUI({
    if(is.null(data())){return()} else{
      validate(
        need(ncol(data())>1,""))
      NombesColumnasData<- colnames(data())
      
      try(selectInput('xmapa', 'X coordinate', choices = NombesColumnasData, selected = NombesColumnasData[1]))
    }})
  
  output$BaseColy<-renderUI({
    if(is.null(data())){return()} else{
      validate(
        need(ncol(data())>=2,""))
      NombesColumnasData<- colnames(data())
      selectInput('ymapa', 'Y coordinate', choices = NombesColumnasData, selected = NombesColumnasData[2])
    }})
  
  output$BaseColRend<-renderUI({
    if(is.null(data())){return()} else{
      validate(
        need(ncol(data())>=3,""))
      NombesColumnasData<- colnames(data())[!colnames(data())%in%c(input$xmapa, input$ymapa)]
      selectInput('rto', 'Target variable',choices = NombesColumnasData, multiple=TRUE, selected=NombesColumnasData)
      
    }})
  
  output$Hemisf <- renderUI({
    validate(
      need(data(),""))
    selectInput("hemisferio", "Hemisphere", choices = list("North" = 1, "South" = 2), selected = 2, width = "100%")
    
  })
  
  output$Zona <- renderUI({
    validate(
      need(data(),""))
    numericInput("zona","Area", 20 ,width = "100%", min = 1, max = 60, step = 1)  
    
  })
  
  output$TextoAviso <- renderUI({
    validate(
      need(data(),"")) 
    
    textOutput("msgTxt")})
  
  output$msgTxt <- renderText({
    try({
     MyFile <- data()[,c(input$xmapa, input$ymapa, input$rto)]
    MyFile <- data()[complete.cases(MyFile[,1:2]), ]
   if(quantile(MyFile[, 1],0.5) < 0 &  quantile(MyFile[, 2],0.5) < 0) {"Projection system will be transform from latlong to UTM automatically"}})
  })
  
  output$DepuratedTable <- renderDataTable({
    validate(
      need(ncol(dataset())==4, 'No extracted data, finally dataset is the same as upload.'))
    MyFile()$UtilizadosDep
  })
  
  output$tablePrueba1 <- renderDataTable({
    validate(
      need(input$file, 'Check input file!'),
      need(ncol(dataset()) == 4, 'No extracted data'))
    data <- MyFile()$CondicionDeDepuracion
    data[complete.cases(data),]
    
  })
  
  dataset <- reactive({
    # validate(
    # browser()
    # table(data$Condition)
    #   need(all(is.na(MyFile()$CondicionDeDepuracion)), 'Superman va de paseo!'))
    data <- MyFile()$CondicionDeDepuracion
    if (all(is.na(data))) {
      ValoresOutput$DataDep <- MyFile()$UtilizadosDep
      if (all(is.na(MyFile()$UtilizadosDep))) {
        return(MyFile()$Datos)
      }
      return(MyFile()$UtilizadosDep)
      }
    if (is.null(data)) {
      ValoresOutput$DataDep <- data
      return(data)
      } else{
        if (ncol(data) == 4 | !is.null(data)) {
          if (ncol(data) >= 4) {
            data[,4] <-  addNA(data[,4])
            levels(data[,4])[length(levels(data[,4]))] <- "NormalPoint"
            ValoresOutput$DataDep <- data
            return(data)}} else {
              ValoresOutput$DataDep <- data
              return(data)}}

  })
  
  nombresCol <- reactive({
    # browser()
    # validate(
    #   need(try(colnames(dataset())), 'Need colnames!!'))
    Nombre <- try(colnames(dataset()))
    ValoresOutput$NombresCol <- Nombre
    return(Nombre)
  })
  
  output$DepuratedPlot <- renderPlotly({
    validate(
      need(input$file, 'Check input file!')#,
      #need(ncol(dataset())==4, 'No depurated data')
    )
    # build graph with ggplot syntax
    p <- ggplot(dataset(), aes_string(x = input$x, y = input$y, color = input$color)) +
      geom_point()
    # browser()
    ggplotly(p) %>%
      layout(autosize = TRUE)
    # layout(height = input$plotHeight, autosize=TRUE)
  })
  #####################  #####################  #####################  #####################  #####################  #####################  #####################  #####################  #####################  #####################  #####################  #####################  #####################  ##################### 
  output$PublicationPlot <- renderPlot({
    validate(
      need(input$file, 'Check input file!')#,
      #need(ncol(dataset())==4, 'No depurated data')
    )
    # build graph with ggplot syntax
    library("RColorBrewer")
    library(cowplot)
    # browser()
    PaletaColorFun <- function(Variable, namepal = 'Set1') {
      if (is.factor(Variable)) {
        PaletaColorVariable <- brewer.pal(n = nlevels(Variable), name = namepal)
        names(PaletaColorVariable) <- levels(Variable) 
        PaletaColorVariable
      }
    }
    if (input$color == "Condition") {  

      Crudos <- dataset()
      
      # Crudos[, input$color] <-factor(Crudos[, input$color], levels = rev(levels(Crudos[, input$color])))# <- rev(levels(dataset()[, input$color]))
      
      Eliminados <- subset(Crudos, Crudos[, input$color] != "NormalPoint",drop = FALSE)
      Remanentes <- subset(Crudos, Crudos[, input$color] == "NormalPoint",drop = FALSE)
      
      PaletaColor <- PaletaColorFun(Crudos[, input$color])
      
      plotCrudos <- ggplot(Crudos, aes_string(x = input$x, y = input$y, 
                                              color = input$color)) + 
        geom_point() + 
        scale_color_manual(values = PaletaColor)
      
      
      plotEliminados <- ggplot(Eliminados, aes_string(x = input$x, y = input$y, 
                                                      color = input$color)) + geom_point() + 
        scale_color_manual(values = PaletaColor)
      
      plotRemanentes <- ggplot(Remanentes, aes_string(x = input$x, y = input$y, 
                                                      color = input$color)) + geom_point() + 
        scale_color_manual(values = PaletaColor)
      
      # a1 <- ggplotly(plotCrudos) 
      # a2 <- ggplotly(plotEliminados) 
      # a3 <- ggplotly(plotRemanentes) 
      
      
      PlotGrid <- plot_grid( plotCrudos + theme(legend.position = "none"),
                             plotEliminados + theme(legend.position = "none"),
                             plotRemanentes + theme(legend.position = "none"),
                             align = 'vh',
                             labels = c("A", "B", "C"),
                             hjust = -1,
                             nrow = 1)
      
      
      legend <- get_legend(plotCrudos + theme(legend.position = "bottom"))

      # add the legend to the row we made earlier. Give it one-third of the width
      # of one plot (via rel_heights).
      p <- plot_grid(PlotGrid, legend, ncol = 1, rel_heights = c(1, .2))
      p
      
    }
    if (input$color == input$rto && "Condition" %in% colnames(dataset())) {
      #####   #####   #####   #####   #####   ##### 
      ##### Yield Plot
      #####   #####   #####   #####   #####   ##### 
      Paleta <- 'Greys'
      cant <- 3
      
      Crudos <- dataset()
      
      quants <- quantile(Crudos[ ,input$rto], seq(0,1, by = 1/cant))
      CategYieldCrudos <- cut(Crudos[ ,input$rto], breaks = quants, include.lowest = TRUE)
      
      Eliminados <- subset(Crudos, Crudos[, "Condition"] != "NormalPoint",drop = FALSE)
      CategYieldEliminados <- subset(CategYieldCrudos, Crudos[, "Condition"] != "NormalPoint",drop = FALSE)
      
      PaletaColorCrudoElim <- PaletaColorFun(CategYieldEliminados, namepal = Paleta)
      
      Remanentes <- subset(Crudos, Crudos[, "Condition"] == "NormalPoint",drop = FALSE)
      quants <- quantile(Remanentes[ ,input$rto], seq(0,1, by = 1/cant))
      CategYieldRemanente <- cut(Remanentes[ ,input$rto], breaks = quants, include.lowest = TRUE)
      
      PaletaYieldRemanentes <- PaletaColorFun(CategYieldRemanente, namepal = Paleta)
      
      
      
      plotCrudosYield <- ggplot(Crudos, aes_string(x = input$x, y = input$y, 
                                                   color = CategYieldCrudos)) + geom_point() + 
        scale_color_manual(values = PaletaColorCrudoElim)
      
      
      plotEliminadosYield <- ggplot(Eliminados, aes_string(x = input$x, y = input$y, 
                                                           color = CategYieldEliminados)) + geom_point() + 
        scale_color_manual(values = PaletaColorCrudoElim)
      
      plotRemanentesYield <- ggplot(Remanentes, aes_string(x = input$x, y = input$y, 
                                                           color = CategYieldRemanente)) + geom_point() + 
        scale_color_manual(values = PaletaYieldRemanentes)
      
      # a1 <- ggplotly(plotCrudosYield) 
      # a2 <- ggplotly(plotEliminadosYield) 
      # a3 <- ggplotly(plotRemanentesYield) 
      
      
      PlotGrid <- plot_grid( plotCrudosYield,# + theme(legend.position = "none"),
                             plotEliminadosYield,# + theme(legend.position = "none"),
                             plotRemanentesYield,# + theme(legend.position = "none"),
                             align = 'vh',
                             labels = c("A", "B", "C"),
                             hjust = -1,
                             nrow = 3) ##  CUANTAS FILAS?
      
      
      legendCrudos <- get_legend(plotCrudosYield )#+ theme(legend.position = "bottom"))
      legendRemanentes <- get_legend(plotRemanentesYield )#+ theme(legend.position = "bottom"))
      # add the legend to the row we made earlier. Give it one-third of the width
      # of one plot (via rel_heights).
      p <- plot_grid(PlotGrid, legendCrudos, legendRemanentes,rel_heights = c(1, .2, .2), ncol = 2, nrow = 1)
     
      p
      
      
      
      
    }
  })
  
  
  
  #####################  #####################  #####################  #####################  #####################  #####################  #####################  #####################  #####################  #####################  #####################  #####################  #####################  ##################### 
  
  #       Funciones
  Formula <- reactive({
    
    NombresCol<-colnames(dataset())
    MiZ<- NombresCol[3]
    MiX<- NombresCol[1]
    MiY<-NombresCol[2]

    if (input$tKriging == 1) {return(as.formula(paste0(MiZ,"~1")))}
    if (input$tKriging == 2) {return(as.formula(paste0(MiZ,"~",MiX,"+",MiY)))}
    if (input$tKriging == 3) {return(as.formula(paste0(MiZ,"~",MiX,"+",MiY, "+I(",MiX,"^2)", "+I(", MiY,"^2)", "+I(",MiX,"*",MiY,")")))}     
  
  })
  ####### Compara los modelos que se especificaron ####
  
  MiKrige <- reactive ({
    withProgress(message = 'Cross validation:',  detail = 'This may take a while...',value = 0, {
      
      Formula <- as.formula(Formula())
      MyFile <- as.data.frame(MyFile()$Datos)
      MyFile[,3] <- as.numeric(as.character(MyFile[,3]))
      coordinates (MyFile) <- c(1,2)
      MyMod=list()
      # browser()
      for (i in SelectedModels()) {
        MyAK=tryCatch({autoKrige.cv(Formula, MyFile, model = c(i), 
                                   nfold = 10, 
                                   nmax=as.numeric(input$nmax), 
                                   nmin=as.numeric(input$nmin),
                                   maxdist=as.numeric(input$distmax))},
                      error=function(e)print(e))
        MyMod[[i]]=MyAK
        incProgress(1/length(SelectedModels()), detail = paste("Doing model", names(myChoice)[myChoice==i]))
        
      }

      #Esta funcion si no puede usar la funcion compare.cv, calcula el RMSE a mano y lo repite 11 vecces
      ModList=lapply(MyMod,function (x) tryCatch(compare.cv(x),error=function(e) {
        cat("Calculating CV by hand\n")
        if(any(c("simpleError","error","condition" ) %in% class(e) )) {
          return(matrix(NA,nrow=11))
        }

        matrix(sqrt(sum(x$krige.cv_output$residual^2, na.rm=TRUE)/sum(complete.cases(x$krige.cv_output$residual))),nrow=11)
        }))
      f=do.call("cbind",ModList)
      colnames(f)=SelectedModels()
      # browser()
      ValoresOutput$MiKrige <- f
      return(f)
    }
    ) })
  
  #####################
  
  output$TModel <- renderTable({
    myresultado <- as.data.frame(MiKrige())
  })
  
  ######
  ## Esta funcion, en vez de SelectedModels, 
  ## se hace con el modelo del RMSE mas cercano a 1
  ######
  MejorModelo <- reactive ({
    ValidationTable=MiKrige()
    tryCatch({
      MyBestModels=names(which.min(apply(ValidationTable[8,],2,as.numeric)))
      return(MyBestModels)},error=function (e) {MyBestModels=colnames(ValidationTable)[1]
      return(MyBestModels)})
  })
  
  Variogram <- reactive({
    # browser()
    Formula <- Formula()
    MyFile <- MyFile()$Datos
    if(is.null(MyFile)) {return()}
    coordinates(MyFile) <- c(1,2)
    Mod<-autofitVariogram(Formula, MyFile, model = MejorModelo(), cutoff = 10000)
    ValoresOutput$Variograma<- Mod
    return(Mod)
  })  
  
  #########  
  
  Mygr <- reactive ({
    
    if(length(MyFile()$Datos) != 0) {
      
      if(length(Bordes()) == 0){
        MyFile <- MyFile()$Datos
        MyZ <- MyFile[,c(1:2)]
        punbor<- chull(MyZ)
        Mybordes <- as.matrix(as.data.frame(MyZ[c(punbor,punbor[1]),1:2]))
        # Mybordes <- as.table(Mybordes)
      }else{
        Mybordes <- Bordes()
      }
      
      gr <- pred_grid(Mybordes, by=as.numeric(input$dimGrilla))
      gri <- polygrid(gr, bor=Mybordes)
      names(gri)[1]<-paste("X")
      names(gri)[2]<-paste("Y")
      gridded(gri) = ~X+Y
      ValoresOutput$Mygr<- gri
      return(gri)
    }
  })
  
  
  
  kriging <- reactive ({
    
    if(length(MyFile()$Datos) != 0) {
      Formula <- Formula()
      MyFile <- MyFile()$Datos
      if(is.null(MyFile)) {return()}
      coordinates (MyFile) <- c(1,2)
      Mygr <- Mygr()
      Modelo <- MejorModelo()
      kriging <- autoKrige(Formula, MyFile, Mygr, model = Modelo,
                           # kriging <- autoKrige(Formula, MyFile, Mygr, model = SelectedModels(),
                           nmax=as.numeric(input$nmax), 
                           nmin=as.numeric(input$nmin),
                           maxdist=as.numeric(input$distmax),
                           block = c(as.numeric(input$block,as.numeric(input$block))),
                           kappa=c(0.05, seq(0.2, 2, 0.1), 5, 10))
      ValoresOutput$kriging <- kriging$krige_output
      return(kriging$krige_output)
    }
  })
  
  
  
  GeoTiff<-reactive({ 
    
    if (input$hemisferio=="2"){
      cordsist <- paste0("+proj=utm +zone=",input$zona," +south" ," +ellps=WGS84 +datum=WGS84")
      r=raster(kriging())
      proj4string(r) <- CRS(cordsist)
      rast=plot(r)
      
    } else  {
      cordsist <- paste0("+proj=utm +zone=",input$zona," +north" ," +ellps=WGS84 +datum=WGS84")
      r = raster(kriging())
      proj4string(r) <- CRS(cordsist)
      rast=plot(r)
      
    }
    ValoresOutput$GeoTiff <- r
    return(r)
  })
  
  
  
  
  #       Plots
  
  output$VariogramPlot <- renderPlot({
    # browser()
    validate(
      need(input$file, 'Check input file!'))
    if(input$SelectPlot == 1 & length(MyFile()$Datos) != 0){
      # dev.new()
      # MiVariograma<-Variogram()
      # browser()
      
      mo <- cbind(Variogram()$var_model)
      nug <- mo[1,2]
      
      if ((nug)==0 ) {param <-2}
      if ((nug)>0) {param <-3}
      
      np <- nrow(Variogram()$exp_var)
      
      mod <- cbind(Variogram()$var_model[,1:4],Variogram()$sserr)
      names(mod)=c("Model", "Parcial Sill","Range","Kappa","SCE")
      Nugget <- mod[1,2]
      Modelo <- mod[2,-4]
      Modelo <- cbind(Modelo,Nugget)
      Modelo <- Modelo[,c(1,2,3,4,5)]
      row.names(Modelo)=NULL
      
      # ValidationTable=MiKrige()
      # MiMejorModelo=MejorModelo()
      ValidationTable=MiKrige()
      MiMejorModelo=MejorModelo()
      MiError= ValidationTable[8,MiMejorModelo][[1]]/mean(MyFile()$Datos[,3])*100
      RMSE=ValidationTable[8,MiMejorModelo][[1]]
      
      Modelo=cbind(Modelo,"RMSE"=RMSE, "Percentage.Error"=MiError)
      suppressWarnings({
      Parametros <- paste(
        c("Model",paste( stack(Modelo[-ncol(Modelo)])[,2]), "Error (%)"),
        c(as.character(Modelo[1,1]),paste(round(stack(Modelo)[,1],1))),
        sep = ": ", collapse = "\n")
      })
# browser()
VariogramData <- variogramLine(vgm(psill=Variogram()$var_model$psill[2], as.character(Variogram()$var_model$model[2]),range=Variogram()$var_model$range[2],nugget=Variogram()$var_model$psill[1]),max(Variogram()$exp_var$dist))
variogg <- ggplot(data =VariogramData ) +
        geom_point(data = Variogram()$exp_var, aes(x=dist,y=gamma), size = 2) + 
        geom_line(aes(x = dist, y = gamma), color = "blue", size = 1.2) +
        xlab("Distance") +
        ylab("Semi-variance") +
        annotate("text", label = Parametros, x = Inf, y = -Inf, hjust = 1, vjust = -0.1, size = 3) +
        scale_y_continuous(limits = c(0, NA)) +
       ggtitle("Experimental variogram and fitted variogram model")
     
     print(variogg) 
      # 
      # 
      #       #### Ver si poner los parametros del semivariograma en el plot
      # plot(Variogram()$exp_var$dist,Variogram()$exp_var$gamma,xlim=c(min(attributes(Variogram()$exp_var)$boundaries),max(Variogram()$exp_var$dist)),
      #      xlab="Distance", ylab="Semi-variance",
      #      main="Experimental variogram and \n fitted variogram model",pch=16, ylim=c(0, max(Variogram()$exp_var$gamma)) )
      # lines(variogramLine(vgm(psill=Variogram()$var_model$psill[2], as.character(Variogram()$var_model$model[2]),
      #                         range=Variogram()$var_model$range[2],nugget=Variogram()$var_model$psill[1]),max(Variogram()$exp_var$dist)),col="blue")
      
      
      
      # dev.new()
      # plot(Variogram())
    }
  },width = 600, height = 600, res = 100)
  
  
  output$KrigingPlot <- renderImage ({
    validate(
      need(input$file, 'Check input file!'))
    # browser()
    if(input$SelectPlot == 2 & length(MyFile()$Datos) != 0){
      outfile <- tempfile(fileext='.png')
      png(outfile, width=900, height=900)
      
      if (is.na(input$min) & is.na(input$max)) {
        zmin <- min(kriging()$var1.pred,na.rm=T)
        zmax <- max (kriging()$var1.pred,na.rm=T)}  
      if (is.na(input$min)== FALSE & is.na(input$max)== FALSE) {
        zmin <- input$min
        zmax <- input$max } 
      if (is.na(input$min)== TRUE & is.na(input$max)== FALSE) {
        zmin <- min(kriging()$var1.pred,na.rm=T)
        zmax <- input$max } 
      if (is.na(input$min)== FALSE & is.na(input$max)== TRUE) {
        zmin <- input$min 
        zmax <- max(kriging()$var1.pred,na.rm=T)}
      image(kriging(), zlim=c(zmin,zmax),"var1.pred", col =terrain.colors(100))
      image.plot(zlim=c(zmin,zmax), legend.only=TRUE, horizontal=F, col=terrain.colors(100), legend.cex=2)
      dev.off()
      list(src = outfile,
           contentType = 'image/png',
           width = 700,
           height = 700,
           alt = "This is alternate text")
    }})
  
  
  output$varKrigingPlot <- renderImage ({
    validate(
      need(input$file, 'Check input file!'))
    
    if(input$SelectPlot == 3 & length(MyFile()$Datos) != 0){
      outfile <- tempfile(fileext='.png')
      png(outfile, width=900, height=900)
      
      if (is.na(input$min_var)== TRUE & is.na(input$max_var)== TRUE) {
        zmin_var <- min(kriging()$var1.var)
        zmax_var <- max (kriging()$var1.var)}  
      if (is.na(input$min_var)== FALSE & is.na(input$max_var)== FALSE) {
        zmin_var <- input$min_var
        zmax_var <- input$max_var} 
      if (is.na(input$min_var)== TRUE & is.na(input$max_var)== FALSE) {
        zmin_var <- min(kriging()$var1.var)
        zmax_var <- input$max_var} 
      if (is.na(input$min_var)== FALSE & is.na(input$max_var)== TRUE) {
        zmin_var <- input$min_var 
        zmax_var <- max(kriging()$var1.var)}
      image(kriging(), zlim=c(zmin_var,zmax_var),"var1.var", col =terrain.colors(100))
      image.plot(zlim=c(zmin_var,zmax_var), legend.only=TRUE, horizontal=F, col=terrain.colors(100), legend.cex = 2)
      dev.off()
      list(src = outfile,
           contentType = 'image/png',
           width = 700,
           height = 700,
           alt = "This is alternate text")
    }})
  
  output$TiffPlot1 <- renderPlot({    
    validate(
      need(input$file, 'Check input file!'))
    plot(GeoTiff())},width = 600, height = 600, res = 100)
  
  
  
  #     render UI plot
  
  output$Plots <- renderUI({
    # browser()
    validate(
      need(input$file, 'Check input file!'))
    if (input$SelectPlot == 1){plotOutput("VariogramPlot")} else if (input$SelectPlot == 2) {
      tabPanel("Plot",
               downloadButton("MiDescarga2","Download Tif"),
               imageOutput("KrigingPlot")
      )} else  {imageOutput("varKrigingPlot") }
    
  })
  
  #Descarga del GeoTiff
  output$MiDescarga2 <- downloadHandler(  
    filename = function() {paste('Map-', Sys.Date(), '.tif', sep='')},
    content = function(con) {writeRaster(GeoTiff(),con,"GTiff")} 
  )    
  
  #Tabla semivariograma experimental
  
  output$varPred <- renderTable({
    #browser()
    validate(
      need(input$file, 'Check input file!'))
    myresultado <- as.data.frame(Variogram()$exp_var)[,1:3]
    names(myresultado)=c("Points", "Distance","Semivariance")
    data.frame(myresultado)
  })
  
  #tabla semivariograma teorico ajustado
  
  output$semivAju <- renderTable({
    validate(
      need(input$file, 'Check input file!'))
    # browser()
    mo <- cbind(Variogram()$var_model)
    nug <- mo[1,2]
    
    if ((nug)==0 ) {param <-2}
    if ((nug)>0) {param <-3}
    
    np <- nrow(Variogram()$exp_var)
    
    mod <- cbind(Variogram()$var_model[,1:4],Variogram()$sserr)
    names(mod)=c("Model", "Parcial Sill","Range","Kappa","SCE")
    Nugget <- mod[1,2]
    Modelo <- mod[2,-4]
    Modelo <- cbind(Modelo,Nugget)
    Modelo <- Modelo[,c(1,2,3,4,5)]
    row.names(Modelo)=NULL
    
    ValidationTable=MiKrige()
    MiMejorModelo=MejorModelo()
    MiError= ValidationTable[8,MiMejorModelo][[1]]/mean(MyFile()$Datos[,3])*100
    RMSE=ValidationTable[8,MiMejorModelo][[1]]
    
    Modelo=cbind(Modelo,"RMSE"=RMSE, "Percentage.Error"=MiError)
    ####
    
    data.frame(Modelo)
  })
  
  # Predichos: Tabla y Descarga
  output$tablita <- renderDataTable({
    validate(
      need(input$file, 'Check input file!'))
    
    myresultado <- as.data.frame(kriging())
    data.frame(myresultado)
  })    
  
  output$Predicted.txt <- downloadHandler(  
    filename = function() {paste('Predicted-', Sys.Date(), '.txt', sep='')},
    content = function(con) {write.table(kriging(), con)},
    contentType =  "text/csv"
  )  
  
  
  output$downloadDepurated <- downloadHandler(
    filename = function() {
      paste('DepuratedData-', Sys.Date(), '.txt', sep='')
    },
    
    content = function(con) {
      validate(
        need(ncol(dataset())==4, 'No extracted data, finally dataset is the same as upload.'))
      write.table(MyFile()$UtilizadosDep, con, quote=FALSE, sep="\t", row.names=FALSE)
      
    }
  )
  
  # output$TablasDepuradas<- renderUI({ 
  #   # browser()
  #   # conditionalPanel(condition = "ncol(dataset())>4",    
  #                    tabPanel("Depurated Data", dataTableOutput("tablePrueba")) 
  #                    tabPanel("Data Extracted", dataTableOutput("tablePrueba1")) 
  #                    tabPanel("Plot Condition Depurated", 
  #                              sidebarPanel(width = 3,
  #                                           selectInput('x', 'X', choices = nombresCol(), selected = nombresCol()[1]),
  #                                           selectInput('y', 'Y', choices = nombresCol(), selected = nombresCol()[2]),
  #                                           selectInput('color', 'Partition',choices = nombresCol(), selected = nombresCol()[4])
  #                              ),
  #                              mainPanel(width = 9,
  #                                        plotlyOutput('DepuratedPlot', height = "600px")
  #                              # )
  #                    ) )})##
  
  #     render resultados
  
  # output$Mensaje<-reactive({if(ncol(dataset())==4){renderText("No depurated data")} else {renderText("")}})
  
  output$resultados <- renderUI({
    validate(
      need(input$file, 'Check input file!'))
    
    tabsetPanel(
      tabPanel("Plots",column(width = 12,
                              selectInput("SelectPlot", label = h5("Select plot"), 
                                          choices = list("Variogram" = 1, "Predicted map" = 2, "Predicted variance map" = 3), 
                                          selected = 1),
                              uiOutput('Plots'))),
      tabPanel("Experimental Variogram", tableOutput("varPred")),
      tabPanel("Fitted Variogram Model", tableOutput("semivAju")),
      tabPanel("Predicted", downloadButton("Predicted.txt","Save File"),dataTableOutput("tablita"))
      ,
      
      # uiOutput("TablasDepuradas")
      # conditionalPanel(condition = "ncol(dataset())==4",
      tabPanel("Depurated Data",downloadButton("downloadDepurated","Save File"), dataTableOutput("DepuratedTable"))
      ,tabPanel("Data Extracted", dataTableOutput("tablePrueba1"))
      ,tabPanel("Plot Condition Data",
                sidebarPanel(width = 3,
                             selectInput('x', 'X', choices = nombresCol(), selected = nombresCol()[1]),
                             selectInput('y', 'Y', choices = nombresCol(), selected = nombresCol()[2]),
                             selectInput('color', 'Partition',choices = nombresCol(), selected = nombresCol()[3])#[NROW(nombresCol())])#,
                             # , textOutput("Mensaje")
                             
                ),
                mainPanel(width = 9,
                          # fluidRow(
                          plotlyOutput('DepuratedPlot', height = "600px")
                          # ,plotOutput('PublicationPlot')
                            
                          # ), 
                          # )
                )) ##
      
      
    )
    
  })
  
  #########################################
  ##############  CLASIFICACION KM-sPC   ##
  #########################################
  # output$Clasificador <- renderUI({
  #   # validate(
  #   #   need(input$file, 'Check input file!'))
  #   
  #   tabsetPanel(
  #     tabPanel("Parameters for KM-sPC classification",
  #              column(width = 12/4,
  #                     checkboxInput("centrado", "Centered", value = TRUE),
  #                     checkboxInput("vecindarionulo", "Data with null neighbor", value = FALSE),
  #                     radioButtons("distancia", "Distance", choices= c("Euclidean"= "euclidean", "Manhattan"="manhattan"))
  #              ),
  #              column(width = 12/4,
  #                     sliderInput("clusters","Number of Cluster to evaluate", min=2, max=15, value = c(2,6)),
  #                     numericInput("iteraciones","Iterations", value=1000, min=1, step=10),
  #                     numericInput("ExpDif", "Degree of fuzzification", value=1.3, step=0.05)
  #              ),
  #              column(width = 12/4,
  #                     h3("Neighborhood network"),
  #                     sliderInput("distanciavecino","Distance between neighbors", min=0, max=1000, value = c(0,35)),
  #                     sliderInput("varexplicada", "Explained variance (%)", min=0, max=100, value = 70)
  #                     
  #                     
  #              )), 
  #     tabPanel("Classification results",
  #              fluidPage(
  #                fluidRow(column( width = 8,dataTableOutput("TablaIndicesConglo")) ),##
  #                fluidRow(column( width = 6,dataTableOutput("TablaResultadosConglom"))  ) )
  #     ),
  #     tabPanel("Cluster Plot",
  #              fluidPage(
  #                column(width = 12/4,
  #                       fluidPage(uiOutput("SelectorCong"))),
  #                column(width = 12-12/4,
  #                       fluidPage(plotlyOutput('ClasificationPlot', height = "600px")))))
  #     
  #     #   ,tabPanel("Plot Classification",
  #     #            sidebarPanel(width = 3,
  #     #                         # selectInput('x', 'X', choices = nombresCol(), selected = nombresCol()[1]),
  #     #                         # selectInput('y', 'Y', choices = nombresCol(), selected = nombresCol()[2]),
  #     #                         selectInput('NumClust', 'Clusters',choices = colnames(Clasificacion()$DatosConCongl), selected = NULL)#[NROW(nombresCol())])#,
  #     #                         # , textOutput("Mensaje")
  #     #                         
  #     #            ),
  #     #            mainPanel(width = 9,
  #     #                      plotlyOutput('ClasificationPlot', height = "600px"))
  #     #   
  #     #   
  #     # )
  #   )
  #   
  # })
  # 
  output$TablaIndicesConglo <- renderDataTable({
    Clasificacion()$Indices}, options = list(paging = FALSE, searching = FALSE, digits=2
                                             #,rowCallback = I("function( nRow, aData) {ind = 2; $('td:eq('+ind+')', nRow).html( parseFloat(aData[ind]).toFixed(2) );}")
    )
  )
  # reactiveValuesToList(input)
  
  output$TablaResultadosConglom<- renderDataTable({
    
    Clasificacion()$ResultadosConglom}, options = list(paging = FALSE, searching = FALSE#, digits=5
                                                       # ,rowCallback = I("function( nRow, aData) {ind = 2; $('td:eq('+ind+')', nRow).html( parseFloat(aData[ind]).toFixed(2) );}")
    )
  )
  
  output$SelectorCong<-renderUI({
    ListaChoices<-colnames(Clasificacion()$DatosConCongl)[!colnames(Clasificacion()$DatosConCongl) %in% c(input$xmapa, input$ymapa)]
    selectInput('NumClust', 'Clusters',choices =ListaChoices 
                , selected = tail(colnames(Clasificacion()$DatosConCongl),1))})
  
  
  Clasificacion <- reactive({
    
    
    # browser()
    
    progress <- Progress$new(session, min=1, max=13)
    on.exit(progress$close())
    progress$set(message = 'KM-sPC classification in progress',
                 detail = 'This may take a while...')
    progress$set(value = 1)
    # TransfCoord()[, c(input$xmapa, input$ymapa)]
    


    if(length(input$rto)==1) {
      Mydata <- as.data.frame(kriging())[,seq_along(c(input$xmapa, input$ymapa,input$rto))]
      colnames(Mydata) <- c(input$xmapa, input$ymapa,input$rto)
    } else {
      MydataNA <- TransfCoord()[, c(input$xmapa, input$ymapa,c(input$rto))]
      Mydata <- na.omit(MydataNA)
      
    }
     MyZ <- Mydata[, c(input$xmapa, input$ymapa)]
    MyY <- Mydata[, c(input$rto), drop=F]
    
    if(ncol(MyY)==1) {
      progress$set(message = 'Fuzzy classification in progress')
      
      clusterKM <-function(cen){
        MC <- cmeans(MyY, dist=input$distancia,centers=cen,
                     iter.max = as.numeric(input$iteraciones), method="cmeans", m=as.numeric(input$ExpDif))
      }
      progress$set(value = 3)
      clasificaciones <- apply(matrix(seq(as.numeric(input$clusters[1])
                                          ,as.numeric(input$clusters[2]),by=1)),1,clusterKM)
      progress$set(value = 4)
    }
    
    if(ncol(MyY)>1) {
      if (input$vecindarionulo)  {
        set.ZeroPolicyOption(TRUE) }
      
      cord <- coordinates (MyZ[,1:2])
      gri <- dnearneigh(cord, as.numeric(input$distanciavecino[1]), as.numeric(input$distanciavecino[2]))
      lw <- try(nb2listw(gri, style = "W"), silent = TRUE)
      progress$set(value = 2)
      # if (lw[1]!="W") {
      #   rm("MyResults")
      #   MyResults=list("Error: aumentar la  m?xima distancia para definir la red de vecindarios"=data.frame())
      #   return(MyResults)
      #   stop("Error: aumentar la  m?xima distancia para definir la red de vecindarios",call. = F)}
      
      #  Analisis de Componentes Principales (PCA)
      
      # Biplot, autovalores asociados a cada CP (grafico de barras) y calculo de correlaciones de las CP1 y CP2 del PCA.
      
      ifelse (input$centrado,
              pca <- dudi.pca(MyY, center=T,scannf = FALSE, nf=ncol(MyY)),
              pca <- dudi.pca(MyY, center=F,scannf = FALSE, nf=ncol(MyY)))
      
      # ms <- adespatial::multispati(pca, lw, scannf = F, nfnega= ncol(MyY), nfposi = ncol(MyY))  ##################################################################################################################
      ms <- multispati(pca, lw, scannf = F, nfnega= ncol(MyY), nfposi = ncol(MyY))
      
      capture.output(resms<- summary(ms), file='NUL')
      var_ms <- as.data.frame(resms[,2])
      nfila_ms <- length(ms$eig)
      propvar_ms <- var_ms/nfila_ms
      propvaracum_ms <- cumsum(propvar_ms)*100
      
      eje_ms <-c(1: nfila_ms)
      resultado_ms <- cbind(eje_ms,resms$eig,resms$"var",propvar_ms,propvaracum_ms)
      names(resultado_ms) <- c("Eje","Autovalores","Varianza Espacial","Proporcion","Prop. Acum.")
      filares_ms <- length(ms$li)
      resultado_ms <-resultado_ms[1:filares_ms,]
      
      num_sPC <- min(which((resultado_ms[,5])>=as.numeric(input$varexplicada)))
      sPC <- ms$li[1:num_sPC]
      
      clusterKM <-function(cen){
        MC <- cmeans(sPC, dist=input$distancia,centers=cen,
                     iter.max = as.numeric(input$iteraciones), method="cmeans", m=as.numeric(input$ExpDif))
      }
      
      clasificaciones <- apply(matrix(seq(as.numeric(input$clusters[1])
                                          ,as.numeric(input$clusters[2]),by=1)),1,clusterKM)
    }
    res_clas <- lapply(clasificaciones,function(Clus) c(Clus$cluster))
    res_clas <- data.frame(do.call("cbind",res_clas))
    Name <- paste("Cluster", " ", seq(input$clusters[1], input$clusters[2]), sep="")
    names(res_clas)=c(Name)
    
    res_iter <- lapply(clasificaciones,function(x) c("Iterations"=x$iter))
    res_iter <- do.call("rbind",res_iter)
    
    res_scdd <- lapply(clasificaciones,function(x) c("SSDW"=x$withinerror))  # sum of square distances within the clusters.
    res_scdd <- do.call("rbind",res_scdd)
    # 
    progress$set(value =3)
    ## Indices
    
    progress$set(value = 4)
    
    
    Ind <- function (obj) {
      # browser()
      fclustIndex_modif(y=obj,MyY, index=c("xie.beni", "fukuyama.sugeno",
                                           "partition.coefficient", "partition.entropy"))
    }
    progress$set(value = 9)
    Indices <- lapply(clasificaciones,Ind)
    Indices <- do.call("rbind",Indices)
    progress$set(value = 10)
    norm <- function (div) {
      # browser()
      div/max(div)}                     #######################################################################################3
    if(nrow(Indices)>1) {
      IndN <-apply(Indices,2,norm)                              #######################################################################################3
      IndN <-apply(IndN,1,function (xx) {sqrt(sum(xx^2))})   
    } else {IndN <- Indices}  #######################################################################################3
    #######################################################################################3
    Cluster <- seq(as.numeric(input$clusters[1]),as.numeric(input$clusters[2]))
    ResultadosIndices <- data.frame(cbind(Cluster,Indices, IndN))
    names(ResultadosIndices)=c("Num. Cluster", "Xie Beni", "Fukuyama Sugeno",
                               "Partition Coefficient", "Entropy of Partition","Summary Index")
    
    #tryCatch({data.frame(Cluster,Indices, IndN)}, error=function (e) {data.frame(cbind(Cluster,Indices, t(IndN)))})
    
    #     par(mfrow=c(2,2))
    #     plot(seq(as.numeric(input$clusters[1]),as.numeric(input$clusters[2])),
    #          ResultadosIndices[,2],xlab = "N?mero de Cluster", ylab="Xie Beni")
    #     plot(seq(as.numeric(input$clusters[1]),as.numeric(input$clusters[2])),
    #          ResultadosIndices[,3],xlab = "N?mero de Cluster", ylab="Fukuyama Sugeno")
    #     plot(seq(as.numeric(input$clusters[1]),as.numeric(input$clusters[2])),
    #          ResultadosIndices[,4],xlab = "N?mero de Cluster", ylab="Coeficiente de Partici?n")
    #     plot(seq(as.numeric(input$clusters[1]),as.numeric(input$clusters[2])),
    #          ResultadosIndices[,5],xlab = "N?mero de Cluster", ylab="Entrop?a de Partici?n")
    progress$set(value = 11)
    resultados <- data.frame(cbind(Cluster,res_iter,res_scdd))
    # x11()
    # plot(seq(as.numeric(input$clusters[1]),as.numeric(input$clusters[2]),by=1),
    #      resultados[,3],xlab = "N?mero de Cluster", ylab="SCDD")
    progress$set(value = 12)

    MisClus <-data.frame("Con" = as.numeric(rownames(res_clas)),res_clas)
 
    progress$set(value = 13)
    
    if(length(input$rto)==1) {
       resultados<-list("Conglomerado" = MisClus ,"ResultadosConglom"=resultados,
                     "Indices" = ResultadosIndices, "DatosConCongl"=data.frame(Mydata,apply(MisClus,2,as.factor)),
                     "NombresColCluster" = colnames(data.frame(Mydata,MisClus)))
      
    } else {
      
      MydataNA_Con <- cbind("Con" = as.numeric(rownames(MydataNA)),MydataNA)
      MydataNA_Con_Cluster <- merge(MydataNA_Con,MisClus, by="Con",all.x=TRUE)
      
      MisClus_1 <- MydataNA_Con_Cluster[,-c(1:length(MydataNA_Con))]
      
      resultados<-list("Conglomerado" = MisClus_1 ,"ResultadosConglom"=resultados,
                       "Indices" = ResultadosIndices, "DatosConCongl"=data.frame(Mydata,apply(MisClus_1,2,as.factor)),
                       "NombresColCluster" = colnames(data.frame(Mydata,MisClus_1)))
      
      
    }
   
    ValoresOutput$Clasificacion<-resultados
    
    return(resultados)
    
  })
  
  
  # output$DatosClusters
  
  output$ClasificationPlot <- renderPlotly({
    validate(
      need(input$file, 'Check input file!')#,
      #need(ncol(dataset())==4, 'No depurated data')
    )
    
    # browser()
    # build graph with ggplot syntax
    p <- ggplot(Clasificacion()$DatosConCongl, aes_string(x = input$xmapa, y = input$ymapa, 
                                                          colour=input$NumClust)) +
      geom_point()
    # browser()
    ggplotly(p) %>%
      layout(autosize=TRUE)
    
    ValoresOutput$GraficoConglom <- p
    # layout(height = input$plotHeight, autosize=TRUE)
  })
  
  #########################################
  ##############  REPORTE   ##
  #########################################
  
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = function() {
      paste('my-report', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    
    content = function(file) {
      # browser()
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(input=isolate(reactiveValuesToList(input)),
                     output=isolate(reactiveValuesToList(ValoresOutput)))
      
      # save(paramts, file="C:/Users/Pablo/Data1.RData")
      # Knit the document, passing in the `paramts` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      
      rmarkdown::render(input=tempReport, output_format=switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()),
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv()))
      # browser()
    }
  )
  
  ValoresOutput <- reactiveValues(Tabla=NULL, TablaCoordTrans=NULL,DataDep=NULL,NombresCol=NULL,
                                  SelectedMdls=NULL,  MiKrige=NULL,
                                  Variograma=NULL, Mygr=NULL, kriging=NULL, GeoTiff=NULL,
                                  Clasificacion=NULL, GraficoConglom=NULL)
  
  observeEvent(input$rto, {
    
    if(is.null(input$rto)){
      hideTab(inputId = "PanelTabSet", target = "Depuration")
      hideTab(inputId = "PanelTabSet", target = "Adjustments")
      hideTab(inputId = "PanelTabSet", target = "Results")
      hideTab(inputId = "PanelTabSet", target = "Report")
      # hideTab(inputId = "PanelTabSet", target = "Cluster")
      }
    
    
    if(length(input$rto)==1){
      showTab(inputId = "PanelTabSet", target = "Depuration")
      showTab(inputId = "PanelTabSet", target = "Adjustments")
      showTab(inputId = "PanelTabSet", target = "Results")
      showTab(inputId = "PanelTabSet", target = "Report")
      # showTab(inputId = "PanelTabSet", target = "Cluster")
      }
    
    if(length(input$rto)>1){
      hideTab(inputId = "PanelTabSet", target = "Depuration")
      hideTab(inputId = "PanelTabSet", target = "Adjustments")
      hideTab(inputId = "PanelTabSet", target = "Results")
      showTab(inputId = "PanelTabSet", target = "Report")
      # showTab(inputId = "PanelTabSet", target = "Cluster")
      }
    
    # if(length(input$rto)==0){
    #   hideTab(inputId = "PanelTabSet", target = "Depuration")
    #   hideTab(inputId = "PanelTabSet", target = "Adjustments")
    #   hideTab(inputId = "PanelTabSet", target = "Results")
    #   hideTab(inputId = "PanelTabSet", target = "Report")
    #   hideTab(inputId = "PanelTabSet", target = "Multivariate")
    # }
    
  })
  
  # output$tabPanel_title = renderText({
  #   if(length(input$rto)>1) {return("Multivariate")}
  #   "Cluster"
  #   # ifelse(length(input$rto)>1,"Multivariate", "Cluster")
  # })
  
})