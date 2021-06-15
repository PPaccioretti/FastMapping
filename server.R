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
###### #####
function(input, output, session) {
  
  suppressPackageStartupMessages(library(plotly))
  
  histdata <- rnorm(2)
  
  
  observeEvent(input$startApl, {
    shinyjs::show(selector = '#PanelTabSet li a[data-value="DatasetTab"]')
    showTab(inputId = "PanelTabSet", target = "DatasetTab")
    updateTabsetPanel(session, "PanelTabSet", selected = "DatasetTab")
  })
  
  observeEvent(
    once = TRUE,
    ignoreNULL = FALSE,
    ignoreInit = FALSE,
    eventExpr = histdata,
    {
      progress <- Progress$new(session, min = 1, max = 15)
      on.exit(progress$close())
      progress$set(message = 'Attaching packages',
                   detail = 'This may take a while...')
      
      suppressPackageStartupMessages({
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
        library(DT)
        progress$set(value = 12)
        library(ade4)
        library(V8)
        progress$set(value = 13)
        library(rmarkdown)
        # library(shinyjs)
        progress$set(value = 14)
        # source("Functions.R")
        progress$set(value = 15)
        
        list_of_packages <- c(
          "shiny",
          # "shinythemes",
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
          "ggpubr",
          "RColorBrewer",
          "cowplot",
          "rmarkdown",
          "knitr",
          "kableExtra",
          "V8",
          "geoR",
          "automap",
          "fields",
          "stars",
          "sf",
          "raster",
          "sp",
          "rgeos",
          "gstat",
          "e1071",
          "spdep",
          "ade4",
          "adespatial",
          "SpatialPack"
        )
        
        
        
        
        invisible(lapply(list_of_packages,
                         function(x)
                           if (!require(x, character.only = TRUE))
                           {
                             install.packages(x)
                             library(x, character.only = TRUE)
                           }))
        # removeModal()
      })
      waiter_hide()
    }
  )
  
  
  session$onSessionEnded(stopApp)
  
  
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
  
  data <- reactive({
    File <- input$file
    if (is.null(File)) {
      return()
    }
    # if(is.null(input$sep)){return()}
    
    MiTabla <-
      fread(File$datapath,
            data.table = FALSE,
            check.names = TRUE)
    
    if (all(!sapply(MiTabla[, !sapply(MiTabla, is.integer)], is.numeric))) {
      MiTabla <-
        try(fread(
          File$datapath,
          data.table = FALSE,
          dec = ",",
          check.names = TRUE
        ))
    }
    
    ValoresOutput$Tabla <- MiTabla
    MiTabla
  })
  
  
  CoordSist_crs <- reactive({
    validate(need(input$hemisferio, input$file, ''))
    Hemisfer <- switch(input$hemisferio,
                       "1" = 6,#" +north",
                       "2" = 7 #" +south"
    )
    cordsist <- as.numeric(paste0(32,Hemisfer,input$zona))
    return(cordsist)
  })
  
  ##### TRANSFORMACION DE COORDENADAS
  TransfCoord<- reactive({
    # MyFile <- data()
    MyFile <- data()[,c(input$xmapa, input$ymapa, input$rto)]
    MyFile <- MyFile[complete.cases(MyFile[,1:2]),]
    
    
    # browser()
    
    # Hemisfer <- switch(input$hemisferio,
    #   "1" = " +north",
    #   "2" = " +south"
    # )
    
    # cordsist <- paste0("+proj=utm +zone=",input$zona,Hemisfer ," +ellps=WGS84 +datum=WGS84")
    
    # Hemisfer <- switch(input$hemisferio,
    #   "1" = 6,#" +north",
    #   "2" = 7 #" +south"
    # )
    # cordsist <- as.numeric(paste0(32,Hemisfer,input$zona))
    
    if(quantile(MyFile[,1],0.5)<0 &  quantile(MyFile[,2],0.5)<0) {
      # coordinates(MyFile) <- c(1,2)
      # proj4string(MyFile) <- CRS("+proj=longlat + datum=dat")
      # Mydata_t <- spTransform(MyFile, CRS(cordsist))
      # Mydata_t <- as.data.frame(Mydata_t)[,c(input$xmapa, input$ymapa, input$rto)]
      
      MyFile_sf <- st_as_sf(MyFile, coords = c(input$xmapa, input$ymapa), crs = 4326)
      Mydata_t <- st_transform(MyFile_sf, CoordSist_crs())
      
      Mydata_t <- data.frame(st_coordinates(Mydata_t), st_drop_geometry(Mydata_t))
      names(Mydata_t) <- c(input$xmapa, input$ymapa, input$rto)
      
      MyFile<-Mydata_t
    }
    
    ValoresOutput$TablaCoordTrans <-
      list(TablaCoords = MyFile, coordproj = CoordSist_crs())
    return(MyFile)
  })
  
  output$table <- DT::renderDataTable({
    
    if(is.null(data())){return()}
    # if(ncol(data())==1) {return()}
    validate(
      need(ncol(data())!=1, "Please check Separator character"))
    ### AQUI DEBERIA VER SI TIENE COMA COMO DECIMAL, A LO MEJOR DESPUES DEL TYRCATCH
    
    tryCatch(
      error= function (e) {data()[,c(input$xmapa, input$ymapa, input$rto)]}, 
      error= function (e) {data()})
    
  }, options = list(pageLength = 5,
                    scrollX = TRUE,
                    lengthMenu = list(c(seq(10, nrow(data()), length.out = 5), -1)[-5], c(seq(10, nrow(data()), length.out = 5)[-5], 'All'))))#   seq(10, nrow(data()), length.out = 5)))
  # initComplete = I("function(settings, json) {alert('Done.');}")))#paging = FALSE))
  
  
  output$edgesTable <- DT::renderDataTable({
    
    if(is.null(Bordes())){return()}
    # if(ncol(data())==1) {return()}
    validate(
      need(ncol(Bordes())!=1, "Please check Separator character"))
    ### AQUI DEBERIA VER SI TIENE COMA COMO DECIMAL, A LO MEJOR DESPUES DEL TYRCATCH
    Bordes()
    
  }, options = list(
    scrollX = TRUE
  ))#
  
  
  output$tb <- renderUI({
    
    if(is.null(data())){return ()}
    if(!is.null(data())) {
      if(is.null(Bordes())) {
        return(tabsetPanel(tabPanel("Data", DT::dataTableOutput("table"))))
      }
      if(!is.null(Bordes())) {
        return(
          tabsetPanel(tabPanel("Data", DT::dataTableOutput("table")),
                      tabPanel("Edges", DT::dataTableOutput("edgesTable")))
        )
        
      }}
    
  })
  
  output$bordesFile <- renderUI({
    if(input$edges == FALSE){return()} else{ fileInput('bordes', 'Edges')}
  })
  
  
  Bordes <- reactive({
    File <- input$bordes
    if(is.null(File) | !input$edges){return()} 
    # if(is.null(input$sep)){return()}
    
    MiTabla<-fread(File$datapath, data.table=FALSE)
    # MiTabla<-read.table(file = File$datapath, sep = input$sep, header = input$header)
    
    # if(sum(sapply(MiTabla, is.numeric))<= 1) {
    #   MiTabla<-try(read.table(file = File$datapath, sep = input$sep, header = input$header, dec=","))
    # }
    
    MiTabla
    
  })
  
  #####################  #####################  #####################  #####################  #####################  #####################  #####################  #####################  #####################  #####################  #####################  #####################  #####################  ##################### 
  MyFile <- reactive({
    progress <- Progress$new(session, min = 1, max = 15)
    on.exit(progress$close())
    progress$set(message = 'Depuration in progress',
                 detail = 'This may take a while...')
    MyFile <- TransfCoord()                      
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
    
    
    #########
    # if(input$AutomaticDep == 'Automatico') {
    #   
    #   
    # }
    #   
    
    
    
    #########
    
    Logi0 <- TRUE %in% c(input$AutomaticDep == 'Automatico',  input$mDepuration %in% c("Outliers", "Inliers"))
    if (as.logical(Logi0))  {
      if (input$SacoBordes) {
        # borde <- mapa[chull(mapa[,1:2]),1:2]
        borde <- getBorders()
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
        
        datos <- subset(MyYdepIn,MyYdepIn$Ii > 0 | MyYdepIn$Pr.z...0. > 0.05)[, c("X","Y","Z")]
        # datos <- datos[,c(1:3)]
        Inliers <- data.frame("Filas" = MyYdepIn$Filas, 
                              "SpatialOutlier" = (MyYdepIn$Ii <= 0 & MyYdepIn$Pr.z...0. <= 0.05))
        if(input$moranPlot) {
          datos <- subset(MyYdepIn,MyYdepIn$Ii > 0 | MyYdepIn$Pr.z...0. > 0.05 | !SpMP)[, c("X","Y","Z")]
          Inliers$"SpatialOutlier_MoranPlot" <- SpMP
        }
        
        condicion <- merge(condicion, Inliers, all.x = TRUE)
        condicion <- unique(condicion)
      }
    } else {datos <- mapa}
    
    progress$set(value = 13)
    condicion <- unique(condicion)
    
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
    
    return(list("Datos" = DatosProcedimiento, #DatosUtilizadosProcedSinOutliers_Inlierns,#DatosProcedimientoOriginales, 
                "CondicionDeDepuracion" = CondicFinal, 
                "UtilizadosDep" = DatosUtilizadosProcedSinOutliers_Inlierns,
                "DatosOriginales"= DatosCrudos   ))
    
    
  })
  #####################  #####################  #####################  #####################  #####################  #####################  #####################  #####################  #####################  #####################  #####################  #####################  #####################  #####################
  output$BaseColx <- renderUI({
    if (is.null(data())) {
      return()
    } else{
      validate(need(ncol(data()) > 1, ""))
      NombesColumnasData <- colnames(data())
      
      try(selectInput('xmapa',
                      'X coordinate',
                      choices = NombesColumnasData,
                      selected = NombesColumnasData[1]))
    }
  })
  
  output$BaseColy <- renderUI({
    if (is.null(data())) {
      return()
    } else{
      validate(need(ncol(data()) >= 2, ""))
      NombesColumnasData <- colnames(data())
      selectInput('ymapa',
                  'Y coordinate',
                  choices = NombesColumnasData,
                  selected = NombesColumnasData[2])
    }
  })
  
  output$BaseColRend <- renderUI({
    if (is.null(data())) {
      return()
    } else{
      validate(need(ncol(data()) >= 3, ""))
      NombesColumnasData <-
        colnames(data())[!colnames(data()) %in% c(input$xmapa, input$ymapa)]
      
      tipify(
        el = selectInput(
          'rto',
          'Target variable',
          choices = NombesColumnasData,
          multiple = TRUE,
          selected = NombesColumnasData
        ),
        title = tagetVariableHelp,
        placement = "top",
        trigger = "hover",
        options = NULL
      )
      
      
    }
  })
  
  output$ui_data_param <- renderUI({
    wellPanel(
      uiOutput("BaseColx", inline = TRUE),
      uiOutput("BaseColy", inline = TRUE),
      uiOutput("BaseColRend", inline = TRUE),
      uiOutput("TextoAviso", inline = TRUE)
    )
  })
  
  
  
  output$Hemisf <- renderUI({
    validate(need(data(), ""))
    selectInput(
      "hemisferio",
      "Hemisphere",
      choices = list("North" = 1, "South" = 2),
      selected = 2,
      width = "100%"
    )
    
  })
  
  output$Zona <- renderUI({
    validate(need(data(), ""))
    numericInput(
      "zona",
      "UTM area",
      20 ,
      width = "100%",
      min = 1,
      max = 60,
      step = 1
    )
    
  })
  
  output$ui_utm_param <- renderUI({
    validate(need(data(), ""))
    br()
    wellPanel(
      uiOutput("Hemisf", inline = TRUE),
      uiOutput("Zona", inline = TRUE)
    )
    
  })
  
  
  output$TextoAviso <- renderUI({
    validate(need(data(), ""))
    
    textOutput("msgTxt")
  })
  
  output$msgTxt <- renderText({
    try({
      MyFile <- data()[, c(input$xmapa, input$ymapa, input$rto)]
      MyFile <- MyFile[complete.cases(MyFile[, 1:2]),]
      if (quantile(MyFile[, 1], 0.5) < 0 &
          quantile(MyFile[, 2], 0.5) < 0) {
        "Projection system will be transformed from latlong to UTM automatically"
      }
    })
  })
  
  
  
  
  
  
  output$DepuratedTable <- renderDataTable({
    validate(
      need(ncol(dataset())==4, 'No extracted data, finally dataset is the same as upload.'))
    MyFile()$UtilizadosDep
  })
  
  output$tableDataExtracted <- DT::renderDataTable({
    validate(
      need(input$file, 'Check input file!'),
      need(ncol(dataset()) == 4, 'No extracted data'))
    # browser()
    # table(MyFile()$CondicionDeDepuracion[complete.cases( MyFile()$Datos),"Condition"], useNA = "always")
    # prop.table(table(data[complete.cases( MyFile()$Datos),"Condition"], useNA = "always"))*100
    
    data <- MyFile()$CondicionDeDepuracion
    
    sd(MyFile()$CondicionDeDepuracion[complete.cases(MyFile()$CondicionDeDepuracion),]$Rinde)
    sd(MyFile()$CondicionDeDepuracion$Rinde)
    
    data[complete.cases(data),]
    
  })
  
  dataset <- reactive({
    
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
    p <- ggplot(dataset(), aes_string(x = input$x, 
                                      y = input$y, 
                                      color = input$color, 
                                      text = input$rto)) +
      geom_point()
    
    ggplotly(p) %>%
      plotly::layout(autosize = TRUE)
    # layout(height = input$plotHeight, autosize=TRUE)
  })
  #####################  #####################  #####################  #####################  #####################  #####################  #####################  #####################  #####################  #####################  #####################  #####################  #####################  ##################### 
  output$PublicationPlot <- renderPlot({
    validate(need(input$file, 'Check input file!')#,
             #need(ncol(dataset())==4, 'No depurated data')
    )
    # browser()
    # build graph with ggplot syntax
    suppressPackageStartupMessages({
      library("RColorBrewer")
      library(cowplot)
      library(ggpubr)
    })
    # browser()
    PaletaColorFun <- function(Variable, namepal = 'Set1') {
      if (is.factor(Variable)) {
        PaletaColorVariable <-
          brewer.pal(n = nlevels(Variable), name = namepal)
        names(PaletaColorVariable) <- levels(Variable)
        PaletaColorVariable
      }
    }
    
    themePlotsCondition <- function() {
      list(
        theme(
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_blank(),
          legend.key = element_rect(fill = NA, color = NA)
        ),
        guides(color = guide_legend(override.aes = list(size = 3)))
      )
      
    }
    
    if (input$color == "Condition") {
      Crudos <- dataset()
      
      
      Eliminados <-
        subset(Crudos, Crudos[, input$color] != "NormalPoint", drop = FALSE)
      Remanentes <-
        subset(Crudos, Crudos[, input$color] == "NormalPoint", drop = FALSE)
      
      PaletaColor <- PaletaColorFun(Crudos[, input$color])
      
      plotCrudos <-
        ggplot(Crudos,
               aes_string(
                 x = input$x,
                 y = input$y,
                 color = input$color
               )) +
        geom_point() +
        scale_color_manual(values = PaletaColor) +
        labs(color = input$color) +
        themePlotsCondition()
      
      
      plotEliminados <-
        ggplot(Eliminados,
               aes_string(
                 x = input$x,
                 y = input$y,
                 color = input$color
               )) + geom_point() +
        scale_color_manual(values = PaletaColor) +
        labs(color = input$color) +
        themePlotsCondition()
      
      plotRemanentes <-
        ggplot(Remanentes,
               aes_string(
                 x = input$x,
                 y = input$y,
                 color = input$color
               )) + geom_point() +
        scale_color_manual(values = PaletaColor) +
        labs(color = input$color) +
        themePlotsCondition()
      
      PlotGrid <- ggpubr::ggarrange(
        plotCrudos,
        plotEliminados,
        plotRemanentes,
        labels = c("A", "B", "C"),
        hjust = -1,
        nrow = 1,
        common.legend = TRUE,
        legend = 'bottom'
      )
      
      print(PlotGrid)
      
    }
    if (input$color == input$rto &&
        "Condition" %in% colnames(dataset())) {
      #####   #####   #####   #####   #####   #####
      ##### Yield Plot
      #####   #####   #####   #####   #####   #####
      # Paleta <- 'Greys'
      Paleta <- 'Greens'# 'Spectral'
      cant <- 4
      
      Crudos <- dataset()
      
      quants <- quantile(Crudos[, input$rto], seq(0, 1, by = 1 / cant))
      CategYieldCrudos <-
        cut(Crudos[, input$rto], breaks = quants, include.lowest = TRUE)
      
      Eliminados <-
        subset(Crudos, Crudos[, "Condition"] != "NormalPoint", drop = FALSE)
      CategYieldEliminados <-
        subset(CategYieldCrudos, Crudos[, "Condition"] != "NormalPoint", drop = FALSE)
      
      PaletaColorCrudoElim <-
        PaletaColorFun(CategYieldEliminados, namepal = Paleta)
      
      Remanentes <-
        subset(Crudos, Crudos[, "Condition"] == "NormalPoint", drop = FALSE)
      quants <-
        quantile(Remanentes[, input$rto], seq(0, 1, by = 1 / cant))
      CategYieldRemanente <-
        cut(Remanentes[, input$rto],
            breaks = quants,
            include.lowest = TRUE)
      
      PaletaYieldRemanentes <-
        PaletaColorFun(CategYieldRemanente, namepal = Paleta)
      
      
      
      plotCrudosYield <-
        ggplot(Crudos,
               aes_string(
                 x = input$x,
                 y = input$y,
                 color = CategYieldCrudos
               )) + geom_point() +
        scale_color_manual(values = PaletaColorCrudoElim) +
        labs(color = input$color) +
        themePlotsCondition()
      
      
      plotEliminadosYield <-
        ggplot(Eliminados,
               aes_string(
                 x = input$x,
                 y = input$y,
                 color = CategYieldEliminados
               )) + geom_point() +
        scale_color_manual(values = PaletaColorCrudoElim) +
        labs(color = input$color) +
        themePlotsCondition()
      
      plotRemanentesYield <-
        ggplot(Remanentes,
               aes_string(
                 x = input$x,
                 y = input$y,
                 color = CategYieldRemanente
               )) + geom_point() +
        scale_color_manual(values = PaletaYieldRemanentes) +
        labs(color = input$color) +
        themePlotsCondition()
      
      # a1 <- ggplotly(plotCrudosYield)
      # a2 <- ggplotly(plotEliminadosYield)
      # a3 <- ggplotly(plotRemanentesYield)
      
      PlotCondition <- ggpubr::ggarrange(
        ggpubr::ggarrange(
          plotCrudosYield,
          plotEliminadosYield,
          ncol = 2,
          nrow = 1,
          common.legend = TRUE,
          legend = 'bottom',
          labels = c("Raw data", "Removed data")
        ),
        plotRemanentesYield,
        heights = c(2, 1),
        # widths	= c(2, 1), 
        labels = c("", "Cleaned data"),
        # hjust = -1,
        nrow = 1,
        ncol = 2,
        legend = 'right'
      )
      
      print(PlotCondition)
      
    }
  })
  
  
  
  #####################  #####################  #####################  #####################  #####################  #####################  #####################  #####################  #####################  #####################  #####################  #####################  #####################  ##################### 
  
  #       Funciones
  Formula <- reactive({
    
    NombresCol<-colnames(dataset())
    MiZ<- NombresCol[3]
    MiX<- NombresCol[1]
    MiY<- NombresCol[2]
    
    if (input$tKriging == 1) {return(as.formula(paste0(MiZ,"~1")))}
    if (input$tKriging == 2) {return(as.formula(paste0(MiZ,"~",MiX,"+",MiY)))}
    if (input$tKriging == 3) {return(as.formula(paste0(MiZ,"~",MiX,"+",MiY, "+I(",MiX,"^2)", "+I(", MiY,"^2)", "+I(",MiX,"*",MiY,")")))}     
    
  })
  ####### Compara los modelos que se especificaron ####
  
  MiKrige <- reactive({
    withProgress(message = 'Cross validation:',
                 detail = 'This may take a while...',
                 value = 0,
                 {
                   Formula <- as.formula(Formula())
                   MyFile <- as.data.frame(MyFile()$Datos)
                   MyFile[, 3] <-
                     as.numeric(as.character(MyFile[, 3]))
                   coordinates(MyFile) <- c(1, 2)
                   MyMod = list()
                   
                   if (nrow(MyFile) > nrow(remove.duplicates(MyFile))) {
                     difRow <- nrow(MyFile) - nrow(remove.duplicates(MyFile))
                     if (difRow == 1) {
                       mensajeElim <-
                         paste(
                           "To perform cross-validation,",
                           difRow,
                           "point pair with equal spatial coordinate was removed"
                         )
                     } else {
                       mensajeElim <-
                         paste(
                           "To perform cross-validation,",
                           difRow,
                           "point pairs with equal spatial coordinates were removed"
                         )
                     }
                     
                     showNotification(mensajeElim ,
                                      type = "default", duration = 3)
                   }
                   
                   for (i in SelectedModels()) {
                     # autoKrige.cv command does not take in account the blocks of your data. It performs the cross-validation point-by-point and not by blocks.
                     #
                     # Cross validation takes in account the accuracy of the estimates of the interpolation (or prediction) for POINTS while block kriging is a smoothing method that divides the whole area into several BLOCKS and calculate the local average of your estimations for each of those area. In other words, for the area 'block' you don't have a 'value' to compare your estimation made by kriging
                     
                     
                     MyAK = tryCatch({
                       autoKrige.cv(
                         Formula,
                         remove.duplicates(MyFile),
                         model = c(i),
                         nfold = 10,
                         nmax = as.numeric(input$nmax),
                         nmin = as.numeric(input$nmin),
                         # block = c(as.numeric(input$block,as.numeric(input$block))),
                         maxdist = as.numeric(input$distmax),
                         miscFitOptions = list(cressie = input$cressie)
                       )
                     },
                     error = function(e)
                       print(e))
                     MyMod[[i]] = MyAK
                     incProgress(1 / length(SelectedModels()),
                                 detail = paste("Validating",
                                                names(myChoice)[myChoice == i],
                                                "model"))
                     
                   }
                   
                   
                   #Esta funcion si no puede usar la funcion compare.cv, calcula el RMSE a mano y lo repite 11 vecces
                   ModList = lapply(MyMod, function(x)
                     tryCatch(
                       compare.cv(x),
                       error = function(e) {
                         cat("Calculating CV by hand\n")
                         
                         showNotification(
                           paste("Something went wrong while cross-validation"),
                           type = "warning",
                           duration = 5,
                           id = "Aviso"
                         )
                         
                         if (any(c("simpleError", "error", "condition") %in% class(e))) {
                           return(matrix(NA, nrow = 11))
                         }
                         
                         matrix(sqrt(
                           sum(x$krige.cv_output$residual ^ 2, na.rm = TRUE) / sum(complete.cases(x$krige.cv_output$residual))
                         ), nrow = 11)
                       }
                     ))
                   f = do.call("cbind", ModList)
                   colnames(f) = SelectedModels()
                   ValoresOutput$MiKrige <- f
                   return(f)
                 })
  })
  
  #####################
  
  output$TModel <- renderTable({
    myresultado <- as.data.frame(MiKrige())
  })
  
  ######
  ## Esta funcion, en vez de SelectedModels,
  ## se hace con el modelo del RMSE mas cercano a 1
  ######
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
  
  Variogram <- reactive({
    Formula <- Formula()
    MyFile <- MyFile()$Datos
    if (is.null(MyFile)) {
      return()
    }
    coordinates(MyFile) <- c(1, 2)
    Mod <- autofitVariogram(
      Formula,
      MyFile,
      model = MejorModelo(),
      cutoff = 10000,
      cressie = input$cressie
    )
    
    ValoresOutput$Variograma <- Mod
    return(Mod)
  })
  
  #########
  
  getBorders <- reactive({
    validate(need(data(), ''))

    if (is.null(input$bordes)) {
      MyZ <- TransfCoord()[, c(input$xmapa, input$ymapa)]
      punbor <- chull(MyZ)
      Mybordes <-
        as.matrix(as.data.frame(MyZ[c(punbor, punbor[1]), 1:2]))
    } else{
      Mybordes <- Bordes()
    }
    return(Mybordes)
  })
  
  
  Mygr <- reactive({
    if (length(MyFile()$Datos) != 0) {
      
      Mybordes <- getBorders()
      
      gr <- pred_grid(Mybordes, by = as.numeric(input$dimGrilla))
      gri <- polygrid(gr, bor = Mybordes)
      
      names(gri)[1] <- paste(input$xmapa)
      names(gri)[2] <- paste(input$ymapa)
      gridded(gri) = as.formula(paste("~", input$xmapa, " + ", input$ymapa))
      ValoresOutput$Mygr <- gri
      return(gri)
    }
  })
  
  
  
  kriging <- reactive({
    if (length(MyFile()$Datos) != 0) {
      Formula <- Formula()
      MyFile <- MyFile()$Datos
      if (is.null(MyFile)) {
        return()
      }
      coordinates(MyFile) <- c(1, 2)
      proj4string(MyFile) <-
        CRS(SRS_string = paste0("EPSG:", CoordSist_crs()))
      Mygr <- Mygr()
      crs(Mygr) <- crs(MyFile)
      
      Modelo <- MejorModelo()
      krigingfit <- autoKrige(
        Formula,
        MyFile,
        Mygr,
        model = Modelo,
        nmax = as.numeric(input$nmax),
        nmin = as.numeric(input$nmin),
        maxdist = as.numeric(input$distmax),
        block = c(as.numeric(
          input$block, as.numeric(input$block)
        )),
        kappa = c(0.05, seq(0.2, 2, 0.1), 5, 10),
        miscFitOptions = list(cressie = input$cressie)
      )
      ValoresOutput$kriging <- krigingfit$krige_output
      return(krigingfit$krige_output)
    }
  })
  
  
  
  GeoTiff <- reactive({ 
    raster_Pred <- stars::st_as_stars(kriging())
    
    return(raster_Pred)

  })
  
  
  
  
  #       Plots
  
  output$VariogramPlot <- renderPlot({
    validate(need(input$file, 'Check input file!'))
    if (input$SelectPlot == 1 & length(MyFile()$Datos) != 0) {
      mo <- cbind(Variogram()$var_model)
      nug <- mo[1, 2]

      mod <- cbind(Variogram()$var_model[, 1:4], Variogram()$sserr)
      names(mod) = c("Model", "Parcial Sill", "Range", "Kappa", "SCE")
      Nugget <- mod[1, 2]
      Modelo <- mod[2,]
      if (Modelo$Kappa == 0.5) {
        Modelo <- mod[2, -4]
      }
      Modelo <- cbind(Modelo, Nugget)
      # Modelo <- Modelo[,c(1,2,3,4,5)]
      row.names(Modelo) = NULL
      
      # ValidationTable=MiKrige()
      # MiMejorModelo=MejorModelo()
      ValidationTable = MiKrige()
      MiMejorModelo = MejorModelo()
      MiError = ValidationTable[8, MiMejorModelo][[1]] / mean(MyFile()$Datos[, 3]) *
        100
      RMSE = ValidationTable[8, MiMejorModelo][[1]]
      
      Modelo = data.frame(
        Modelo,
        "RMSE" = RMSE,
        "Error (%)" = MiError,
        check.names = FALSE
      )
      suppressWarnings({
        Parametros <- paste(
          c("Model", paste(stack(Modelo[-ncol(Modelo)])[, 2]), "Error (%)"),
          c(as.character(Modelo[1, 1]), paste(round(
            stack(Modelo)[, 1], 1
          ))),
          sep = ": ",
          collapse = "\n"
        )
      })
      
      VariogramData <-
        variogramLine(Variogram()$var_model, max(Variogram()$exp_var$dist))
      variogg <- ggplot(data = VariogramData) +
        geom_point(data = Variogram()$exp_var,
                   aes(x = dist, y = gamma),
                   size = 2) +
        geom_line(aes(x = dist, y = gamma), color = "blue", size = 1.2) +
        xlab("Distance") +
        ylab("Semi-variance") +
        annotate(
          "text",
          label = Parametros,
          x = Inf,
          y = -Inf,
          hjust = 1,
          vjust = -0.1,
          size = 3
        ) +
        scale_y_continuous(limits = c(0, NA)) +
        ggtitle("Experimental variogram and fitted variogram model")
      
      ValoresOutput$variogg <- variogg
      
      print(variogg)
      
    }
  }, width = 600, height = 600, res = 100)
  
  
  output$KrigingPlot <- renderImage({
    validate(need(input$file, 'Check input file!'))
    if (input$SelectPlot == 2 & length(MyFile()$Datos) != 0) {
      outfile <- tempfile(fileext = '.png')
      png(outfile, width = 900, height = 900)
      
      zmin <- input$min
      zmax <- input$max
      
      if (is.na(zmin)) {
        zmin <- min(kriging()$var1.pred, na.rm = T)
      }
      if (is.na(zmax)) {
        zmax <- max(kriging()$var1.pred, na.rm = T)
      }
      
      image(kriging(),
            zlim = c(zmin, zmax),
            "var1.pred",
            col = terrain.colors(100))
      image.plot(
        zlim = c(zmin, zmax),
        legend.only = TRUE,
        horizontal = F,
        col = terrain.colors(100),
        legend.cex = 2
      )
      dev.off()
      list(
        src = outfile,
        contentType = 'image/png',
        width = 700,
        height = 700,
        alt = "Predicted Plot"
      )
    }
  }, deleteFile = TRUE)
  
  
  output$varKrigingPlot <- renderImage({
    validate(need(input$file, 'Check input file!'))
    
    if (input$SelectPlot == 3 & length(MyFile()$Datos) != 0) {
      outfile <- tempfile(fileext = '.png')
      png(outfile, width = 900, height = 900)
      
      zmin_var <- input$min_var
      zmax_var <- input$max_var
      
      if (is.na(zmin_var)) {
        zmin_var <- min(kriging()$var1.var, na.rm = T)
        }
      if (is.na(zmax_var)) {
        zmax_var <- max(kriging()$var1.var, na.rm = T)
      }
      
      image(
        kriging(),
        zlim = c(zmin_var, zmax_var),
        "var1.var",
        col = terrain.colors(100)
      )
      image.plot(
        zlim = c(zmin_var, zmax_var),
        legend.only = TRUE,
        horizontal = F,
        col = terrain.colors(100),
        legend.cex = 2
      )
      dev.off()
      list(
        src = outfile,
        contentType = 'image/png',
        width = 700,
        height = 700,
        alt = "Variance Plot"
      )
    }
  }, deleteFile = TRUE)
  
  output$TiffPlot1 <- renderPlot({
    validate(need(input$file, 'Check input file!'))
    plot(GeoTiff(), col = terrain.colors(100))
    
  }, width = 600, height = 600, res = 100)
  
  
  
  #     render UI plot
  
  output$Plots <- renderUI({
    validate(need(input$file, 'Check input file!'))
    
    # browser()
    
    if (input$SelectPlot == 1) {
      plotOutput("VariogramPlot")
    } else if (input$SelectPlot == 2) {
      tabPanel(
        "Plot",
        downloadButton("MiDescarga2", "Download Tif"),
        imageOutput("KrigingPlot")
      )
    } else  {
      imageOutput("varKrigingPlot")
    }
    
  })
  
  #Descarga del GeoTiff
  output$MiDescarga2 <- downloadHandler(
    filename = function() {
      paste('Map-', Sys.Date(), '.tif', sep = '')
    },
    content = function(con) {
      # browser()
      # writeRaster(GeoTiff(),con,"GTiff")
      Predicted_Tiff <- GeoTiff()
      stars::write_stars(Predicted_Tiff, con, layer = attributes(Predicted_Tiff)$names)
    }
  )
  
  #Tabla semivariograma experimental
  
  output$varPred <- renderTable({
    validate(need(input$file, 'Check input file!'))
    myresultado <- as.data.frame(Variogram()$exp_var)[, 1:3]
    names(myresultado) = c("Points", "Distance", "Semivariance")
    data.frame(myresultado)
  })
  
  #tabla semivariograma teorico ajustado
  
  output$semivAju <- renderTable({
    validate(need(input$file, 'Check input file!'))
    
    mo <- cbind(Variogram()$var_model)
    nug <- mo[1, 2]


    mod <- cbind(Variogram()$var_model[, 1:4], Variogram()$sserr)
    names(mod) = c("Model", "Parcial Sill", "Range", "Kappa", "SCE")
    
    Nugget <- mod[1, 2]
    Modelo <- mod[2,]
    if (Modelo$Kappa == 0.5) {
      Modelo <- mod[2, -4]
    }
    Modelo <- cbind(Modelo, Nugget)
    # Modelo <- Modelo[,c(1,2,3,4,5)]
    row.names(Modelo) = NULL
    
    ValidationTable = MiKrige()
    MiMejorModelo = MejorModelo()
    MiError = ValidationTable[8, MiMejorModelo][[1]] / mean(MyFile()$Datos[, 3]) *
      100
    RMSE = ValidationTable[8, MiMejorModelo][[1]]
    
    Modelo = data.frame(
      Modelo,
      "RMSE" = RMSE,
      "Error (%)" = MiError,
      check.names = FALSE
    )
    ####
    
    data.frame(Modelo, check.names = FALSE)
  })
  
  # Predichos: Tabla y Descarga
  output$tablita <- DT::renderDataTable({
    validate(need(input$file, 'Check input file!'))
    
    myresultado <- data.frame(kriging())
    
    datatable(myresultado, rownames = FALSE)
  })
  
  
  output$predictedSummary <- renderPrint({
    validate(need(input$file, ''))
    
    myresultado <- data.frame(kriging())
    summary(myresultado)
  })
  
  output$Predicted.txt <- downloadHandler(
    filename = function() {
      paste('Predicted-', Sys.Date(), '.txt', sep = '')
    },
    content = function(con) {
      write.table(kriging(), con, row.names = FALSE)
    },
    contentType =  "text/csv"
  )
  
  
  output$downloadDepurated <- downloadHandler(
    filename = function() {
      paste('DepuratedData-', Sys.Date(), '.txt', sep = '')
    },
    
    content = function(con) {
      validate(need(
        ncol(dataset()) == 4,
        'No extracted data, finally dataset is the same as upload.'
      ))
      write.table(
        MyFile()$UtilizadosDep,
        con,
        quote = FALSE,
        sep = "\t",
        row.names = FALSE
      )
      
    }
  )
  
  output$resultados <- renderUI({
    validate(need(input$file, 'Check input file!'))
    
    tabsetPanel(
      tabPanel("Plots", column(
        width = 12,
        selectInput(
          "SelectPlot",
          label = h5("Select plot"),
          choices = list(
            "Variogram" = 1,
            "Predicted map" = 2,
            "Predicted variance map" = 3
          ),
          selected = 1
        ),
        uiOutput('Plots')
      )),
      tabPanel("Experimental Variogram", tableOutput("varPred")),
      tabPanel("Fitted Variogram Model", tableOutput("semivAju")),
      tabPanel(
        "Predicted",
        downloadButton("Predicted.txt", "Save File"),
        DT::dataTableOutput("tablita"),
        h4("Predicted values summary"),
        verbatimTextOutput("predictedSummary")
      ),
      tabPanel(
        "Depurated Data",
        downloadButton("downloadDepurated", "Save File"),
        dataTableOutput("DepuratedTable")
      ),
      
      tabPanel(
        "Data Extracted",
        DT::dataTableOutput("tableDataExtracted")
      ),
      
      tabPanel(
        "Plot Condition Data",
        sidebarPanel(
          width = 3,
          selectInput('x',
                      'X',
                      choices = nombresCol(),
                      selected = nombresCol()[1]),
          selectInput('y',
                      'Y',
                      choices = nombresCol(),
                      selected = nombresCol()[2]),
          selectInput(
            'color',
            'Partition',
            choices = nombresCol(),
            selected = nombresCol()[3]
          )
        ),
        mainPanel(
          width = 9,
          fluidRow(
            plotlyOutput('DepuratedPlot', height = "600px") %>%
              withSpinner()
          ),
          fluidRow(
            plotOutput('PublicationPlot',
                       width = "100%",
                       height = "400px")
          )
        )
      )
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
  output$TablaIndicesConglo <- DT::renderDataTable({
    datatable(
      Clasificacion()$Indices,
      rownames = FALSE,
      options = list(searching = FALSE,
                     paging = FALSE)
    )
  })
  
  output$GraficoIndicesConglo <- renderPlotly({
    dataIndicConglWide <-
      data.frame(
        "Cluster" = Clasificacion()$Indices[, 1],
        scale(Clasificacion()$Indices[, -1]),
        check.names = FALSE
      )
    
    dataIndicesConglomLong <- reshape(
      dataIndicConglWide,
      idvar = "Cluster",
      times = names(dataIndicConglWide)[-1],
      timevar = "Index",
      v.names = "Value",
      varying = list(names(dataIndicConglWide)[-1]),
      direction = "long"
    )
    dataIndicesConglomLong$Index <-
      factor(
        dataIndicesConglomLong$Index,
        labels = unique(dataIndicesConglomLong$Index),
        levels = unique(dataIndicesConglomLong$Index)
      )
    
    dataIndicesConglomLong$isSummary <-
      as.numeric(dataIndicesConglomLong$Index == "Summary Index") + 1
    
    
    ggplotCongl <-  ggplot(dataIndicesConglomLong,
                           aes(x = Cluster, y = Value, color = Index)) +
      geom_point() +
      geom_line(linetype = dataIndicesConglomLong$isSummary) +
      labs(y = "Standardized value")
    
    ggplotly(ggplotCongl) %>%
      plotly::layout(autosize = TRUE)
    
  })
  
  output$TablaResultadosConglom <- DT::renderDataTable({
    datatable(
      Clasificacion()$ResultadosConglom,
      rownames = FALSE,
      options = list(searching = FALSE,
                     paging = FALSE)
    )
  })
  

  SelectBestCluster <- reactive({
    Indices <- Clasificacion()$Indices
    Selec <-
      names(Clasificacion()$Conglomerado)[which.min(Indices[, ncol(Indices)])]
    Selec
  })
  
  
  
  output$SelectorCong <- renderUI({
    ListaChoices <-
      colnames(Clasificacion()$DatosConCongl)[!colnames(Clasificacion()$DatosConCongl) %in% c(input$xmapa, input$ymapa)]
    selectInput('NumClust',
                'Clusters',
                choices = ListaChoices ,
                selected = SelectBestCluster())
  })
  
  
  output$changedistanciavecinomax <- renderUI({
    if (input$distanciavecino[2] >= 1000) {
      fluidRow(
        h5(
          "It seems that you want more distance than 1000 m between neighbours... which one do you want?"
        ),
        numericInput("maxdistNeigh", "Max distance", value = input$distanciavecino[2])
      )
    }
  })
  
  output$ui_multivariate_params <- renderUI({
    validate(need(length(input$rto) > 1))
    column(
      width = 12 / 3,
      h3("Spatial PCA parameters"),
      # br(),
      checkboxInput("centrado", "Centered variables", value = TRUE),
      sliderInput(
        "varexplicada",
        "Explained variance (%)",
        min = 0,
        max = 100,
        value = 70
      ),
      
      h4("Neighborhood network"),
      checkboxInput("vecindarionulo", "Data with null neighbor", value = FALSE),
      sliderInput(
        "distanciavecino",
        "Distance between neighbors",
        min = 0,
        max = 1000,
        value = c(0, 35)
      ),
      uiOutput("changedistanciavecinomax"),
      
      bsTooltip(
        "centrado",
        centradoClusterHelp,
        placement = "bottom",
        trigger = "hover",
        options = NULL
      ),
      bsTooltip(
        "varexplicada",
        varexplicadaClusterHelp,
        placement = "bottom",
        trigger = "hover",
        options = NULL
      ),
      bsTooltip(
        "vecindarionulo",
        vecindarionuloClusterHelp,
        placement = "bottom",
        trigger = "hover",
        options = NULL
      ),
      bsTooltip(
        "distanciavecino",
        distanciavecinoClusterHelp,
        placement = "bottom",
        trigger = "hover",
        options = NULL
      )
      
    )
    
  })
  
  
  
  observeEvent(input$maxdistNeigh, {
    # if(input$distanciavecino) {
    updateNumericInput(session, "distanciavecino", max = max(c(1000, input$maxdistNeigh)))
    # }
    
  })
  
  Clasificacion <- reactive({
    
    set.seed(2020)
    progress <- Progress$new(session, min=1, max=13)
    on.exit(progress$close())
    progress$set(message = 'KM-sPC classification in progress',
                 detail = 'This may take a while...')
    progress$set(value = 1)
    # TransfCoord()[, c(input$xmapa, input$ymapa)]
    
    
    
    if(length(input$rto)==1) {
      MydataNA <- as.data.frame(kriging())[,seq_along(c(input$xmapa, input$ymapa,input$rto))]
      
      FilasNA <- apply(MydataNA, 1, function(x) {
        any(is.na(x))
      })
      
      Mydata <- na.omit(MydataNA)
      colnames(Mydata) <- c(input$xmapa, input$ymapa,input$rto)
    } else {
      MydataNA <- TransfCoord()[, c(input$xmapa, input$ymapa,input$rto)]
      
      FilasNA <- apply(MydataNA, 1, function(x) {
        any(is.na(x))
      })
      
      Mydata <- na.omit(MydataNA)
      
    }
    MyZ <- Mydata[, c(input$xmapa, input$ymapa)]
    MyY <- Mydata[, c(input$rto), drop=F]
    
    
    clusterKM <-function(cen, datos){
      MC <- cmeans(datos, dist=input$distancia,centers=cen,
                   iter.max = as.numeric(input$iteraciones), method="cmeans", m=as.numeric(input$ExpDif))
    }   
    
    if(ncol(MyY)==1) {
      progress$set(message = 'Fuzzy classification in progress')
      
      progress$set(value = 3)
      clasificaciones <- apply(matrix(seq(as.numeric(input$clusters[1])
                                          ,as.numeric(input$clusters[2]),by=1)),1,clusterKM,datos = MyY)
      progress$set(value = 4)
    }
    
    if(ncol(MyY)>1) {
      
      set.ZeroPolicyOption(input$vecindarionulo)
      
      cord <- coordinates(MyZ[,1:2])
      gri <- dnearneigh(cord, as.numeric(input$distanciavecino[1]), as.numeric(input$distanciavecino[2]))
      # lw <- try(nb2listw(gri, style = "W"), silent = TRUE)
      
      lw <- tryCatch(nb2listw(gri, style = "W"), silent = TRUE,
                     error = function(e){
                       if(agrepl("Empty neighbour sets found", e)){
                         showNotification("Try with more distance between neighbors.\n
                                    Or select data with null neighbor ", type = "error")
                         updateTabsetPanel(session, "ClustersTabs", selected = "ClasifParameters")
                         stop("Empty neighbour sets found",call. = FALSE)
                       }
                     })
      
      
      progress$set(value = 2)
      #  Analisis de Componentes Principales (PCA)
      
      
      # Biplot, autovalores asociados a cada CP (grafico de barras) y calculo de correlaciones de las CP1 y CP2 del PCA.
      ###### PCA con Coords ------
      # pca <- dudi.pca(Mydata, center=input$centrado,scannf = FALSE, nf=ncol(Mydata))
      # ms <- adespatial::multispati(pca, lw, scannf = F, nfnega= ncol(Mydata), nfposi = ncol(Mydata))  ########################################################################
      ### PCA SIN COORDS -----
      # pca <- dudi.pca(scale(MyY), center=FALSE,scannf = FALSE, nf=ncol(MyY))
      
      pca <- dudi.pca(MyY, center=input$centrado,scannf = FALSE, nf=ncol(MyY))
      ms <- adespatial::multispati(pca, lw, scannf = F, nfnega= ncol(MyY), nfposi = ncol(MyY))  #########################################################################
      ### Error cuando no encuentra vecinos Error in adespatial::multispati: object of class 'listw' expected
      # ms <- multispati(pca, lw, scannf = F, nfnega= ncol(MyY), nfposi = ncol(MyY))
      
      invisible(capture.output(resms <- summary(ms)))
      var_ms <- resms[,2, drop = F]
      nfila_ms <- length(ms$eig)
      propvar_ms <- var_ms/nfila_ms
      propvaracum_ms <- cumsum(propvar_ms)*100
      
      eje_ms <-c(1:nfila_ms)
      resultado_ms <- data.frame(eje_ms,resms$eig,resms$"var",propvar_ms,propvaracum_ms)
      names(resultado_ms) <- c("Eje","Autovalores","Varianza Espacial","Proporcion","Prop. Acum.")
      filares_ms <- length(ms$li)
      resultado_ms <- resultado_ms[1:filares_ms,]
      
      num_sPC <- min(which(resultado_ms[,5]>as.numeric(input$varexplicada)))
      sPC <- ms$li[1:num_sPC]
      #cor(data.frame(MyY, sPC))
      
      clasificaciones <- apply(matrix(seq(as.numeric(input$clusters[1])
                                          ,as.numeric(input$clusters[2]),by=1)),
                               1,clusterKM, datos = sPC)#sPCCoords) ######### ------
    }
    
    res_clas <- lapply(clasificaciones,function(Clus) c(Clus$cluster))
    res_clas <- data.frame(do.call("cbind",res_clas))
    Name <- paste("Cluster", "_", seq(input$clusters[1], input$clusters[2]), sep="")
    names(res_clas)=c(Name)
    
    res_iter <- lapply(clasificaciones,function(x) c("Iterations"=x$iter))
    res_iter <- do.call("rbind",res_iter)
    
    res_scdd <- lapply(clasificaciones,function(x) c("SSDW"=x$withinerror))  # sum of square distances within the clusters.
    res_scdd <- do.call("rbind",res_scdd)
    # 
    progress$set(value =3)
    ## Indices
    
    progress$set(value = 4)
    
    
    Ind <- function(obj) {
      fclustIndex_modif(y=obj,MyY, index=c("xie.beni", #"fukuyama.sugeno",
                                           "partition.coefficient", "partition.entropy"))
    }
    
    
    progress$set(value = 9)
    Indices <- lapply(clasificaciones,Ind)
    Indices <- do.call("rbind",Indices)
    
    progress$set(value = 10)
    norm <- function (div) {
      div/max(div)}                     #######################################################################################3
    if(nrow(Indices)>1) {
      IndN <-apply(Indices,2,norm)                              #######################################################################################3
      IndN <-apply(IndN,1,function (xx) {sqrt(sum(xx^2))})   
    } else {IndN <- Indices}  #######################################################################################3
    #######################################################################################3
    Cluster <- seq(as.numeric(input$clusters[1]),as.numeric(input$clusters[2]))
    ResultadosIndices <- data.frame(Cluster,Indices, IndN)
    names(ResultadosIndices)=c("Num. Cluster", "Xie Beni", #"Fukuyama Sugeno",
                               "Partition Coefficient", "Entropy of Partition","Summary Index")
    
    
    progress$set(value = 11)
    resultados <- data.frame(Cluster,res_iter,res_scdd)
    
    progress$set(value = 12)
    
    # MisClus <-data.frame("Con" = as.numeric(rownames(res_clas)),res_clas)
    MisClus <- data.frame(res_clas)
    progress$set(value = 13)
    if(length(input$rto)==1) {
      resultados<-list("Conglomerado" = MisClus ,"ResultadosConglom"=resultados,
                       "Indices" = ResultadosIndices, "DatosConCongl"=data.frame(Mydata,apply(MisClus,2,as.factor)),
                       "NombresColCluster" = colnames(data.frame(Mydata,MisClus)))
      
    } else {
      MisClus <-data.frame("Con" = as.numeric(rownames(res_clas)),res_clas)
      MydataNA_Con <- cbind("Con" = as.numeric(rownames(MydataNA)),MydataNA)
      MydataNA_Con_Cluster <- merge(MydataNA_Con,MisClus, by="Con",all.x=TRUE)
      
      MisClus_1 <- MydataNA_Con_Cluster[,-c(1:length(MydataNA_Con))]
      DatosConConglNA <- data.frame(MydataNA,apply(MisClus_1,2,as.factor))
      DatosConCongl <- na.omit(DatosConConglNA)
      # browser()
      resultados<-list("Conglomerado" = MisClus_1 ,"ResultadosConglom"=resultados,
                       "Indices" = ResultadosIndices, "DatosConCongl"= DatosConCongl,
                       "NombresColCluster" = colnames(DatosConCongl),
                       "DatosConConglNA"= DatosConConglNA, "PCA_Results" = resultado_ms)
      
      
    }
    # rm(".Random.seed")
    # rm(".GlobalEnv$.Random.seed")
    # set.seed(Sys.time())
    ValoresOutput$Clasificacion<-resultados
    return(resultados)
    
  })
  
  
  # output$DatosClusters
  output$corrPlotClasif <- renderPlot({
    if (length(input$rto) == 1) {
      plotClasifVar <-
        ggplot(Clasificacion()$DatosConCongl, aes_string(input$rto)) +
        geom_density()
    } else {
      # browser()
      if (length(input$rto) < 15) {
        plotClasifVar <- ggpairs(Clasificacion()$DatosConCongl,
                                 columns = input$rto,
                                 progress = FALSE)
        
      } else {
        plotClasifVar <- ggpairs(
          Clasificacion()$DatosConCongl,
          columns = input$rto,
          lower = "blank",
          progress = TRUE
        )
        
      }
      
    }
    ValoresOutput$corrVariables <- plotClasifVar
    
    print(plotClasifVar)
  })
  
  output$ClasifMatrCorr <- renderPlot({
    validate(need(
      agrepl("Cluster", input$NumClust),
      "Must select a Cluster column"
    ))
    
    if (length(input$rto) == 1) {
      MatrClasPlot <-
        ggplot(
          Clasificacion()$DatosConCongl,
          aes_string(
            input$rto,
            color = input$NumClust,
            fill = input$NumClust
          )
        ) +
        geom_density()
      
      # print(ggscatmat(Clasificacion()$DatosConCongl,
      #                 columns = input$rto))
    } else {
      if (length(input$rto) < 15) {
        MatrClasPlot <- ggpairs(
          Clasificacion()$DatosConCongl,
          mapping = aes_string(color = input$NumClust),
          columns = input$rto,
          progress = FALSE
        )
        
      } else {
        MatrClasPlot <- ggpairs(
          Clasificacion()$DatosConCongl,
          mapping = aes_string(color = input$NumClust),
          lower = "blank",
          columns = input$rto,
          progress = FALSE
        )
        
        
      }
    }
    
    ValoresOutput$corrVariablesClust <- MatrClasPlot
    
    print(MatrClasPlot)
    
  })
  
  # ### Download Table Cluster
  # Clasificacion()$DatosConCongl
  output$downloadClasification <- downloadHandler(
    filename = function() {
      paste('Clasification-', Sys.Date(), '.txt', sep = '')
    },
    content = function(file) {
      write.table(
        Clasificacion()$DatosConCongl,
        file,
        sep = "\t",
        quote = FALSE,
        row.names = FALSE
      )
    }
  )
  
  
  output$ClasificationPlot <- renderPlotly({
    validate(need(input$file, 'Check input file!'),
             need(input$NumClust, ''))
    
    # build graph with ggplot syntax
    p <-
      ggplot(
        Clasificacion()$DatosConCongl,
        aes_string(
          x = input$xmapa,
          y = input$ymapa,
          colour = input$NumClust
        )
      ) +
      geom_point()
    ggplotly(p) %>%
      plotly::layout(autosize = TRUE)
    
    ValoresOutput$GraficoConglom <- p
    
  })
  #### Cluster Validation -----
  ValidCluster <- reactive({
    validate(
      need(input$NumClust,
           label = "Select the number of cluster to validate in 'Cluster Plot' tab"),
      need(agrepl("Cluster_*", input$NumClust),
           label = "Select a valid number of cluster to validate")
    )
    
    withProgress(message = 'Validating...', value = 0, {
      zoneValidTables <-
        ValidVarKrig(
          ClustersFM =  datos_variables_Valid()$datos,
          datosAValid = datos_variables_Valid()$variables,
          numCluster = input$NumClust,
          EstDesc = VarKrigDescrReac(),
          crs = st_crs(CoordSist_crs())$proj4string
        )#CoordSist_crs())#ValoresOutput$TablaCoordTrans$coordproj)
    })
    ValoresOutput$zoneValidationTables <- zoneValidTables
    return(zoneValidTables)
    
  })
  
  output$clustervalidationTables <- renderUI({
    LL <- vector("list", length(ValidCluster()$Diferencias))
    for (i in seq_len(length(ValidCluster()$Diferencias))) {
      LL[[i]] <-
        fluidRow(
          column(width = 12 / 3,
                 h3(names(
                   ValidCluster()$Diferencias
                 )[i]),
                 DT::dataTableOutput(paste0("dt_", i))),
          column(width = 12 / 4,
                 br(),
                 br(),
                 plotOutput(paste0("plot_", i), height = "190px")),
          br(),
          br()
          
        )
      
      
    }
    return(LL)
    
  })
  
  observeEvent(input$NumClust, {
    sapply(seq_len(length(ValidCluster()$Diferencias)), function(i) {
      id <- paste0("dt_", i)
      output[[id]] <- DT::renderDataTable(
        DT::datatable(
          ValidCluster()$Diferencias[[i]],
          options = list(
            paging = FALSE,
            searching = FALSE,
            autoWidth = TRUE,
            info = FALSE
          ),
          rownames = FALSE,
          selection = 'none'
          
        )
      )

      output[[paste0("plot_", i)]] <-
        renderPlot(makePlotClusterValid(ValidCluster()$Diferencias[[i]]))
    })
    
  })
  
  datos_variables_Valid <- reactive({
    datosAValid_ConSelection <- input$rto
    DatosAValid <- Clasificacion()$DatosConCongl
    # browser()
    if (input$makeSelectProces) {
      datosAValid_ConSelection <-
        unique(c(input$Variable_selection_process, input$rto))
      # datosAValid_ConSelection[!is.null(datosAValid_ConSelection)]
      
      Clasif <- Clasificacion()$DatosConCongl
      DatosOrig <- data()
      DatosOrig <-
        DatosOrig[, unique(c(
          input$Variable_selection_process,
          intersect(names(Clasif), names(DatosOrig))
        ))]
      DatosAValid <- merge(Clasif, DatosOrig, all.x = TRUE)
    }
    return(list("datos" = DatosAValid,
                "variables" = datosAValid_ConSelection))
  })
  
  
  
  VarKrigDescrReac <- reactive({
    VarKrigDescr(
      ClustersFM =  datos_variables_Valid()$datos,
      datosAValid =  datos_variables_Valid()$variables,
      crs = st_crs(CoordSist_crs())$proj4string#ValoresOutput$TablaCoordTrans$coordproj
    )
    
  })
  
  
  
  #########################################
  ##############  REPORTE   #####
  #########################################
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = function() {
      paste('FastMapping-Report', Sys.Date(), sep = '.', switch(
        input$format,
        PDF = 'pdf',
        HTML = 'html',
        Word = 'docx'
      ))
    },
    
    content = function(file) {
      withProgress(message = 'Rendering, please wait!', {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        params <- list(
          input = isolate(reactiveValuesToList(input)),
          output = isolate(reactiveValuesToList(ValoresOutput))
        )
        library(knitr)
        # browser()
        # save(paramts, file="C:/Users/Pablo/Data1.RData")
        
        # Knit the document, passing in the `paramts` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(
          input = tempReport,
          output_format = switch(
            input$format,
            PDF = pdf_document(),
            HTML = html_document(),
            Word = word_document()
          ),
          output_file = file,
          params = params,
          envir = new.env(parent = globalenv())
        )
        
      })
    }
    
  )
  
  
  ValoresOutput <-
    reactiveValues(
      Tabla = NULL,
      TablaCoordTrans = NULL,
      DataDep = NULL,
      NombresCol = NULL,
      SelectedMdls = NULL,
      MiKrige = NULL,
      Variograma = NULL,
      Mygr = NULL,
      kriging = NULL,
      GeoTiff = NULL,
      variogg = NULL,
      Clasificacion = NULL,
      GraficoConglom = NULL,
      corrVariables = NULL,
      corrVariablesClust = NULL,
      zoneValidationTables = NULL
    )
  
  observeEvent(input$rto, {
    shinyjs::show(selector = '#PanelTabSet li a[data-value="DatasetTab"]')
    shinyjs::show(selector = '#PanelTabSet li a[data-value="DepurationTab"]')
    shinyjs::show(selector = '#PanelTabSet li a[data-value="PredictionTab"]')
    shinyjs::show(selector = '#PanelTabSet li a[data-value="ResultsTab"]')
    shinyjs::show(selector = '#PanelTabSet li a[data-value="ClusterTab"]')
    shinyjs::show(selector = '#PanelTabSet li a[data-value="ReportTab"]')
    
    if (is.null(input$rto)) {
      hideTab(inputId = "PanelTabSet", target = "DepurationTab")
      hideTab(inputId = "PanelTabSet", target = "PredictionTab")
      hideTab(inputId = "PanelTabSet", target = "ResultsTab")
      hideTab(inputId = "PanelTabSet", target = "ReportTab")
      hideTab(inputId = "PanelTabSet", target = "ClusterTab")
    }
    
    
    if (length(input$rto) == 1) {
      showTab(inputId = "PanelTabSet", target = "DepurationTab")
      showTab(inputId = "PanelTabSet", target = "PredictionTab")
      showTab(inputId = "PanelTabSet", target = "ResultsTab")
      showTab(inputId = "PanelTabSet", target = "ReportTab")
      showTab(inputId = "PanelTabSet", target = "ClusterTab")
    }
    
    if (length(input$rto) > 1) {
      hideTab(inputId = "PanelTabSet", target = "DepurationTab")
      hideTab(inputId = "PanelTabSet", target = "PredictionTab")
      hideTab(inputId = "PanelTabSet", target = "ResultsTab")
      showTab(inputId = "PanelTabSet", target = "ReportTab")
      showTab(inputId = "PanelTabSet", target = "ClusterTab")
    }
    
    output$tabPanel_title <- renderText({
      if (length(input$rto) > 1) {
        return("Multivariate")
      }
      "Cluster"
      # ifelse(length(input$rto)>1,"Multivariate", "Cluster")
    })
    
  })
  
  ####### Variable Selection process ######
  observeEvent(input$makeSelectProces, {
    showNotification("This process is under development, will be available soon.",
                     type = c("error"))
  }, ignoreInit = TRUE)
  
  
  
  output$SelectProcessUI <-
    renderUI({
      if (input$makeSelectProces) {
        tagList(
          selectInput(
            "Variable_selection_process",
            "Variable selection process",
            choices = names(data())[!names(data()) %in% c(input$xmapa, input$ymapa)],
            multiple = TRUE
          ),
          numericInput(
            "alpha_level_Corr",
            "Significance level",
            value = 0.15,
            min = 0,
            max = 1,
            step = 0.05
          ),
          bsTooltip(
            "Variable_selection_process",
            VariableSelectionProcessHelp,
            placement = "bottom",
            trigger = "hover",
            options = NULL
          ),
          bsTooltip(
            "alpha_level_Corr",
            alpha_corr_Help,
            placement = "bottom",
            trigger = "hover",
            options = NULL
          )
        )
        
      }
      
    })
  
  
  
}
