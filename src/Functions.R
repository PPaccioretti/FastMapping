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

library(shiny)
library(shinythemes)

# Moran Plot
moran.plot1 <-function (x, listw, zero.policy = NULL, spChk = NULL, labels = NULL,
                        xlab = NULL, ylab = NULL, quiet = NULL, ...){
  # browser()
  if (!inherits(listw, "listw"))
    stop(paste(deparse(substitute(listw)), "is not a listw object"))
  if (is.null(quiet))
    quiet <- !get("verbose", envir = .spdepOptions)
  stopifnot(is.vector(x))
  stopifnot(is.logical(quiet))
  if (is.null(zero.policy))
    zero.policy <- get("zeroPolicy", envir = .spdepOptions)
  stopifnot(is.logical(zero.policy))
  xname <- deparse(substitute(x))
  if (!is.numeric(x))
    stop(paste(xname, "is not a numeric vector"))
  if (any(is.na(x)))
    stop("NA in X")
  n <- length(listw$neighbours)
  if (n != length(x))
    stop("objects of different length")
  if (is.null(spChk))
    spChk <- get.spChkOption()
  if (spChk && !chkIDs(x, listw))
    stop("Check of data and weights ID integrity failed")
  labs <- TRUE
  if (is.logical(labels) && !labels)
    labs <- FALSE
  if (is.null(labels) || length(labels) != n)
    labels <- as.character(attr(listw, "region.id"))
  wx <- lag.listw(listw, x, zero.policy = zero.policy)
  if (is.null(xlab))
    xlab <- xname
  if (is.null(ylab))
    ylab <- paste("spatially lagged", xname)
  if (zero.policy) {
    n0 <- wx == 0
    if (any(n0)) {
      symbols(x[n0], wx[n0], inches = FALSE, circles = rep(diff(range(x))/50,
                                                           length(which(n0))), bg = "grey", add = TRUE)
    }
  }
  xwx.lm <- lm(wx ~ x)
  infl.xwx <- influence.measures(xwx.lm)
  is.inf <- which(apply(infl.xwx$is.inf, 1, any))
  if (!quiet)
    summary(infl.xwx)
  invisible(infl.xwx)
}



fclustIndex_modif <- function (y, x, index = "all"){
  clres <- y
  gath.geva <- function(clres, x) {
    xrows <- dim(clres$me)[1]
    xcols <- dim(clres$ce)[2]
    ncenters <- dim(clres$centers)[1]
    scatter <- array(0, c(xcols, xcols, ncenters))
    scatternew <- array(0, c(xcols, xcols, ncenters))
    fhv <- as.double(0)
    apd <- as.double(0)
    pd <- as.double(0)
    control <- as.double(0)
    for (i in 1:ncenters) {
      paronomastis <- as.double(0)
      paronomastis2 <- as.double(0)
      for (j in 1:xrows) {
        paronomastis <- paronomastis + clres$me[j, i]
        diff <- x[j, ] - clres$ce[i, ]
        scatternew[, , i] <- clres$me[j, i] * (t(t(diff)) %*%
                                                 t(diff))
        scatter[, , i] <- scatter[, , i] + scatternew[,
                                                      , i]
      }
      scatter[, , i] <- scatter[, , i]/paronomastis
      for (j in 1:xrows) {
        diff <- x[j, ] - clres$ce[i, ]
        control <- (t(diff) %*% solve(scatter[, , i])) %*%
          t(t(diff))
        if (control < 1)
          paronomastis2 <- paronomastis2 + clres$me[j,
                                                    i]
      }
      fhv <- fhv + sqrt(det(scatter[, , i]))
      apd <- apd + paronomastis2/sqrt(det(scatter[, , i]))
      pd <- pd + paronomastis2
    }
    pd <- pd/fhv
    apd <- apd/ncenters
    retval <- list(fuzzy.hypervolume = fhv, average.partition.density = apd,
                   partition.density = pd)
    return(retval)
  }
  xie.beni <- function(clres) {
    xrows <- dim(clres$me)[1]
    minimum <- -1
    error <- clres$within
    ncenters <- dim(clres$centers)[1]
    for (i in 1:(ncenters - 1)) {
      for (j in (i + 1):ncenters) {
        diff <- clres$ce[i, ] - clres$ce[j, ]
        diffdist <- t(diff) %*% t(t(diff))
        if (minimum == -1)
          minimum <- diffdist
        if (diffdist < minimum)
          minimum <- diffdist
      }
    }
    xiebeni <- error/(xrows * minimum)
    return(xiebeni)
  }
  fukuyama.sugeno <- function(clres) {
    xrows <- dim(clres$me)[1]
    ncenters <- dim(clres$centers)[1]
    error <- clres$within
    k2 <- as.double(0)
    meancenters <- apply(clres$ce, 2, mean)
    for (i in 1:ncenters) {
      paronomastis3 <- as.double(0)
      for (j in 1:xrows) {
        paronomastis3 <- paronomastis3 + (clres$me[j,
                                                   i]^2)
      }
      diff <- clres$ce[i, ] - meancenters
      diffdist <- t(diff) %*% t(t(diff))
      k2 <- k2 + paronomastis3 * diffdist
    }
    fukuyamasugeno <- error - k2
    return(fukuyamasugeno)
  }
  partition.coefficient <- function(clres) {
    xrows <- dim(clres$me)[1]
    partitioncoefficient <- sum(apply(clres$me^2, 1, sum))/xrows
    partitioncoefficient <- 1/partitioncoefficient
    return(partitioncoefficient)
  }
  partition.entropy <- function(clres) {
    xrows <- dim(clres$me)[1]
    ncenters <- dim(clres$centers)[1]
    partitionentropy <- 0
    for (i in 1:xrows) {
      for (k in 1:ncenters) {
        if (clres$me[i, k] != 0)
          partitionentropy <- partitionentropy + (clres$me[i,
                                                           k] * log(clres$me[i, k]))
      }
    }
    partitionentropy <- partitionentropy/((-1) * xrows)
    return(partitionentropy)
  }
  separation.index <- function(clres, x) {
    xrows <- dim(clres$me)[1]
    xcols <- dim(x)[2]
    ncenters <- dim(clres$centers)[1]
    maxcluster <- double(ncenters)
    minimum <- -1
    for (i in 1:ncenters) {
      maxcluster[i] <- max(dist(matrix(x[clres$cl == i],
                                       ncol = xcols)))
    }
    maxdia <- maxcluster[rev(order(maxcluster))[1]]
    for (i in 1:(ncenters - 1)) {
      for (j in (i + 1):(ncenters)) {
        for (m in 1:xrows) {
          if (clres$cl[m] == i) {
            for (l in 1:xrows) {
              if (clres$cl[l] == j) {
                diff <- x[m, ] - x[l, ]
                diffdist <- sqrt(t(diff) %*% t(t(diff)))
                fraction <- diffdist/maxdia
                if (minimum == -1)
                  minimum <- fraction
                if (fraction < minimum)
                  minimum <- fraction
              }
            }
          }
        }
      }
    }
    return(minimum)
  }
  proportion.exponent <- function(clres) {
    k <- dim(clres$centers)[2]
    xrows <- dim(clres$me)[1]
    bexp <- as.integer(1)
    for (j in 1:xrows) {
      greatint <- as.integer(1/max(clres$me[j, ]))
      aexp <- as.integer(0)
      for (l in 1:greatint) {
        aexp <- aexp + (-1)^(l + 1) * (gamma(k + 1)/(gamma(l +
                                                             1) * gamma(k - l + 1))) * (1 - l * max(clres$me[j,
                                                                                                             ]))^(k - 1)
      }
      bexp <- bexp * aexp
    }
    proportionexponent <- -log(bexp)
    return(proportionexponent)
  }
  index <- pmatch(index, c("gath.geva", "xie.beni", "fukuyama.sugeno",
                           "partition.coefficient", "partition.entropy", "proportion.exponent",
                           "separation.index", "all"))
  if (any(is.na(index)))
    stop("invalid clustering index")
  if (any(index == -1))
    stop("ambiguous index")
  vecallindex <- numeric(9)

  if (any(index == 1) || (index == 8)) {
    gd <- gath.geva(clres, x)
    vecallindex[1] <- gd$fuzzy
    vecallindex[2] <- gd$average
    vecallindex[3] <- gd$partition
  }
  if (any(index == 2) || (index == 8))
    vecallindex[4] <- xie.beni(clres)
  if (any(index == 3) || (index == 8))
    vecallindex[5] <- fukuyama.sugeno(clres)
  if (any(index == 4) || (index == 8))
    vecallindex[6] <- partition.coefficient(clres)
  if (any(index == 5) || (index == 8))
    vecallindex[7] <- partition.entropy(clres)
  if (any(index == 6) || (index == 8))
    vecallindex[8] <- proportion.exponent(clres)
  if (any(index == 7) || (index == 8))
    vecallindex[9] <- separation.index(clres, x)
  names(vecallindex) <- c("fhv", "apd", "pd", "xb", "fs", "pc",
                          "pe", "pre", "si")
  if (any(index < 8)) {
    if (any(index == 1))
      vecallindex <- vecallindex[1:3]
    else vecallindex <- vecallindex[index + 2]
  }
  return(vecallindex)
}


CalcLetras <- function(MisMedias, pvalue, alpha = 0.05) {
  #Adapted from agricolae package
  lastC <-
    function(x) {
      y<-sub(" +$", "",x)
      p1<-nchar(y)
      cc<-substr(y,p1,p1)
      return(cc)
    }
  
  
  Q <- matrix(1, ncol = nrow(MisMedias), nrow = nrow(MisMedias))
  p <- pvalue
  k <- 0
  for (i in 1:(nrow(MisMedias) - 1)) {
    for (j in (i + 1):nrow(MisMedias)) {
      k <- k + 1
      Q[i, j] <- p[k]
      Q[j, i] <- p[k]
    }
  }
  
  
  
  n <- nrow(MisMedias)
  z <- MisMedias
  letras<-c(letters[1:26],LETTERS[1:26],1:9,c(".","+","-","*","/","#","$",
                                              "%","&","^","[","]",":","@",";","_","?","!","=","#",rep(" ",2000)))
  w <- z[order(z[[ 2]], decreasing = FALSE), ]
  M<-rep("",n)
  k<-1
  k1<-0
  j<-1
  i<-1
  cambio<-n
  cambio1<-0
  chequeo=0
  M[1]<-letras[k]
  q <- as.numeric(rownames(w)) #Check
  while(j<n) {
    chequeo<-chequeo+1
    if (chequeo > n) break
    for(i in j:n) {
      s<-Q[q[i],q[j]]>alpha
      if(s) {
        if(lastC(M[i]) != letras[k]) M[i]<-paste(M[i],letras[k],sep="")
      }
      else {
        k<-k+1
        cambio<-i
        cambio1<-0
        ja<-j
        for(jj in cambio:n) M[jj]<-paste(M[jj],"",sep="") # El espacio
        M[cambio]<-paste(M[cambio],letras[k],sep="")
        for( v in ja:cambio) {
          if(Q[q[v],q[cambio]]<=alpha) {j<-j+1
          cambio1<-1
          }
          else break
        }
        break
      }
    }
    if (cambio1 ==0 )j<-j+1
  }
  #-----------
  w<-data.frame(w,stat=M)
  if(k>81) 
    cat("\n",k,"groups are estimated.The number of groups exceeded the maximum of 81 labels. change to group=FALSE.\n")
  invisible(w)
}


makeMeanComparisson <-
  function(VariableEstudiada,
           Clasif,
           numCluster,
           EstDescr = EstDesc ,
           alpha = 0.05,
           retDMS = FALSE) {
    Medias <-
      Clasif %>%
      group_by(!!rlang::sym(numCluster)) %>%
      summarize_at(VariableEstudiada, mean)
    ## Test ----
    
    MisMedias <- Medias[, c(numCluster, VariableEstudiada)] %>%
      arrange(!!rlang::sym(VariableEstudiada)) #%>%
    # mutate(DifSign = do.call(rbind, Krig)[ VariableEstudiada, 2] * 1.96 * 2)
    
    
    
    comb <- utils::combn(nrow(MisMedias), 2)
    nn <- ncol(comb)
    dif <- rep(0, nn)
    pvalue <- dif
    sdtdif <- dif
    sig <- rep(" ", nn)
    DMS <- EstDescr[VariableEstudiada, 2] * qnorm(1 - alpha / 2) * 2
    
    for (k in 1:nn) {
      i <- comb[1, k]
      j <- comb[2, k]
      dif[k] <- MisMedias[i, 2] - MisMedias[j, 2]
      sdtdif[k] <- EstDescr[VariableEstudiada, 2]
      pvalue[k] <- abs(dif[k][[1]]) <= DMS
    }
    
    MisLetras <-
      CalcLetras(MisMedias = MisMedias,
                 pvalue = pvalue,
                 alpha = alpha)
    if (retDMS) {
      return(list("TablaMedias" = MisLetras, "DMS" = DMS))
      }
    MisLetras
  }

VarKrigDescr <-
  function(ClustersFM,
           datosAValid,
           crs) {
    datos_predsf <- st_as_sf(ClustersFM, coords = 1:2, crs = crs)
    Krig <-
      sapply(datosAValid, function(columna) {
        # browser()
        if (columna == "geometry") {
          return(NULL)
        }
        
        formulaKr <- as.formula(paste(columna, "~ 1"))
        
        # Ajuste de semivariograma empírico
        semiv_emp <- variogram(formulaKr, datos_predsf)
        
        # Ajuste de semivariograma teóricos sin valores iniciales de los parámetros
        modelos <-
          fit.variogram(semiv_emp, vgm(c("Exp", "Sph", "Gau")))
        kriging_cv <-
          krige.cv(
            formulaKr,
            datos_predsf,
            nfold = 10,
            nmin = 7,
            nmax = 25,
            model = modelos
          )
        
        media <- mean(datos_predsf[[columna]], na.rm = T)
        Cv <-
          sd(datos_predsf[[columna]], na.rm = T) / media * 100
        tamPunt <- sum(!is.na(datos_predsf[[columna]]))
        data.frame(
          "Variable" = as.character(columna),
          "MedianaK" = median(sqrt(kriging_cv$var1.var)),
          "MediaVar" = media,
          "CVVar" = Cv,
          "n" = tamPunt
        )
        
      }, simplify = FALSE)
    
    #### Est Descriptiva  ----
    # AreaP <- datos_predsf %>%
    #   summarise(geometry = st_combine(geometry)) %>%
    #   st_convex_hull() %>%
    #   st_area()
    # # browser()
    # AreaHa <- AreaP / 10000
    
    EstDesc <- do.call(rbind, Krig)
    EstDesc
  }


ValidVarKrig <-
  function(ClustersFM,
           datosAValid,
           numCluster,
           crs,
           EstDesc,
           ndeci = 2) {
    MedCV <-
      paste0(round(EstDesc$MediaVar, 2), " (", round(EstDesc$CVVar, 1), ")")
    MedCV <-
      data.table("Variable" = EstDesc$Variable,
                 MedCV,
                 "n" = EstDesc$n)
    
    EstDescTable <- dcast(data = MedCV,
                     n ~ Variable ,
                     value.var = "MedCV")
    
    ##### ComparacionMedias ----
    
    MeansComparissons <-
      sapply(
        datosAValid,
        makeMeanComparisson,
        Clasif = ClustersFM,
        numCluster = numCluster,
        EstDescr = EstDesc,
        retDMS = FALSE,
        simplify = FALSE
      )
    
    return(list(Diferencias = MeansComparissons,
                Descriptivo = EstDescTable))
  }


makePlotClusterValid <- function(datos, colCluster = 1, colMean = 2, colLetters = 3) {
  # browser()
  MyColumns <- names(datos)
  MyClusterName <- MyColumns[colCluster]
  MyMeanName <-  MyColumns[colMean]
  MyLettersName <-  MyColumns[colLetters]
  ggpl <- ggplot(datos,
         aes_string(x = MyClusterName, y = MyMeanName)) +
    geom_point(shape = 15, size = 2) +
    geom_text(aes_string(label = MyLettersName), nudge_y = 0.05, nudge_x = 0.05)
  print(ggpl)
}
 
