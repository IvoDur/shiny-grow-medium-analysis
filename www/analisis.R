## Creacion del data para el análisis a traves de los parametros de la app
analisis <- function(capacidad, matriz){
  Analista <- c(
    rep("Analista 1", 6), 
    rep("Analista 2", 6), 
    rep("Analista 3", 6)
  )
  
  Muestra <- c(
    rep(c("Media", "Alta"), 9)
  )
  
  CRH <- c(
    rep(
      c(
        rep(paste0(capacidad-5, "%"), 2), 
        rep(paste0(capacidad, "%"), 2), 
        rep(paste0(capacidad+5, "%"), 2)
      ), 
    3)
  )
  
  PlNorm <- c(t(matriz)[, 1], t(matriz)[, 2], t(matriz)[, 3])
  
  ## Datos de prueba para no depender de la app o bien si quiero renderizar solo este Rmd (comentar de ser necesario)
  # PlNorm = c(77, 90, 81, 91, 69, 93, 80, 93, 74, 92, 67, 86, 81, 90, 83, 88, 73, 91)
  
  Datos <- as.data.frame(cbind(PlNorm, Analista, CRH, Muestra)) %>% 
    mutate(PlNorm = as.numeric(PlNorm))
  
  
  ## -----------------------------------------------------------------------------
  
  
  ## Análisis estadísticos necesarios
  SHAPIRO.p <- shapiro.test(resid(aov(PlNorm ~ Muestra + Analista + CRH, data = Datos)) / sd(resid(aov(PlNorm ~ Muestra + Analista + CRH, data = Datos))))$p.value 
  BARTLETT.p <- bartlett.test(PlNorm ~ CRH, data = Datos)[["p.value"]]
  KRUSKALL.p <- kruskal.test(Datos$PlNorm, Datos$CRH)$p.value
  ANOVA.p <- summary(aov(PlNorm ~ Muestra + Analista + CRH, data = Datos))[[1]]$`Pr(>F)`[3]
  MEDIANS <- aggregate(PlNorm ~ CRH, data = Datos, FUN = mean)
  MEDIAN.max <- MEDIANS$CRH[MEDIANS$PlNorm == max(MEDIANS$PlNorm)]
  
  
  ## -----------------------------------------------------------------------------
  
  
  ## Loop para imprimir soluciones
  
  # Si se cumplen los supuestos para el ANOVA
  if(SHAPIRO.p > .05 && BARTLETT.p > .05) {
    
    # No hay diferencias significativas -> Elijo el de mayor mediana
    if(KRUSKALL.p > .05 && ANOVA.p > .05) {
      
      # Tengo que considerar los casos en que las medianas coincidan
      if(nchar(MEDIAN.max) == 1) {
        resultadoApp <- paste0("Estadísticamente, se recomienda utilizar una CRH de ", MEDIAN.max, "% porque es la que provocó un porcentaje de plántulas normales mediano mayor.")
      }
      else if(nchar(MEDIAN.max) == 2) {
        resultadoApp <- paste0("Estadísticamente, se recomienda utilizar una CRH de ", MEDIAN.max[1], "% o de ", MEDIAN.max[1],"%  porque son las que provocaron un porcentaje de plántulas normales mediano mayor.")
      }
      else {
        resultadoApp <- paste0("Estadísticamente, las tres capacidades provocaron el mismo porcentaje de plántulas normales mediano. Cualquiera de las tres puede elegirse.")
      }
      
      # Hay diferencia en las medianas pero no en las medias -> Concluyo en funcion del test de DUNN
    } else if (KRUSKALL.p < .05 && ANOVA.p > .05) {
      
      # Loop para encontrar el o los mejores
      DUNN <- dunn.test(Datos$PlNorm, Datos$CRH, method = "bh", altp = T)
      mejor.DUNN <- character()
      for (i in 1:3) {
        if (DUNN[["altP.adjusted"]][i] < .05) {
          if (DUNN[["Z"]][i] > 0) {
            mejor.DUNN <- c(mejor.DUNN, str_split_i(DUNN[["comparisons"]][i], " ", 1))
          } else {
            mejor.DUNN <- c(mejor.DUNN, str_split_i(DUNN[["comparisons"]][i], " ", 3))
          }
        }
      }
      
      # Loop para que imprima correctamente segun si hay uno o dos mejores
      if (length(mejor.DUNN) == 2) {
        if(mejor.DUNN[1] == mejor.DUNN[2]) {
          mejor.DUNN <- paste0(mejor.DUNN[1], " porque es la que provocó")
        } else {
          mejor.DUNN <- paste0(mejor.DUNN[1], " o ", mejor.DUNN[2], " porque son las que provocaron")
        }
      } else if (length(mejor.DUNN) == 3) {
        if(mejor.DUNN[1] == mejor.DUNN[2]) {
          mejor.DUNN <- mejor.DUNN[1]
        } else if(mejor.DUNN[1] == mejor.DUNN[3]) {
          mejor.DUNN <- mejor.DUNN[1]
        } else if(mejor.DUNN[2] == mejor.DUNN[3]) {
          mejor.DUNN <- mejor.DUNN[2]
        }
      } else {
        mejor.DUNN <- mejor.DUNN
      }
      
      resultadoApp <- paste0("Estadísticamente, se recomienda utilizar una CRH de ", mejor.DUNN, " un porcentaje de plántulas normales mediano estadísticamente mayor.")
      
      # Hay diferencia en las medias pero no en las medianas -> Concluyo en funcion del test de TUKEY
    } else if (KRUSKALL.p > .05 && ANOVA.p < .05) {
      
      # Loop para encontrar el o los mejores
      TUKEY <- emmeans(aov(PlNorm ~ Muestra + CRH, data = Datos), pairwise ~ CRH, adjust = "tukey")
      mejor.TUKEY <- character()
      for (i in 1:3) {
        if (summary(TUKEY)$contrasts$p.value[i] < .05) {
          if (summary(TUKEY)$contrasts$estimate[i] > 0) {
            mejor.TUKEY <- c(mejor.TUKEY, str_split_i(TUKEY[["contrasts"]]@levels[["contrast"]][i], " ", 1))
          } else {
            mejor.TUKEY <- c(mejor.TUKEY, str_split_i(TUKEY[["contrasts"]]@levels[["contrast"]][i], " ", 3))
          }
        }
      }
      
      # Loop para que imprima correctamente segun si hay uno o dos mejores
      if (length(mejor.TUKEY) == 2) {
        if(mejor.TUKEY[1] == mejor.TUKEY[2]) {
          mejor.TUKEY <- paste0(mejor.TUKEY[1], " porque es la que provocó")
        } else {
          mejor.TUKEY <- paste0(mejor.TUKEY[1], " o ", mejor.TUKEY[2], " porque son las que provocaron")
        }
      } else if (length(mejor.TUKEY) == 3) {
        if(mejor.TUKEY[1] == mejor.TUKEY[2]) {
          mejor.TUKEY <- mejor.TUKEY[1]
        } else if(mejor.TUKEY[1] == mejor.TUKEY[3]) {
          mejor.TUKEY <- mejor.TUKEY[1]
        } else if(mejor.TUKEY[2] == mejor.TUKEY[3]) {
          mejor.TUKEY <- mejor.TUKEY[2]
        }
      } else {
        mejor.TUKEY <- mejor.TUKEY
      }
      
      resultadoApp <- paste0("Estadísticamente, se recomienda utilizar una CRH de ", mejor.TUKEY, " un porcentaje de plántulas normales medio estadísticamente mayor.")
      
      # Hay diferencia en las medias y en las medianas -> Concluyo en funcion del test de TUKEY    
    } else {
      
      # Loop para encontrar el o los mejores
      TUKEY <- emmeans(aov(PlNorm ~ Muestra + CRH, data = Datos), pairwise ~ CRH, adjust = "tukey")
      mejor.TUKEY <- character()
      for (i in 1:3) {
        if (summary(TUKEY)$contrasts$p.value[i] < .05) {
          if (summary(TUKEY)$contrasts$estimate[i] > 0) {
            mejor.TUKEY <- c(mejor.TUKEY, str_split_i(TUKEY[["contrasts"]]@levels[["contrast"]][i], " ", 1))
          } else {
            mejor.TUKEY <- c(mejor.TUKEY, str_split_i(TUKEY[["contrasts"]]@levels[["contrast"]][i], " ", 3))
          }
        }
      }
      
      # Loop para que imprima correctamente segun si hay uno o dos mejores
      if (length(mejor.TUKEY) == 2) {
        if(mejor.TUKEY[1] == mejor.TUKEY[2]) {
          mejor.TUKEY <- paste0(mejor.TUKEY[1], " porque es la que provocó")
        } else {
          mejor.TUKEY <- paste0(mejor.TUKEY[1], " o ", mejor.TUKEY[2], " porque son las que provocaron")
        }
      } else if (length(mejor.TUKEY) == 3) {
        if(mejor.TUKEY[1] == mejor.TUKEY[2]) {
          mejor.TUKEY <- mejor.TUKEY[1]
        } else if(mejor.TUKEY[1] == mejor.TUKEY[3]) {
          mejor.TUKEY <- mejor.TUKEY[1]
        } else if(mejor.TUKEY[2] == mejor.TUKEY[3]) {
          mejor.TUKEY <- mejor.TUKEY[2]
        }
      } else {
        mejor.TUKEY <- mejor.TUKEY
      }
      
      resultadoApp <- paste0("Estadísticamente, se recomienda utilizar una CRH de ", mejor.TUKEY, " un porcentaje de plántulas normales medio estadísticamente mayor.")
      
    }
    
    # Si no se cumplen los supuestos para el ANOVA
  } else {
    
    # No hay diferencias significativas -> Elijo el de mayor mediana
    if(KRUSKALL.p > .05) {
      
      # Tengo que considerar los casos en que las medianas coincidan
      if(nchar(MEDIAN.max) == 1) {
        resultadoApp <- paste0("Estadísticamente, se recomienda utilizar una CRH de ", MEDIAN.max, "% porque es la que provocó un porcentaje de plántulas normales mediano mayor en este experimento.")
      }
      else if(nchar(MEDIAN.max) == 2) {
        resultadoApp <- paste0("Estadísticamente, se recomienda utilizar una CRH de ", MEDIAN.max[1], "% o de ", MEDIAN.max[1],"%  porque son las que provocaron un porcentaje de plántulas normales mediano mayor en este experimento.")
      }
      else {
        resultadoApp <- paste0("Estadísticamente, las tres capacidades provocaron el mismo porcentaje de plántulas normales mediano. Cualquiera de las tres puede elegirse.")
      }
      
      # Hay diferencias significativas -> Concluyo en funcion del test de DUNN
    } else {
      
      # Loop para encontrar el o los mejores
      DUNN <- dunn.test(Datos$PlNorm, Datos$CRH, method = "bh", altp = T)
      mejor.DUNN <- character()
      for (i in 1:3) {
        if (DUNN[["altP.adjusted"]][i] < .05) {
          if (DUNN[["Z"]][i] > 0) {
            mejor.DUNN <- c(mejor.DUNN, str_split_i(DUNN[["comparisons"]][i], " ", 1))
          } else {
            mejor.DUNN <- c(mejor.DUNN, str_split_i(DUNN[["comparisons"]][i], " ", 3))
          }
        }
      }
      
      # Loop para que imprima correctamente segun si hay uno o dos mejores
      if (length(mejor.DUNN) == 2) {
        if(mejor.DUNN[1] == mejor.DUNN[2]) {
          mejor.DUNN <- paste0(mejor.DUNN[1], " porque es la que provocó")
        } else {
          mejor.DUNN <- paste0(mejor.DUNN[1], " o ", mejor.DUNN[2], " porque son las que provocaron")
        }
      } else if (length(mejor.DUNN) == 3) {
        if(mejor.DUNN[1] == mejor.DUNN[2]) {
          mejor.DUNN <- mejor.DUNN[1]
        } else if(mejor.DUNN[1] == mejor.DUNN[3]) {
          mejor.DUNN <- mejor.DUNN[1]
        } else if(mejor.DUNN[2] == mejor.DUNN[3]) {
          mejor.DUNN <- mejor.DUNN[2]
        }
      } else {
        mejor.DUNN <- mejor.DUNN
      }
      
      resultadoApp <- paste0("Estadísticamente, se recomienda utilizar una CRH de ", mejor.DUNN, " un porcentaje de plántulas normales mediano estadísticamente mayor.")
    }
  }
  return(resultadoApp)
}