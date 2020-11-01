library(tidyverse)
library(e1071)
library(moments)
library(gt)
library(ggpubr)
library(ggplotify)
library(grid)
# 1. Creamos las muestras, evaluamos el estadístico en ellas, así como la media muestral

entrega_dos <- function(alpha, beta){
        lista <- list()
        media_poblacional <- alpha / (alpha + beta)
        varianza_poblacional <- alpha * beta / (((alpha + beta) ^ 2 ) * (alpha + beta + 1))
        tamaño_muestral <- c(10, 100, 1000, 10000, 100000)
        lista[[6]] <- matrix(ncol = 40, nrow = length(tamaño_muestral)) 
        
        for(i in seq_along(tamaño_muestral)){
                lista[[i]] <- matrix(ncol = 40, nrow = tamaño_muestral[i]) 
                for(j in 1:40){
                        set.seed(j)
                        lista[[i]][, j] <- rbeta(tamaño_muestral[i], shape1 = alpha, shape2 = beta)
                        colnames(lista[[i]])[j] <- as.numeric(gsub("V", "", colnames(lista[[i]])[j]))
                        colnames(lista[[i]])[j] <-  as.numeric(gsub("V", "", colnames(lista[[i]])[j]))
                        lista[[6]][i, j] <- 0.5*(quantile(lista[[i]][, j], probs = c(0.6)) + 
                                                        quantile(lista[[i]][, j], probs = c(0.4))) 
                }
        }
        
        
        rownames(lista[[6]]) <- tamaño_muestral
        # Esperanza estadístico
        lista[[6]] <- t(lista[[6]])
        lista[[7]] <- t(colMeans(lista[[6]])) %>% unname() %>% as.numeric()
        lista[[7]] <- data.frame(tamaño_muestral = as.character(tamaño_muestral), media_estadistico = lista[[7]])
        
        # Varianza estadístico
        lista[[8]] <- data.frame(tamaño_muestral = as.character(tamaño_muestral), varianza_estadistico = as.numeric(unname(apply(lista[[6]], 2, var))))
        lista[[7]] <- merge(lista[[7]], lista[[8]])
        lista[[8]] <- NULL
        
        # Sesgo, precisión, ECM, consistencia (implicita, ya que hemos calculado la varianza y el sesgo)
        for(i in 1:nrow(lista[[7]])){
                lista[[7]][i, 4] <- lista[[7]][i, 2] - media_poblacional
                lista[[7]][i, 5] <- 1/(lista[[7]][i, 3]) # precision = 1/var(t)
                lista[[7]][i, 6] <- lista[[7]][i, 3] + (lista[[7]][i, 4])^2 # ecm(t) = varianza(t) + (sesgo(t))^2
        }
        colnames(lista[[7]]) <- c("tamaño_muestral", "esperanza_estadistico", 
                                  "varianza_estadistico", "sesgo_estadístico", 
                                  "precisión_estadístico", "ECM_estadístico")
        
        
        
        # Falta invarianza y suficiencia
        
        # Robustez
        distribucion_muestral_beta <- as.data.frame(matrix(nrow = 40, ncol = 10))
        distribucion_muestral_uniforme <- as.data.frame(matrix(nrow = 40, ncol = 10))
        distribucion_muestral_contaminada <- as.data.frame(matrix(nrow = 40, ncol = 10))
        estadistico <- as.data.frame(matrix(nrow = 1, ncol = 40))
        media_muestral <- as.data.frame(matrix(nrow = 1, ncol = 40))
        porcentaje_contaminacion <- 0.9
        for(j in 1:40){
                distribucion_muestral_beta[, j] <- rbeta(10, shape1 = alpha, shape2 = beta)
                distribucion_muestral_uniforme[, j] <- runif(10, min = 2, max = 3)
                distribucion_muestral_contaminada[, j] <- (porcentaje_contaminacion * distribucion_muestral_beta[, j]) + 
                        ((1 - porcentaje_contaminacion) * distribucion_muestral_beta[, j])
                colnames(distribucion_muestral_beta)[j] <-  as.numeric(gsub("V", "", colnames(distribucion_muestral_beta)[j]))
                colnames(distribucion_muestral_uniforme)[j] <-  as.numeric(gsub("V", "", colnames(distribucion_muestral_uniforme)[j]))
                colnames(distribucion_muestral_contaminada)[j] <-  as.numeric(gsub("V", "", colnames(distribucion_muestral_contaminada)[j]))
                estadistico[, j] <- 0.5*(quantile(distribucion_muestral_contaminada[, j], probs = c(0.6)) + 
                                                quantile(distribucion_muestral_contaminada[, j], probs = c(0.4)))
        }
        media_muestral <- data.frame(Muestra = names(colMeans(distribucion_muestral_contaminada)), 
                                     mediamuestral = unname(colMeans(distribucion_muestral_contaminada))) 
        lista[[8]] <- distribucion_muestral_contaminada
        lista[[9]] <- gather(as.data.frame(estadistico))
        lista[[9]][, 1] <- 1:nrow(lista[[9]])
        colnames(lista[[9]]) <- c("Muestra", "Estadistico")
        
        
        lista[[10]] <- data.frame(Estimador = "Estadístico",
                                  Media = mean(lista[[9]][, 2]),
                                  Mediana = median(lista[[9]][, 2]),
                                  SD = sd(lista[[9]][, 2]), 
                                  IQR = IQR(lista[[9]][, 2]), 
                                  MAD = mad(lista[[9]][, 2]), 
                                  Curtosis = moments::kurtosis(lista[[9]][, 2]), 
                                  Asimetría = e1071::skewness(lista[[9]][, 2]))
        
        lista[[11]] <- media_muestral
        
        lista[[12]] <- data.frame(Estimador = "Media Muestral", 
                                  Media = mean(lista[[11]][, 2]),
                                  Mediana = median(lista[[11]][, 2]),
                                  SD = sd(lista[[11]][, 2]), 
                                  IQR = IQR(lista[[11]][, 2]), 
                                  MAD = mad(lista[[11]][, 2]), 
                                  Curtosis = moments::kurtosis(lista[[11]][, 2]), 
                                  Asimetría = e1071::skewness(lista[[11]][, 2]))
        
        
        lista[[13]] <- rbind(lista[[10]], lista[[12]])
        names(lista) <- c("muestras_10", "muestras_100", "muestras_1000", "muestras_10000", "muestras_100000",
                          "estadístico_n", "propiedades_estimador", "distribucion_contaminada", 
                          "estadistico", "medidas_estadistico", "media_muestral", "medidas_mediamuestral", "tabla_comparacion")
        

        lista
}

resultados <- entrega_dos(alpha = 2, beta = 2)




