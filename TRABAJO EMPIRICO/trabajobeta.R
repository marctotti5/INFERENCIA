
library(tidyverse)
library(e1071)
library(moments)

# 1. Creamos las muestras, evaluamos el estadístico en ellas, así como la media muestral
muestreo <- function(alpha, beta){
        lista <- list()
        tabla_muestras <- matrix(ncol = 40, nrow = 10) %>% as.data.frame()
        lista[[5]] <- matrix(ncol = 40, nrow = 1) %>% as.data.frame()
        media_poblacional <- alpha / (alpha + beta)
        varianza_poblacional <- alpha * beta / (((alpha + beta) ^ 2 ) * (alpha + beta + 1))
        for(j in 1:40){
                set.seed(j)
                tabla_muestras[, j] <- rbeta(10, shape1 = alpha, shape2 = beta)
                colnames(tabla_muestras)[j] <- as.numeric(gsub("V", "", colnames(tabla_muestras)[j]))
                colnames(lista[[5]])[j] <-  as.numeric(gsub("V", "", colnames(tabla_muestras)[j]))
                lista[[5]][, j] <- 0.5*(quantile(tabla_muestras[, j], probs = c(0.6)) + quantile(tabla_muestras[, j], probs = c(0.4)))
        }
        lista[[1]] <- tabla_muestras
        lista[[2]] <- media_poblacional
        lista[[3]] <- varianza_poblacional
        lista[[4]] <- data.frame(Muestra = names(colMeans(tabla_muestras)), mediamuestral = unname(colMeans(tabla_muestras))) 
        lista[[5]] <- gather(as.data.frame(lista[[5]])) 
        colnames(lista[[5]]) <- c("Muestra", "Estadístico")
        lista[[6]] <- mean(lista[[4]][, 2])
        lista[[7]] <- sd(lista[[4]][, 2])
        lista[[8]] <- IQR(lista[[4]][, 2])
        lista[[9]] <- mad(lista[[4]][, 2])
        lista[[10]] <- moments::kurtosis(lista[[4]][, 2])
        lista[[11]] <- e1071::skewness(lista[[4]][, 2])
        lista[[12]] <- mean(lista[[5]][, 2])
        lista[[13]] <- sd(lista[[5]][, 2])
        names(lista) <- c("tabla_muestras", "media_poblacional", "varianza_poblacional", "media_muestral", 
                          "estadistico", "media_mediamuestral", "desviacion_mediamuestral", "IQR_mediamuestral", "MAD_mediamuestral", 
                          "kurtosis_mediamuestral", "asimetria_mediamuestral", "media_estadistico", "desviacion_estadistico")
        lista
}



resultados <- muestreo(alpha = 2, beta = 2)

datosgrafico <- inner_join(resultados$media_muestral , resultados$estadistico)

# 2. Gráficos sobre nuestro estimador: analizar y calcular otras medidas (de centralización y dispersión), hacer boxplots, hacer gráficos de los residuos, etc

grafico_estadistico <- ggplot(data = datosgrafico, aes(x = Estadístico)) + geom_density(fill = "lightblue") + 
        ggtitle("Distribución en el Muestreo de nuestro Estadístico") + 
        geom_vline(aes(xintercept = resultados$media_estadistico, col = "Estimador"), linetype = "dashed", size = 1) +
        geom_vline(aes(xintercept = resultados$media_poblacional,  col = "Media Poblacional"), size = 1) + 
        scale_color_manual(name = "", values = c("Media Poblacional" = "black", Estimador = "red")) + 
        xlab("x") + ylab("f(x)") +
        theme(legend.position = "top",
              legend.text = element_text(size = 14),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              axis.title = element_text(size = 13, face = "bold"), 
              plot.title = element_text(size = 18, face = 'bold', hjust = 0.5)) + xlim(0, 1)

grafico_estadistico_zoom <- ggplot(data = datosgrafico, aes(x = Estadístico)) + geom_density(fill = "lightblue") + 
        ggtitle("Distribución en el Muestreo de nuestro Estadístico") + 
        geom_vline(aes(xintercept = resultados$media_estadistico, col = "Estimador"), linetype = "dashed", size = 1) +
        geom_vline(aes(xintercept = resultados$media_poblacional,  col = "Media Poblacional"), size = 1) + 
        scale_color_manual(name = "", values = c("Media Poblacional" = "black", Estimador = "red")) + 
        xlab("x") + ylab("f(x)") +
        theme(legend.position = "top",
              legend.text = element_text(size = 14),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              axis.title = element_text(size = 13, face = "bold"), 
              plot.title = element_text(size = 18, face = 'bold', hjust = 0.5)) + xlim(0.35, 0.65)


# 3. Gráficos media muestral: analizar y  calcular otras medidas (de centralización y dispersión), hacer boxplots, hacer gráficos de los residuos, etc
grafico_mediamuestral <- ggplot(data = datosgrafico, aes(x = mediamuestral)) + geom_density(fill = "lightblue") + 
        ggtitle("Distribución en el Muestreo de la Media Muestral") + 
        geom_vline(aes(xintercept = resultados$media_mediamuestral, col = "Media Muestral"), linetype = "dashed", size = 1) +
        geom_vline(aes(xintercept = resultados$media_poblacional,  col = "Media Poblacional"), size = 1) + 
        scale_color_manual(name = "", values = c("Media Poblacional" = "black", "Media Muestral" = "red")) + 
        xlab("x") + ylab("f(x)") +
        theme(legend.position = "top",
              legend.text = element_text(size = 14),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              axis.title = element_text(size = 13, face = "bold"), 
              plot.title = element_text(size = 18, face = 'bold', hjust = 0.5)) + xlim(0, 1)

grafico_mediamuestral_zoom <- ggplot(data = datosgrafico, aes(x = mediamuestral)) + geom_density(fill = "lightblue") + 
        ggtitle("Distribución en el Muestreo de la Media Muestral") + 
        geom_vline(aes(xintercept = resultados$media_mediamuestral, col = "Media Muestral"), linetype = "dashed", size = 1) +
        geom_vline(aes(xintercept = resultados$media_poblacional,  col = "Media Poblacional"), size = 1) + 
        scale_color_manual(name = "", values = c("Media Poblacional" = "black", "Media Muestral" = "red")) + 
        xlab("x") + ylab("f(x)") +
        theme(legend.position = "top",
              legend.text = element_text(size = 14),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              axis.title = element_text(size = 13, face = "bold"), 
              plot.title = element_text(size = 18, face = 'bold', hjust = 0.5)) + xlim(0.35, 0.65)



# 4. Comparación de la Media Muestral y nuestro Estimador (el que menor varianza o desviación tenga, será un mejor estimador)
  