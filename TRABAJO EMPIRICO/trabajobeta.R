
library(tidyverse)
library(e1071)
library(moments)
library(gt)
library(ggpubr)
library(ggplotify)
library(grid)
# 1. Creamos las muestras, evaluamos el estadístico en ellas, así como la media muestral

muestreo <- function(alpha, beta){
        lista <- list()
        tabla_muestras <- matrix(ncol = 40, nrow = 10) %>% as.data.frame()
        lista[[6]] <- matrix(ncol = 40, nrow = 1) %>% as.data.frame()
        media_poblacional <- alpha / (alpha + beta)
        varianza_poblacional <- alpha * beta / (((alpha + beta) ^ 2 ) * (alpha + beta + 1))
        for(j in 1:40){
                set.seed(j)
                tabla_muestras[, j] <- rbeta(10, shape1 = alpha, shape2 = beta)
                colnames(tabla_muestras)[j] <- as.numeric(gsub("V", "", colnames(tabla_muestras)[j]))
                colnames(lista[[6]])[j] <-  as.numeric(gsub("V", "", colnames(tabla_muestras)[j]))
                lista[[6]][, j] <- 0.5*(quantile(tabla_muestras[, j], probs = c(0.6)) + quantile(tabla_muestras[, j], probs = c(0.4)))
        }
        lista[[1]] <- tabla_muestras
        lista[[2]] <- media_poblacional
        lista[[3]] <- varianza_poblacional
        lista[[4]] <- data.frame(Muestra = names(colMeans(tabla_muestras)), mediamuestral = unname(colMeans(tabla_muestras))) 
        lista[[5]] <- data.frame(Estimador = "Media Muestral", 
                                 Media = mean(lista[[4]][, 2]),
                                 Mediana = median(lista[[4]][, 2]),
                                 SD = sd(lista[[4]][, 2]), 
                                 IQR = IQR(lista[[4]][, 2]), 
                                 MAD = mad(lista[[4]][, 2]), 
                                 Curtosis = moments::kurtosis(lista[[4]][, 2]), 
                                 Asimetría = e1071::skewness(lista[[4]][, 2]))
        lista[[6]] <- gather(as.data.frame(lista[[6]])) 
        colnames(lista[[6]]) <- c("Muestra", "Estadístico")
        lista[[7]] <- data.frame(Estimador = "Estadístico",
                                 Media = mean(lista[[6]][, 2]),
                                 Mediana = median(lista[[6]][, 2]),
                                 SD = sd(lista[[6]][, 2]), 
                                 IQR = IQR(lista[[6]][, 2]), 
                                 MAD = mad(lista[[6]][, 2]), 
                                 Curtosis = moments::kurtosis(lista[[6]][, 2]), 
                                 Asimetría = e1071::skewness(lista[[6]][, 2]))
        lista[[8]] <- rbind(lista[[5]], lista[[7]])
        names(lista) <- c("tabla_muestras", "media_poblacional", "varianza_poblacional", "media_muestral", 
                          "medidas_media_muestral", "estadistico", "medidas_estadistico", "tabla_comparacion")
        lista
}



resultados <- muestreo(alpha = 2, beta = 2)

datosgrafico <- inner_join(resultados$media_muestral , resultados$estadistico)


# 2. Gráficos sobre nuestro estimador: analizar y calcular otras medidas (de centralización y dispersión), hacer boxplots, hacer gráficos de los residuos, etc
## 2.0 Medidas de centralización, dispersión y forma de nuestro estimador
tabla_medidas_estadistico <- gt(resultados$medidas_estadistico[-1]) %>% 
        tab_header(title = md("**Estadístico**")) %>% 
        tab_spanner(label = md("***Medidas de Centralización***"), columns = c(1, 2)) %>% 
        tab_spanner(label = md("***Medidas de Dispersión***"), columns = c(3:5)) %>% 
        tab_spanner(label = md("***Medidas de Forma***"), columns = c(6, 7))

## 2.1 Histogramas

grafico_estadistico_histogram <- ggplot(data = datosgrafico, aes(x = Estadístico)) + 
        geom_histogram(fill = "lightblue", binwidth = 0.05, color = "black") + 
        ggtitle("Histograma de nuestro Estadístico") + 
        geom_vline(aes(xintercept = resultados[[7]]$Media, col = "Estimador"), linetype = "dashed", size = 1) +
        geom_vline(aes(xintercept = resultados$media_poblacional,  col = "Media Poblacional"), size = 1) + 
        scale_color_manual(name = "", values = c("Media Poblacional" = "black", Estimador = "red")) + 
        xlab("x") + ylab("f(x)") +
        theme(legend.position = "top",
              legend.text = element_text(size = 14),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              axis.title = element_text(size = 13, face = "bold"), 
              plot.title = element_text(size = 18, face = 'bold', hjust = 0.5)) + xlim(0, 1)

grafico_estadistico_histogram_zoom <- ggplot(data = datosgrafico, aes(x = Estadístico)) + 
        geom_histogram(fill = "lightblue", binwidth = 0.05, color = "black") + 
        ggtitle("Histograma de nuestro Estadístico") + 
        geom_vline(aes(xintercept = resultados[[7]]$Media, col = "Estimador"), linetype = "dashed", size = 1) +
        geom_vline(aes(xintercept = resultados$media_poblacional,  col = "Media Poblacional"), size = 1) + 
        scale_color_manual(name = "", values = c("Media Poblacional" = "black", Estimador = "red")) + 
        xlab("x") + ylab("f(x)") +
        theme(legend.position = "top",
              legend.text = element_text(size = 14),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              axis.title = element_text(size = 13, face = "bold"), 
              plot.title = element_text(size = 18, face = 'bold', hjust = 0.5)) + xlim(0.35, 0.65)


## 2.2 Gráficos de densidad

grafico_estadistico_density <- ggplot(data = datosgrafico, aes(x = Estadístico)) + geom_density(fill = "lightblue") + 
        ggtitle("Distribución en el Muestreo de nuestro Estadístico") + 
        geom_vline(aes(xintercept = resultados[[7]]$Media, col = "Estimador"), linetype = "dashed", size = 1) +
        geom_vline(aes(xintercept = resultados$media_poblacional,  col = "Media Poblacional"), size = 1) + 
        scale_color_manual(name = "", values = c("Media Poblacional" = "black", Estimador = "red")) + 
        xlab("x") + ylab("f(x)") +
        theme(legend.position = "top",
              legend.text = element_text(size = 14),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              axis.title = element_text(size = 13, face = "bold"), 
              plot.title = element_text(size = 18, face = 'bold', hjust = 0.5)) + xlim(0, 1)

grafico_estadistico_density_zoom <- ggplot(data = datosgrafico, aes(x = Estadístico)) + geom_density(fill = "lightblue") + 
        ggtitle("Distribución en el Muestreo de nuestro Estadístico") + 
        geom_vline(aes(xintercept = resultados[[7]]$Media, col = "Estimador"), linetype = "dashed", size = 1) +
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
## 3.0 Medidas de centralización, dispersión y forma de nuestro estimador
tabla_medidas_media_muestral <- gt(resultados$medidas_media_muestral[-1]) %>% 
        tab_header(title = md("**Estadístico**")) %>% 
        tab_spanner(label = md("***Medidas de Centralización***"), columns = c(1, 2)) %>% 
        tab_spanner(label = md("***Medidas de Dispersión***"), columns = c(3:5)) %>% 
        tab_spanner(label = md("***Medidas de Forma***"), columns = c(6, 7)) %>% 
        tab_options(table.width = 5) 

# Cuando n tiende a infinito, se supone que la varianza de la media muestral será sigma^2/n
resultados$varianza_poblacional / 40
resultados$medidas_media_muestral$SD^2
resultados[[4]]

## 3.1 Histogramas
grafico_mediamuestral_histogram <- ggplot(data = datosgrafico, aes(x = mediamuestral)) + 
        geom_histogram(fill = "lightblue", color = "black", binwidth = 0.05) + 
        ggtitle("Histograma de la Media Muestral") + 
        geom_vline(aes(xintercept = resultados[[5]]$Media, col = "Media Muestral"), linetype = "dashed", size = 1) +
        geom_vline(aes(xintercept = resultados$media_poblacional,  col = "Media Poblacional"), size = 1) + 
        scale_color_manual(name = "", values = c("Media Poblacional" = "black", "Media Muestral" = "red")) + 
        xlab("x") + ylab("f(x)") +
        theme(legend.position = "top",
              legend.text = element_text(size = 14),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              axis.title = element_text(size = 13, face = "bold"), 
              plot.title = element_text(size = 18, face = 'bold', hjust = 0.5)) + xlim(0, 1)

grafico_mediamuestral_histogram_zoom <- ggplot(data = datosgrafico, aes(x = mediamuestral)) + 
        geom_histogram(fill = "lightblue", color = "black", binwidth = 0.05) + 
        ggtitle("Histograma de la Media Muestral") + 
        geom_vline(aes(xintercept = resultados[[5]]$Media, col = "Media Muestral"), linetype = "dashed", size = 1) +
        geom_vline(aes(xintercept = resultados$media_poblacional,  col = "Media Poblacional"), size = 1) + 
        scale_color_manual(name = "", values = c("Media Poblacional" = "black", "Media Muestral" = "red")) + 
        xlab("x") + ylab("f(x)") +
        theme(legend.position = "top",
              legend.text = element_text(size = 14),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              axis.title = element_text(size = 13, face = "bold"), 
              plot.title = element_text(size = 18, face = 'bold', hjust = 0.5)) + xlim(0.35, 0.65)


## 3.2 Gráficos de Densidad

grafico_mediamuestral_density <- ggplot(data = datosgrafico, aes(x = mediamuestral)) + geom_density(fill = "lightblue") + 
        ggtitle("Distribución en el Muestreo de la Media Muestral") + 
        geom_vline(aes(xintercept = resultados[[5]]$Media, col = "Media Muestral"), linetype = "dashed", size = 1) +
        geom_vline(aes(xintercept = resultados$media_poblacional,  col = "Media Poblacional"), size = 1) + 
        scale_color_manual(name = "", values = c("Media Poblacional" = "black", "Media Muestral" = "red")) + 
        xlab("x") + ylab("f(x)") +
        theme(legend.position = "top",
              legend.text = element_text(size = 14),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              axis.title = element_text(size = 13, face = "bold"), 
              plot.title = element_text(size = 18, face = 'bold', hjust = 0.5)) + xlim(0, 1)

grafico_mediamuestral_density_zoom <- ggplot(data = datosgrafico, aes(x = mediamuestral)) + geom_density(fill = "lightblue") + 
        ggtitle("Distribución en el Muestreo de la Media Muestral") + 
        geom_vline(aes(xintercept = resultados[[5]]$Media, col = "Media Muestral"), linetype = "dashed", size = 1) +
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
tabla_medidas_media_muestral <- gt(resultados$tabla_comparacion) %>% 
        tab_header(title = md("**Estadístico**")) %>% 
        tab_spanner(label = md("***Medidas de Centralización***"), columns = c(2, 3)) %>% 
        tab_spanner(label = md("***Medidas de Dispersión***"), columns = c(4:6)) %>% 
        tab_spanner(label = md("***Medidas de Forma***"), columns = c(7, 8))

datos_grafico_comparativo <- gather(datosgrafico[, -1])
colnames(datos_grafico_comparativo) <- c("Estimador", "Valor")
datos_grafico_comparativo[, 1] <- gsub("mediamuestral", "Media Muestral", datos_grafico_comparativo[, 1])
cutoff <- data.frame( yintercept = resultados$media_poblacional, cutoff = factor(50) )

grafico_comparativo_boxplot <- ggplot(data = datos_grafico_comparativo, aes(x = Estimador, y = Valor, fill = Estimador, color = Estimador)) + 
        geom_boxplot(alpha = 0.5) + geom_jitter() + ggtitle("Diagrama de Caja: Estadístico vs Media Muestral") + 
        theme(legend.position = "none",
              legend.text = element_text(size = 14),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              axis.title = element_text(size = 13, face = "bold"), 
              plot.title = element_text(size = 18, face = 'bold', hjust = 0.5)) 

grafico_comparativo_density <- ggarrange(grafico_estadistico_density,  
                                         grafico_mediamuestral_density, 
                                         nrow = 2)
grafico_comparativo_histogram <- ggarrange(grafico_estadistico_histogram,  
                                           grafico_mediamuestral_histogram, 
                                           nrow = 2)
