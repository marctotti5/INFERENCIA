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
        datos_mediamuestral <- matrix(ncol = 40, nrow = length(tamaño_muestral))
        
        for(i in seq_along(tamaño_muestral)){
                lista[[i]] <- matrix(ncol = 40, nrow = tamaño_muestral[i]) 
                for(j in 1:40){
                        set.seed(j)
                        lista[[i]][, j] <- rbeta(tamaño_muestral[i], shape1 = alpha, shape2 = beta)
                        colnames(lista[[i]])[j] <- as.numeric(gsub("V", "", colnames(lista[[i]])[j]))
                        colnames(lista[[i]])[j] <-  as.numeric(gsub("V", "", colnames(lista[[i]])[j]))
                        lista[[6]][i, j] <- 0.5*(quantile(lista[[i]][, j], probs = c(0.6)) + 
                                                        quantile(lista[[i]][, j], probs = c(0.4))) 
                        datos_mediamuestral[i, j] <- mean(lista[[i]][, j])
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
                                  "varianza_estadistico", "sesgo_estadistico", 
                                  "precisión_estadistico", "ECM_estadistico")
        
        rownames(datos_mediamuestral) <- rownames(lista[[6]])
        datos_mediamuestral <- t(datos_mediamuestral)
        datos_mediamuestral <- data.frame(tamaño_muestral = as.character(tamaño_muestral),
                                              esperanza_mediamuestral = as.numeric(unname(apply(datos_mediamuestral, 2, mean))),
                                              varianza_mediamuestral = as.numeric(unname(apply(datos_mediamuestral, 2, var))),
                                              sesgo_mediamuestral = as.numeric(unname(apply(datos_mediamuestral, 2, mean))) - media_poblacional, 
                                              precision_mediamuestral = 1/as.numeric(unname(apply(datos_mediamuestral, 2, var))))
        datos_mediamuestral$ECM_mediamuestral <- datos_mediamuestral$varianza_mediamuestral + (datos_mediamuestral$sesgo_mediamuestral)^2
        colnames(datos_mediamuestral) <- c("tamaño_muestral", "esperanza_mediamuestral", 
                                               "varianza_mediamuestral", "sesgo_mediamuestral", 
                                               "precisión_mediamuestral", "ECM_mediamuestral")
        lista[[25]] <- datos_mediamuestral
        
        # Falta invarianza y suficiencia
        
        # Robustez
        distribucion_muestral_beta <- as.data.frame(matrix(nrow = 10, ncol = 40))
        distribucion_muestral_uniforme <- as.data.frame(matrix(nrow = 10, ncol = 40))
        distribucion_muestral_contaminada <- as.data.frame(matrix(nrow = 10, ncol = 40))
        estadistico <- as.data.frame(matrix(nrow = 1, ncol = 40))
        media_muestral <- as.data.frame(matrix(nrow = 1, ncol = 40))
        porcentaje_contaminacion <- 0.9
        unif_prob_contaminar <- matrix(runif(400, min = 0, max = 1), byrow = TRUE, ncol = 40, nrow = 10) %>% as.data.frame()
        
        for(j in 1:40){
                distribucion_muestral_beta[, j] <- rbeta(10, shape1 = alpha, shape2 = beta)
                distribucion_muestral_uniforme[, j] <- runif(10, min = 2, max = 3)
                for(i in 1:10){
                        if(unif_prob_contaminar[i, j] <= 0.9){
                                distribucion_muestral_contaminada[i, j] <- distribucion_muestral_beta[i, j]
                        } else if(unif_prob_contaminar[i, j] > 0.9) {
                                distribucion_muestral_contaminada[i, j] <- distribucion_muestral_uniforme[i, j]
                        }
                }
        }
        for(j in 1:40){
                estadistico[, j] <- 0.5*(quantile(distribucion_muestral_contaminada[, j], probs = c(0.6)) + 
                                                 quantile(distribucion_muestral_contaminada[, j], probs = c(0.4)))
        }
        colnames(distribucion_muestral_beta) <-  as.numeric(gsub("V", "", colnames(distribucion_muestral_beta)))
        colnames(distribucion_muestral_uniforme) <-  as.numeric(gsub("V", "", colnames(distribucion_muestral_uniforme)))
        colnames(distribucion_muestral_contaminada) <-  as.numeric(gsub("V", "", colnames(distribucion_muestral_contaminada)))
        media_muestral <- data.frame(Muestra = names(colMeans(distribucion_muestral_contaminada)), 
                                     mediamuestral = unname(colMeans(distribucion_muestral_contaminada))) 
        media_muestral$Muestra <- as.numeric(media_muestral$Muestra)
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
        lista[[14]] <- media_poblacional
        lista[[15]] <- varianza_poblacional
        lista[[16]] <- datos_mediamuestral
        
        # Ley de los grandes numeros
        num_experimentos <- c(rep(0, 16), 40, 400, 4000, 40000, 400000)
        lista[[22]] <- matrix(ncol = 10, nrow = length(num_experimentos)) 
        datos_mediamuestral_numexperimentos <- matrix(ncol = 10, nrow = length(num_experimentos))
        
        for(i in 17:21){
                lista[[i]] <- matrix(ncol = 10, nrow = num_experimentos[i]) 
                for(j in 1:10){
                        set.seed(j)
                        lista[[i]][, j] <- rbeta(num_experimentos[i], shape1 = alpha, shape2 = beta)
                        lista[[22]][i, j] <- 0.5*(quantile(lista[[i]][, j], probs = c(0.6)) + 
                                                         quantile(lista[[i]][, j], probs = c(0.4))) 
                        datos_mediamuestral_numexperimentos[i, j] <- mean(lista[[i]][, j])
                }
        }
        lista[[22]] <- lista[[22]][complete.cases(lista[[22]]), ]
        datos_mediamuestral_numexperimentos <- datos_mediamuestral_numexperimentos[complete.cases(datos_mediamuestral_numexperimentos), ]
        rownames(lista[[22]]) <- num_experimentos[17:length(num_experimentos)]
        num_experimentos <- c(40, 400, 4000, 40000, 400000)
        
        # 6 es 22
        lista[[22]] <- t(lista[[22]])
        lista[[23]] <- t(colMeans(lista[[22]])) %>% unname() %>% as.numeric()
        lista[[23]] <- data.frame(experimentos = as.character(num_experimentos), media_estadistico = lista[[23]])
        
        # Varianza estadístico
        lista[[24]] <- data.frame(experimentos = as.character(num_experimentos), varianza_estadistico = as.numeric(unname(apply(lista[[22]], 2, var))))
        lista[[23]] <- merge(lista[[23]], lista[[24]])
        lista[[24]] <- NULL
        
        # Sesgo, precisión, ECM, consistencia (implicita, ya que hemos calculado la varianza y el sesgo)
        for(i in 1:nrow(lista[[23]])){
                lista[[23]][i, 4] <- lista[[23]][i, 2] - media_poblacional
                lista[[23]][i, 5] <- 1/(lista[[23]][i, 3]) # precision = 1/var(t)
                lista[[23]][i, 6] <- lista[[23]][i, 3] + (lista[[23]][i, 4])^2 # ecm(t) = varianza(t) + (sesgo(t))^2
        }
        colnames(lista[[23]]) <- c("experimentos", "esperanza_estadistico", 
                                  "varianza_estadistico", "sesgo_estadistico", 
                                  "precisión_estadistico", "ECM_estadistico")
        rownames(datos_mediamuestral_numexperimentos) <- rownames(lista[[22]])
        datos_mediamuestral_numexperimentos <- t(datos_mediamuestral_numexperimentos)
        datos_mediamuestral_numexperimentos <- data.frame(experimentos = as.character(num_experimentos),
                                                          esperanza_mediamuestral = as.numeric(unname(apply(datos_mediamuestral_numexperimentos, 2, mean))),
                                                          varianza_mediamuestral = as.numeric(unname(apply(datos_mediamuestral_numexperimentos, 2, var))),
                                                          sesgo_mediamuestral = as.numeric(unname(apply(datos_mediamuestral_numexperimentos, 2, mean))) - media_poblacional, 
                                                          precision_mediamuestral = 1/as.numeric(unname(apply(datos_mediamuestral_numexperimentos, 2, var))))
        datos_mediamuestral_numexperimentos$ECM_mediamuestral <- datos_mediamuestral_numexperimentos$varianza_mediamuestral + (datos_mediamuestral_numexperimentos$sesgo_mediamuestral)^2
        colnames(datos_mediamuestral_numexperimentos) <- c("experimentos", "esperanza_mediamuestral", 
                                                           "varianza_mediamuestral", "sesgo_mediamuestral", 
                                                           "precisión_mediamuestral", "ECM_mediamuestral")
        lista[[24]] <- datos_mediamuestral_numexperimentos
        lista[[25]] <- datos_mediamuestral
        names(lista) <- c("muestras_10", "muestras_100", 
                          "muestras_1000", "muestras_10000", 
                          "muestras_100k", "estadístico_n", 
                          "propiedades_estimador", "distribucion_contaminada", 
                          "estadistico", "medidas_estadistico", 
                          "media_muestral", "medidas_media_muestral", 
                          "tabla_comparacion", "media_poblacional", "varianza_poblacional",
                          "datos_mediamuestral", "muestras_40exp", "muestras_400exp", "muestras_4000exp",
                          "muestras_40000exp", "muestras_400000exp", "estadistico_exp", "propiedades_estimador_exp", 
                          "propiedades_mediamuestral_exp", "propiedades_mediamuestral")
        
        
        lista
}

resultados <- entrega_dos(alpha = 2, beta = 2)

# Insesgadez 
datos_graficos <- merge(resultados$propiedades_estimador_exp, resultados$propiedades_mediamuestral_exp)
colores_esperanza <- c("E[T]" = "black", "E[Media Muestral]" = "blue",  "Media Poblacional" = "red")
# Hemos visto analíticamente que nuestro estimador es insesgado, es decir, que en promedio es la media poblacional
# Aquí podemos ver que a medida que aumenta el tamaño muestral (n), la esperanza de nuestro estadístico se va acercando cada vez más a la media poblacional
# De aquí podemos suponer que para un n suficientemente grande, la esperanza del estadístico convergerá a la media poblacional
grafico_esperanza <- ggplot(data = datos_graficos, aes(x = experimentos, y = esperanza_estadistico, group = 1)) + 
        geom_hline(aes(yintercept = resultados$media_poblacional, color = "Media Poblacional"), 
                   linetype = "dashed", size = 1) +
        geom_line(aes(color = "E[T]"), size = 0.75) + 
        geom_point(size = 3) + 
        geom_line(aes(x = experimentos, y = esperanza_mediamuestral , color = "E[Media Muestral]"), size = 0.75) +
        geom_point(aes(x = experimentos, y = esperanza_mediamuestral), size = 3, color = "blue") + 
        scale_color_manual(values = colores_esperanza) + 
        ggtitle(expression(bold(paste("Esperanza de los estimadores de ", mu)))) + 
        xlab("Número de muestras de tamaño 10") + ylab("Esperanza") +
        theme(legend.position = "top",
              legend.title = element_blank(),
              legend.text = element_text(size = 14),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              axis.title = element_text(size = 13, face = "bold"), 
              plot.title = element_text(size = 18, face = 'bold', hjust = 0.5)) 

# En otras palabras, el sesgo de nuestro estimador tiende a cero cuando n tiende a infinito.
colores_sesgo <- c("b[T]" = "black", "b[Media Muestral]" = "blue",  "0" = "red")
# Hemos visto analíticamente que nuestro estimador es insesgado, es decir, que en promedio es la media poblacional
# Aquí podemos ver que a medida que aumenta el tamaño muestral (n), la esperanza de nuestro estadístico se va acercando cada vez más a la media poblacional
# De aquí podemos suponer que para un n suficientemente grande, la esperanza del estadístico convergerá a la media poblacional
grafico_sesgo_exp <- ggplot(data = datos_graficos, aes(x = experimentos, y = sesgo_estadistico, group = 1)) + 
        geom_hline(aes(yintercept = 0, color = "0"), 
                   linetype = "dashed", size = 1) +
        geom_line(aes(color = "b[T]"), size = 0.75) + 
        geom_point(size = 3) + 
        geom_line(aes(x = experimentos, y = sesgo_mediamuestral , color = "b[Media Muestral]"), size = 0.75) +
        geom_point(aes(x = experimentos, y = sesgo_mediamuestral), size = 3, color = "blue") + 
        scale_color_manual(values = colores_sesgo) + 
        ggtitle(expression(bold(paste("Sesgo de los estimadores de ", mu)))) + 
        xlab("Número de muestras de tamaño 10") + ylab("Sesgo") +
        theme(legend.position = "top",
              legend.title = element_blank(),
              legend.text = element_text(size = 14),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              axis.title = element_text(size = 13, face = "bold"),
              axis.title.y = element_text(size = 13, face = "bold"),
              plot.title = element_text(size = 18, face = 'bold', hjust = 0.5)) 



# Precisión: tanto la media muestral como nuestro estimador son insesgados, pero la media tiene una mayor precisión que nuestro estimador, 
# esto se debe a que tiene menor varianza que T.
colores_precision <- c("Pr[T]" = "black", "Pr[Media Muestral]" = "red")
grafico_comparacion_precision <- ggplot(data = datos_graficos, aes(x = experimentos, group = 1)) + 
        geom_line(aes(y = precisión_estadistico, color = "Pr[T]"), size = 0.75) + 
        geom_point(aes(y = precisión_estadistico, color = "Pr[T]"), size = 3) + 
        geom_line(aes(y = precisión_mediamuestral, color = "Pr[Media Muestral]"), size = 0.75) +
        geom_point(aes(y = precisión_mediamuestral, color = "Pr[Media Muestral]"), size = 3) + 
        scale_color_manual(values = colores_precision) +
        ggtitle(expression(bold(paste("Precisión de los estimadores de ", mu)))) + 
        xlab("Número de muestras de tamaño 10") + ylab("Precisión") +
        theme(legend.position = "top",
              legend.title = element_blank(),
              legend.text = element_text(size = 14),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              axis.title = element_text(size = 13, face = "bold"), 
              plot.title = element_text(size = 18, face = 'bold', 
                                        hjust = 0.5)) 

# Varianza : La varianza tiende a cero cuando n tiende a infinito
colores_varianza <- c("Var[T]" = "black", "Var[Media Muestral]" = "red")
grafico_varianza <- ggplot(data = datos_graficos, aes(x = experimentos, group = 1)) + 
        geom_line(aes(y = varianza_estadistico, color = "Var[T]"), size = 0.75) + 
        geom_point(aes(y = varianza_estadistico, color = "Var[T]"), size = 3) + 
        geom_line(aes(y = varianza_mediamuestral, color = "Var[Media Muestral]"), size = 0.75) +
        geom_point(aes(y = varianza_mediamuestral, color = "Var[Media Muestral]"), size = 3) + 
        scale_color_manual(values = colores_varianza) +
        ggtitle(expression(bold(paste("Varianza de los estimadores de ", mu)))) + 
        xlab("Número de muestras de tamaño 10") + ylab("Varianza") +
        theme(legend.position = "top",
              legend.title = element_blank(),
              legend.text = element_text(size = 14),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              axis.title = element_text(size = 13, face = "bold"), 
              plot.title = element_text(size = 18, face = 'bold', 
                                        hjust = 0.5)) 


# ECM = VAR(T) + b(T)
# Al ser ambos la media muestral y nuestro estadístico insesgados, su ECM es su varianza, y como la media muestral tiene menor varianza, tiene menor ECM
# Es decir, la media muestral se aleja en promedio menos de la media poblacional que nuestro estimador T.
# Por lo tanto la media muestral sería mejor estimador de la media poblacional que nuestro estimador
colores_ecm <- c("ECM[T]" = "black", "ECM[Media Muestral]" = "red")
grafico_comparacion_ECM <- ggplot(data = datos_graficos, aes(x = experimentos, group = 1)) + 
        geom_line(aes(y = ECM_estadistico, color = "ECM[T]"), size = 0.75) + 
        geom_point(aes(y = ECM_estadistico, color = "ECM[T]"), size = 3) + 
        geom_line(aes(y = ECM_mediamuestral, color = "ECM[Media Muestral]"), size = 0.75) +
        geom_point(aes(y = ECM_mediamuestral, color = "ECM[Media Muestral]"), size = 3) + 
        scale_color_manual(values = colores_ecm) +
        ggtitle(expression(bold(paste("ECM de los estimadores de ", mu)))) + 
        xlab("Número de muestras de tamaño 10") + ylab("Error Cuadrático Medio (ECM)") +
        theme(legend.position = "top",
              legend.title = element_blank(),
              legend.text = element_text(size = 14),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              axis.title = element_text(size = 13, face = "bold"), 
              plot.title = element_text(size = 18, face = 'bold', 
                                        hjust = 0.5)) 


# Consistencia: la varianza y el sesgo tienden a cero a medida que n tiende a infinito. 
# Esto significa que nuestro estimador es consistente, es decir que cuando n tiende a infinito aproxima bien la media poblacional
datos_graficos_tamaño_muestral <- merge(resultados$propiedades_estimador, resultados$propiedades_mediamuestral)
colores_varianza <- c("Var[T]" = "black", "Var[Media Muestral]" = "blue")
grafico_sesgo_tamañomuestral <- ggplot(data = datos_graficos_tamaño_muestral, aes(x = tamaño_muestral, y = sesgo_estadistico, group = 1)) + 
        geom_hline(aes(yintercept = 0, color = "0"), 
                   linetype = "dashed", size = 1) +
        geom_line(aes(color = "b[T]"), size = 0.75) + 
        geom_point(size = 3) + 
        geom_line(aes(x = tamaño_muestral, y = sesgo_mediamuestral , color = "b[Media Muestral]"), size = 0.75) +
        geom_point(aes(x = tamaño_muestral, y = sesgo_mediamuestral), size = 3, color = "blue") + 
        scale_color_manual(values = colores_sesgo) + 
        ggtitle(expression(bold(paste("Sesgo de los estimadores de ", mu)))) + 
        xlab("Tamaño muestral (n)") + ylab("Sesgo") +
        theme(legend.position = "top",
              legend.title = element_blank(),
              legend.text = element_text(size = 14),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              axis.title = element_text(size = 13, face = "bold"),
              axis.title.y = element_text(size = 13, face = "bold"),
              plot.title = element_text(size = 18, face = 'bold', hjust = 0.5)) 
grafico_varianza_tamaño_muestral <- ggplot(data = datos_graficos_tamaño_muestral, aes(x = tamaño_muestral, group = 1)) + 
        geom_line(aes(y = varianza_estadistico, color = "Var[T]"), size = 0.75) + 
        geom_point(aes(y = varianza_estadistico, color = "Var[T]"), size = 3) + 
        geom_line(aes(y = varianza_mediamuestral, color = "Var[Media Muestral]"), size = 0.75) +
        geom_point(aes(y = varianza_mediamuestral, color = "Var[Media Muestral]"), size = 3) + 
        scale_color_manual(values = colores_varianza) +
        ggtitle(expression(bold(paste("Varianza de los estimadores de ", mu)))) + 
        xlab("Tamaño muestral (n)") + ylab("Varianza") +
        theme(legend.position = "top",
              legend.title = element_blank(),
              legend.text = element_text(size = 14),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              axis.title = element_text(size = 13, face = "bold"), 
              plot.title = element_text(size = 18, face = 'bold', 
                                        hjust = 0.5)) 


grafico_consistencia <- ggarrange(grafico_varianza_tamaño_muestral, grafico_sesgo_tamañomuestral) 

# Robustez
# Hemos contaminado la muestra con p = 0.1, de una
datosgrafico <- inner_join(resultados$media_muestral , resultados$estadistico)
# 2. 2. Gráficos sobre nuestro estimador: analizar y calcular otras medidas (de centralización y dispersión), hacer boxplots, hacer gráficos de los residuos, etc
tabla_medidas_estadistico <- gt(resultados$medidas_estadistico[-1]) %>% 
        tab_header(title = md("**Estadístico T**")) %>% 
        tab_spanner(label = md("***Medidas de Centralización***"), columns = c(1, 2)) %>% 
        tab_spanner(label = md("***Medidas de Dispersión***"), columns = c(3:5)) %>% 
        tab_spanner(label = md("***Medidas de Forma***"), columns = c(6, 7))

## 2.1 Histogramas

grafico_estadistico_histogram_density <- ggplot(data = datosgrafico, aes(x = Estadistico)) + 
        geom_histogram(aes(y = ..ndensity..), fill = "lightblue", binwidth = 0.05, color = "black", position = "identity") + 
        geom_density(aes(x = Estadistico, y = ..ndensity..), color = "blue", size = 0.75) +
        ggtitle("Histograma de nuestro Estadístico ") + 
        geom_vline(aes(xintercept = resultados$medidas_estadistico$Media, col = "Estimador"), linetype = "dashed", size = 1) +
        geom_vline(aes(xintercept = resultados$media_poblacional,  col = "Media Poblacional"), size = 1) + 
        scale_color_manual(name = "", values = c("Media Poblacional" = "black", Estimador = "red")) + 
        xlab("x") + ylab("f(x)") +
        theme(legend.position = "top",
              legend.text = element_text(size = 14),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              axis.title = element_text(size = 13, face = "bold"), 
              plot.title = element_text(size = 18, face = 'bold', hjust = 0.5)) + xlim(0, 1)





# 3. Gráficos media muestral: analizar y  calcular otras medidas (de centralización y dispersión), hacer boxplots, hacer gráficos de los residuos, etc
## 3.0 Medidas de centralización, dispersión y forma de nuestro estimador
tabla_medidas_media_muestral <- gt(resultados$medidas_media_muestral[-1]) %>% 
        tab_header(title = md("**Media Muestral**")) %>% 
        tab_spanner(label = md("***Medidas de Centralización***"), columns = c(1, 2)) %>% 
        tab_spanner(label = md("***Medidas de Dispersión***"), columns = c(3:5)) %>% 
        tab_spanner(label = md("***Medidas de Forma***"), columns = c(6, 7)) %>% 
        tab_options(table.width = 5) 

# Cuando n tiende a infinito, se supone que la varianza de la media muestral será sigma^2/n
varianzamediamuestral_data <- data.frame(varianzamediamuestral_real = resultados$medidas_media_muestral$SD^2, 
                                         varianzamediamuestral_teorica = resultados$varianza_poblacional / 10)
colnames(varianzamediamuestral_data) <- c("Valor Obtenido", "Valor Teórico")
tabla_varianza_muestral_poblacional <- gt(varianzamediamuestral_data) %>% 
        tab_header(title = md("**Varianza de la Media Muestral**")) %>% 
        cols_align(align = "center")



## 3.1 Histogramas
grafico_mediamuestral_histogram_density <- ggplot(data = datosgrafico, aes(x = mediamuestral)) + 
        geom_histogram(aes(y = ..ndensity..), fill = "lightblue", color = "black", binwidth = 0.05) + 
        geom_density(aes(y = ..ndensity..), size = 0.75, col = "blue") +
        ggtitle("Histograma de la Media Muestral") + 
        geom_vline(aes(xintercept = resultados$medidas_media_muestral$Media, col = "Media Muestral"), linetype = "dashed", size = 1) +
        geom_vline(aes(xintercept = resultados$media_poblacional,  col = "Media Poblacional"), size = 1) + 
        scale_color_manual(name = "", values = c("Media Poblacional" = "black", "Media Muestral" = "red")) + 
        xlab("x") + ylab("f(x)") +
        theme(legend.position = "top",
              legend.text = element_text(size = 14),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              axis.title = element_text(size = 13, face = "bold"), 
              plot.title = element_text(size = 18, face = 'bold', hjust = 0.5)) + xlim(0, 1)

# 4. Comparación de la Media Muestral y nuestro Estimador (el que menor varianza o desviación tenga, será un mejor estimador)
tabla_medidas_comparacion <- gt(resultados$tabla_comparacion) %>% 
        tab_header(title = md("**Comparativa de Estimadores**")) %>% 
        tab_spanner(label = md("***Medidas de Centralización***"), columns = c(2, 3)) %>% 
        tab_spanner(label = md("***Medidas de Dispersión***"), columns = c(4:6)) %>% 
        tab_spanner(label = md("***Medidas de Forma***"), columns = c(7, 8))

datos_grafico_comparativo <- gather(datosgrafico[, -1])
colnames(datos_grafico_comparativo) <- c("Estimador", "Valor")
datos_grafico_comparativo[, 1] <- gsub("mediamuestral", "Media Muestral", datos_grafico_comparativo[, 1])
cutoff <- data.frame( yintercept = resultados$media_poblacional, cutoff = factor(50) )

grafico_comparativo_boxplot <- ggplot(data = datos_grafico_comparativo, aes(x = Estimador, y = Valor, fill = Estimador, color = Estimador)) + 
        geom_boxplot(alpha = 0.5) + ggtitle("Diagrama de Caja: Estadístico T vs Media Muestral") + 
        theme(legend.position = "none",
              legend.text = element_text(size = 14),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              axis.title = element_text(size = 13, face = "bold"), 
              plot.title = element_text(size = 18, face = 'bold', hjust = 0.5)) 

grafico_comparativo_histogram_density <- ggarrange(grafico_estadistico_histogram_density,  
                                                   grafico_mediamuestral_histogram_density, 
                                                   nrow = 2)