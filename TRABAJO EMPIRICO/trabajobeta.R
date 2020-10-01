
library(tidyverse)


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
        lista[[7]] <- mean(lista[[5]][, 2])
        lista[[8]] <- sd(lista[[5]][, 2])
        names(lista) <- c("tabla_muestras", "media_poblacional", "varianza_poblacional", "media_muestral", 
                          "estadístico", "media_mediamuestral", "media_estadístico", "desviación_estadístico")
        lista
}


resultados <- muestreo(alpha = 2, beta = 2)

datosgráfico <- inner_join(resultados$media_muestral , resultados$estadístico)

gráfico_estadistico <- ggplot(data = datosgráfico, aes(x = Estadístico)) + geom_density(fill = "lightblue") + 
        ggtitle("Distribución en el Muestreo de nuestro Estadístico") + 
        geom_vline(aes(xintercept = resultados$media_estadístico, col = "Estimador"), linetype = "dashed", size = 1) +
        geom_vline(aes(xintercept = resultados$media_poblacional,  col = "Media Poblacional"), size = 1) + 
        scale_color_manual(name = "", values = c("Media Poblacional" = "black", Estimador = "red")) + 
        xlab("x") + ylab("f(x)") +
        theme(legend.position = "top",
              legend.text = element_text(size = 14),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              axis.title = element_text(size = 13, face = "bold"), 
              plot.title = element_text(size = 18, face = 'bold', hjust = 0.5)) + xlim(0, 1)

gráfico_estadistico_zoom <- ggplot(data = datosgráfico, aes(x = Estadístico)) + geom_density(fill = "lightblue") + 
        ggtitle("Distribución en el Muestreo de nuestro Estadístico") + 
        geom_vline(aes(xintercept = resultados$media_estadístico, col = "Estimador"), linetype = "dashed", size = 1) +
        geom_vline(aes(xintercept = resultados$media_poblacional,  col = "Media Poblacional"), size = 1) + 
        scale_color_manual(name = "", values = c("Media Poblacional" = "black", Estimador = "red")) + 
        xlab("x") + ylab("f(x)") +
        theme(legend.position = "top",
              legend.text = element_text(size = 14),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              axis.title = element_text(size = 13, face = "bold"), 
              plot.title = element_text(size = 18, face = 'bold', hjust = 0.5)) + xlim(0.35, 0.65)

gráfico_mediamuestral <- ggplot(data = datosgráfico, aes(x = mediamuestral)) + geom_density(fill = "lightblue") + 
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

gráfico_mediamuestral_zoom2 <- ggplot(data = datosgráfico, aes(x = mediamuestral)) + geom_density(fill = "lightblue") + 
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




