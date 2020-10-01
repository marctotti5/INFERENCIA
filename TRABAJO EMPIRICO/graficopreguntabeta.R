set.seed(1)
data <- rbeta(1000000, shape1 = 2, shape2 = 2)
data <- data.frame(datos = data)

ggplot(data = data, aes(x = datos)) + geom_density(fill = "lightblue")  + 
        xlab("x") + ylab("f(x)") + 
        xlim(0, 1) + ggtitle(expression("Distribución Beta" ~ alpha == "2," ~ beta == 2)) + 
        theme(legend.text = element_text(size = 14),
              axis.text.x = element_text(size = 15),
              axis.text.y = element_text(size = 15),
              axis.title = element_text(size = 15, face = "bold"), 
              plot.title = element_text(size = 25, face = 'bold', hjust = 0.5))
              