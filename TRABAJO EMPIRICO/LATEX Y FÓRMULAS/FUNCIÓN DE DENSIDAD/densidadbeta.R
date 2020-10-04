# 1. Función de Densidad de una Beta: lo exportamos y guardamos en la carpeta de LATEX
densidadbeta <- expression(paste(f(x) == frac(Gamma(alpha + beta),  Gamma(alpha) ~ Gamma(beta)) ~ x^{alpha - 1} ~ (1-x)^{beta - 1}))
plot(0, 0, type = "n", xlim = c(-0.25, 0.25), ylim = c(-0.25, 0.25), xaxt = "n", yaxt = "n", ann = FALSE, frame.plot = FALSE)
text(0, 0, labels = densidadbeta, cex = 2)

densidadbeta2 <- expression(paste(f(x) == frac(1, Beta(alpha, beta)) ~ x^{alpha - 1} ~ (1-x)^{beta - 1}))