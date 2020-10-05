# EJEMPLOS LATEX
par(mfrow = c(1,1))
latex2exp::latex2exp_examples()
ejemplo <- latex2exp::TeX("$\\left(\\int_{0}^{1} \\sin(x) dx \\right)$")
# 1. Función de Densidad de una Beta: lo exportamos y guardamos en la carpeta de LATEX
densidadbeta <- expression(paste(f(x) == frac(Gamma(alpha + beta),  Gamma(alpha) ~ Gamma(beta)) ~ x^{alpha - 1} ~ (1-x)^{beta - 1}))

## Gráfico Función de densidad 1 (sin función beta)
plot(0, 0, type = "n", xlim = c(-0.25, 0.25), ylim = c(-0.25, 0.25), xaxt = "n", yaxt = "n", ann = FALSE, frame.plot = FALSE)
text(0, 0, labels = densidadbeta, cex = 2)

## 1.2 Segunda manera de escribir la función de densidad (utilizando la función beta)
densidadbeta2 <- expression(paste(f(x) == frac(1, Beta(alpha, beta)) ~ x^{alpha - 1} ~ (1-x)^{beta - 1}))
integralbeta <- latex2exp::TeX("$\\left(\\int_{0}^{1} \\x^{\\alpha - 1} (1-x)^{\\beta - 1} dx \\right)$")
funcionbeta <- expression(paste(paste(Beta(alpha, beta) == frac(Gamma(alpha) ~ Gamma(beta), Gamma(alpha + beta))) == integral(x^{alpha-1} ~ (1-x)^{beta-1}*dx, 0, 1)))
propiedadfuncionbeta1 <- expression(paste(Beta(alpha, beta) == Beta(beta, alpha)))
propiedadfuncionbeta2 <- expression(paste(Beta(alpha, 1) == frac(1, alpha)))
propiedadfuncionbeta3 <- expression(paste(Beta(alpha + 1, beta) == frac(alpha, (alpha + beta)) ~ Beta(alpha, beta)))

### Gráfico Función de densidad 2 (con función beta)
par(mfrow = c(1,1))
plot(0, 0, type = "n", xlim = c(-0.25, 0.25), ylim = c(-0.25, 0.25), xaxt = "n", yaxt = "n", ann = FALSE, frame.plot = FALSE)
text(0, 0, labels = densidadbeta2, cex = 2)

### Gráfico Función Beta
plot(0, 0, type = "n", xlim = c(-0.25, 0.25), ylim = c(-0.25, 0.25), xaxt = "n", yaxt = "n", ann = FALSE, frame.plot = FALSE)
text(0, 0, labels = funcionbeta, cex = 2)

### Gráfico Propiedades función Beta
par(mfrow = c(3,1), mar=c(0, 0, 0, 0), oma = c(13, 13, 13, 13))
plot(0, 0, type = "n", xlim = c(-0.25, 0.25), ylim = c(-0.25, 0.25), xaxt = "n", yaxt = "n", ann = FALSE, frame.plot = FALSE)
text(0, 0, labels = propiedadfuncionbeta1, cex = 2)

plot(0, 0, type = "n", xlim = c(-0.25, 0.25), ylim = c(-0.25, 0.25), xaxt = "n", yaxt = "n", ann = FALSE, frame.plot = FALSE)
text(0, 0, labels = propiedadfuncionbeta2, cex = 2)

plot(0, 0, type = "n", xlim = c(-0.25, 0.25), ylim = c(-0.25, 0.25), xaxt = "n", yaxt = "n", ann = FALSE, frame.plot = FALSE)
text(0, 0, labels = propiedadfuncionbeta3, cex = 2)

