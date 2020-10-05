# Momentos de la distribución Beta
esperanzabeta <- expression(paste("E[X]" == frac(alpha, (alpha + beta))))
varianzabeta <- expression(paste("Var[X]" == frac(alpha ~ beta, (alpha + beta)^{2} ~ (alpha + beta + 1))))
par(mfrow = c(2,1), mar=c(0, 0, 0, 0), oma = c(8, 8, 8, 8))
plot(0, 0, type = "n", xlim = c(-0.25, 0.25), ylim = c(-0.25, 0.25), xaxt = "n", yaxt = "n", ann = FALSE, frame.plot = FALSE)
text(0, 0, labels = esperanzabeta, cex = 3)
plot(0, 0, type = "n", xlim = c(-0.25, 0.25), ylim = c(-0.25, 0.25), xaxt = "n", yaxt = "n", ann = FALSE, frame.plot = FALSE)
text(0, 0, labels = varianzabeta, cex = 3)