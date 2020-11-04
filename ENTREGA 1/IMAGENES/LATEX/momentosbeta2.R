# Momentos de la distribución Beta
esperanzabeta <- expression(paste("E[X]" == frac(alpha, (alpha + beta))))
varianzabeta <- expression(paste("Var[X]" == frac(alpha ~ beta, (alpha + beta)^{2} ~ (alpha + beta + 1))))
funciongeneradora <- expression(paste(M[x](t) == sum(frac(Beta(alpha + k, beta), Beta(alpha, beta)) ~ frac(t^{k}, "k!"), k==0, infinity)))
par(mfrow = c(3,1), mar=c(0, 0, 0, 0), oma = c(13, 13, 13, 13))
plot(0, 0, type = "n", xlim = c(-0.25, 0.25), ylim = c(-0.25, 0.25), xaxt = "n", yaxt = "n", ann = FALSE, frame.plot = FALSE)
text(0, 0, labels = esperanzabeta, cex = 3)
plot(0, 0, type = "n", xlim = c(-0.25, 0.25), ylim = c(-0.25, 0.25), xaxt = "n", yaxt = "n", ann = FALSE, frame.plot = FALSE)
text(0, 0, labels = varianzabeta, cex = 3)
plot(0, 0, type = "n", xlim = c(-0.25, 0.25), ylim = c(-0.25, 0.25), xaxt = "n", yaxt = "n", ann = FALSE, frame.plot = FALSE)
text(0, 0, labels = funciongeneradora, cex = 3)

# Momentos de la distribución Beta (2, 2)
esperanzabeta22 <- expression(paste(paste("E[X]" == mu) == 0.5))
varianzabeta22 <- expression(paste(paste("Var[X]" == sigma^{2})) == 0.05)
par(mfrow = c(2,1), mar=c(0, 0, 0, 0), oma = c(15, 15, 15, 15))
plot(0, 0, type = "n", xlim = c(-0.25, 0.25), ylim = c(-0.25, 0.25), xaxt = "n", yaxt = "n", ann = FALSE, frame.plot = FALSE)
text(0, 0, labels = esperanzabeta22, cex = 3)
plot(0, 0, type = "n", xlim = c(-0.25, 0.25), ylim = c(-0.25, 0.25), xaxt = "n", yaxt = "n", ann = FALSE, frame.plot = FALSE)
text(0, 0, labels = varianzabeta22, cex = 3)



# 
