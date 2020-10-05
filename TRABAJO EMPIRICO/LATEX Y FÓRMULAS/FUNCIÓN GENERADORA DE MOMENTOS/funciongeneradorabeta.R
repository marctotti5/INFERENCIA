funciongeneradora <- expression(paste(M[x](t) == sum(frac(Beta(alpha + k, beta), Beta(alpha, beta)) ~ frac(t^{k}, "k!"), k==0, infinity)))
par(mfrow = c(3,1), mar=c(0, 0, 0, 0), oma = c(13, 13, 13, 13))
plot(0, 0, type = "n", xlim = c(-0.25, 0.25), ylim = c(-0.25, 0.25), xaxt = "n", yaxt = "n", ann = FALSE, frame.plot = FALSE)
text(0, 0, labels = funciongeneradora, cex = 3)
