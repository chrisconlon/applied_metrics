# Bias-variance tradeoff for a shrinkage estimator of a mean (slides: "Mean squared error").
# X_i ~ N(mu, sigma^2), n draws; estimator theta_hat(lambda) = (1 - lambda) * mean(X).
# Bias = -lambda * mu, Var = (1 - lambda)^2 sigma^2 / n. Run:  Rscript bias_variance.R
set.seed(93)
mu <- 1; sigma <- 3; n <- 20; R <- 5000
lambdas <- seq(0, 0.8, by = 0.05)
xbar <- replicate(R, mean(rnorm(n, mu, sigma)))
sim <- t(sapply(lambdas, function(l) {
  est <- (1 - l) * xbar
  c(bias2 = (mean(est) - mu)^2, var = var(est), mse = mean((est - mu)^2))
}))
lam <- seq(0, 0.8, length.out = 200)
bias2_f <- (lam * mu)^2; var_f <- (1 - lam)^2 * sigma^2 / n; mse_f <- bias2_f + var_f
lstar <- (sigma^2 / n) / (mu^2 + sigma^2 / n)
cat(sprintf("lambda* = %.2f, MSE(unbiased) = %.3f, MSE(lambda*) = %.3f\n",
            lstar, sigma^2 / n, lstar^2 * mu^2 + (1 - lstar)^2 * sigma^2 / n))
pdf("bias_variance.pdf", width = 6.5, height = 3.6, pointsize = 11)
par(mar = c(4, 4, 1, 1))
plot(NA, xlim = range(lambdas), ylim = c(0, max(mse_f) * 1.05), xlab = expression(paste("shrinkage ", lambda)),
     ylab = "", las = 1)
lines(lam, bias2_f, lwd = 2, col = "#bb0000"); lines(lam, var_f, lwd = 2, col = "#1f4e79"); lines(lam, mse_f, lwd = 3)
points(lambdas, sim[, "bias2"], pch = 16, col = "#bb0000"); points(lambdas, sim[, "var"], pch = 16, col = "#1f4e79")
points(lambdas, sim[, "mse"], pch = 16)
abline(v = lstar, lty = 2); abline(v = 0, lty = 3)
text(lstar, max(mse_f), bquote(lambda^"*" == .(round(lstar, 2))), pos = 4, cex = 0.9)
text(0.01, max(mse_f) * 0.97, "unbiased", pos = 4, cex = 0.85)
legend("right", bty = "n", legend = c(expression(Bias^2), "Variance", expression(MSE == Bias^2 + Variance), "simulated (5000 samples)"),
       col = c("#bb0000", "#1f4e79", "black", "black"), lwd = c(2, 2, 3, NA), pch = c(NA, NA, NA, 16))
dev.off()
