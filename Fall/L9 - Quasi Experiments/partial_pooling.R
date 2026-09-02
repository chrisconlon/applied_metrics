# Partial pooling figure for L9a (Panel Data and Fixed Effects): store effects estimated three ways.
# No pooling (store means), complete pooling (grand mean), partial pooling (empirical Bayes / random effects).
# Dependencies: none beyond base R.  Run:  Rscript partial_pooling.R
set.seed(20261022)
S <- 60; sd_alpha <- 0.4; sd_eps <- 1.0
alpha <- rnorm(S, 0, sd_alpha)                              # true store effects (log sales)
n_s <- sample(c(4, 8, 16, 32, 64), S, replace = TRUE)      # weeks observed per store: very uneven
ybar <- alpha + rnorm(S, 0, sd_eps / sqrt(n_s))             # store sample means
grand <- mean(ybar)
# variance components from the data (method of moments): sigma_eps^2 known here for clarity, sigma_alpha^2 estimated
s2_eps <- sd_eps^2; s2_alpha <- max(0, var(ybar) - mean(s2_eps / n_s))
shrink <- s2_alpha / (s2_alpha + s2_eps / n_s)               # weight on the store's own mean
eb <- grand + shrink * (ybar - grand)
mse <- c(no_pooling = mean((ybar - alpha)^2), complete = mean((grand - alpha)^2), partial = mean((eb - alpha)^2))
cat(sprintf("sigma_alpha^2 hat %.3f (truth %.3f) | MSE: no pooling %.4f, complete pooling %.4f, partial pooling %.4f\n", s2_alpha, sd_alpha^2, mse[1], mse[2], mse[3]))
cat(sprintf("ranking: top-5 by store mean have n = %s; top-5 by partial pooling have n = %s\n",
            paste(n_s[order(-ybar)][1:5], collapse = ","), paste(n_s[order(-eb)][1:5], collapse = ",")))
pdf("fig_partial_pooling.pdf", width = 8, height = 4, pointsize = 11)
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
cols <- c("#bb0000", "#e69f00", "#1f4e79", "#1f4e79", "#1f4e79")[match(n_s, c(4, 8, 16, 32, 64))]
plot(ybar, eb, pch = 16, col = cols, xlab = "no pooling: store sample mean", ylab = "partial pooling: shrunken estimate",
     main = "Shrinkage toward the grand mean", xlim = range(ybar), ylim = range(ybar))
abline(0, 1, lty = 3); abline(h = grand, lty = 2, col = "grey50")
legend("topleft", bty = "n", cex = 0.85, legend = c("n = 4 weeks", "n = 8", "n = 16 or more"), col = c("#bb0000", "#e69f00", "#1f4e79"), pch = 16)
plot(n_s, shrink, pch = 16, col = "#1f4e79", log = "x", xlab = "weeks observed per store (log scale)", ylab = "weight on the store's own mean",
     main = expression(paste("Weight  ", sigma[alpha]^2 / (sigma[alpha]^2 + sigma[epsilon]^2 / n[s]))), ylim = c(0, 1))
dev.off()
