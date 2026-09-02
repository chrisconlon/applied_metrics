# Synthetic difference-in-differences vs synthetic control vs DiD (Session 11 addendum).
# Factor model with a trend factor; one treated city, 30 donors; tax cuts sales 10% from quarter 13.
# Dependencies: fixest. Follows Arkhangelsky, Athey, Hirshberg, Imbens and Wager (2021).
suppressPackageStartupMessages(library(fixest)); set.seed(66)
J <- 30; Tq <- 20; T0 <- 12
simplex <- function(th) exp(th) / sum(exp(th))
sc_w <- function(y1, Y0) { obj <- function(th) sum((y1[1:T0] - Y0[1:T0, ] %*% simplex(th))^2)
  simplex(optim(rep(0, ncol(Y0)), obj, method = "BFGS", control = list(maxit = 2000))$par) }
sdid <- function(Y, T0) {                                   # Y: Tq x (1 + J), column 1 treated
  y1 <- Y[, 1]; Y0 <- Y[, -1]; Tpost <- nrow(Y) - T0
  zeta <- (Tpost^(1/4)) * sd(diff(Y0[1:T0, ]))               # regularization as in AAHIW
  obj_w <- function(th) { w <- simplex(th[-1]); sum((th[1] + Y0[1:T0, ] %*% w - y1[1:T0])^2) + zeta^2 * T0 * sum(w^2) }
  pw <- optim(c(0, rep(0, J)), obj_w, method = "BFGS", control = list(maxit = 3000))$par; w <- simplex(pw[-1])
  post_mean <- colMeans(Y0[(T0 + 1):nrow(Y), , drop = FALSE])
  obj_l <- function(th) { l <- simplex(th[-1]); sum((th[1] + t(Y0[1:T0, ]) %*% l - post_mean)^2) }
  pl <- optim(c(0, rep(0, T0)), obj_l, method = "BFGS", control = list(maxit = 3000))$par; l <- simplex(pl[-1])
  d <- expand.grid(t = 1:nrow(Y), unit = 1:(J + 1)); d$y <- as.vector(Y); d$W <- as.integer(d$unit == 1 & d$t > T0)
  d$wt <- ifelse(d$unit == 1, 1, w[d$unit - 1]) * ifelse(d$t > T0, 1 / Tpost, l[d$t])
  list(tau = coef(feols(y ~ W | unit + t, d, weights = ~wt))[1], w = w, l = l)
}
run <- function(mu1, alpha1 = 0, sd_alpha = 0) {
  lam <- cbind(seq(0, 2, length.out = Tq), rnorm(Tq)); mu <- matrix(runif((J + 1) * 2), J + 1, 2); mu[1, ] <- c(mu1, 0.5)
  alpha <- c(alpha1, rnorm(J, 0, sd_alpha))                  # unit level shifts; treated may sit above every donor
  Y <- 100 + matrix(alpha, Tq, J + 1, byrow = TRUE) + 5 * lam %*% t(mu) + matrix(rnorm(Tq * (J + 1)), Tq, J + 1)
  truth <- -0.1 * mean(Y[(T0 + 1):Tq, 1]); Y[(T0 + 1):Tq, 1] <- 0.9 * Y[(T0 + 1):Tq, 1]
  y1 <- Y[, 1]; Y0 <- Y[, -1]
  did <- mean(y1[-(1:T0)]) - mean(y1[1:T0]) - (mean(Y0[-(1:T0), ]) - mean(Y0[1:T0, ]))
  w <- sc_w(y1, Y0); sc <- mean((y1 - Y0 %*% w)[-(1:T0)])
  s <- sdid(Y, T0)
  c(truth = truth, did = did, sc = sc, sdid = unname(s$tau), pre_rmspe_sc = sqrt(mean((y1 - Y0 %*% w)[1:T0]^2)))
}
set.seed(66); in_hull <- run(0.9); set.seed(66); level <- run(0.9, alpha1 = 12, sd_alpha = 5); set.seed(66); out_hull <- run(1.3)
print(round(rbind(in_hull, level, out_hull), 2))
set.seed(1); reps <- t(replicate(60, run(0.9, alpha1 = 12, sd_alpha = 5)))
bias <- colMeans(reps[, 2:4] - reps[, 1]); rmse <- sqrt(colMeans((reps[, 2:4] - reps[, 1])^2))
print(round(rbind(bias, rmse), 2))
write_tab <- function(file, header, rows, align) { con <- file(file, "w"); writeLines(sprintf("\\begin{tabular}{%s}\\toprule", align), con)
  writeLines(paste0(header, " \\\\ \\midrule"), con); for (r in rows) writeLines(paste0(r, " \\\\"), con); writeLines("\\bottomrule\\end{tabular}", con); close(con) }
fmt <- function(x, d = 1) formatC(x, format = "f", digits = d)
write_tab("tab_sdid.tex",
  " & DiD (all donors) & Synthetic control & Synthetic DiD & Truth",
  c(sprintf("Trends differ, levels similar (treated inside the hull) & %s & %s & %s & %s", fmt(in_hull["did"]), fmt(in_hull["sc"]), fmt(in_hull["sdid"]), fmt(in_hull["truth"])),
    sprintf("Trends differ \\emph{and} treated level above every donor & %s & %s & %s & %s", fmt(level["did"]), fmt(level["sc"]), fmt(level["sdid"]), fmt(level["truth"])),
    sprintf("Treated trend outside the hull (loading $1.3$) & %s & %s & %s & %s", fmt(out_hull["did"]), fmt(out_hull["sc"]), fmt(out_hull["sdid"]), fmt(out_hull["truth"])),
    sprintf("\\midrule Bias, 60 draws of the level-shift design & %s & %s & %s & ", fmt(bias["did"], 2), fmt(bias["sc"], 2), fmt(bias["sdid"], 2)),
    sprintf("RMSE, 60 draws of the level-shift design & %s & %s & %s & ", fmt(rmse["did"], 2), fmt(rmse["sc"], 2), fmt(rmse["sdid"], 2))),
  "lcccc")
cat("done.\n")
