# Simulations and figures for Session 10 (Lecture 10: Regression Discontinuity).
# All data are simulated with known ground truth.
#
# Running example: SOX Section 404 at the $75M public-float threshold (the Session 9 DiD example, now in a
# single fiscal-2004 cross-section). Running variable x = log(float / $75M); accelerated filers (x >= 0) must
# have the auditor attest to internal controls. Outcome: log audit fees. True jump at the cutoff: +0.35.
#
# Run from this directory:  Rscript rdd_sims.R
# Produces fig_rd_*.pdf and tab_rd_*.tex. Dependencies: fixest, rdrobust, rddensity.

suppressPackageStartupMessages({
  library(fixest)
  library(rdrobust)
  library(rddensity)
})
set.seed(20261105)

pdf_open <- function(f, w = 7, h = 4.2) pdf(f, width = w, height = h, pointsize = 11)
col_treat <- "#1f4e79"; col_ctrl <- "#808080"; col_truth <- "#bb0000"; col_alt <- "#e69f00"
write_tab <- function(file, header, rows, align) {
  con <- file(file, "w")
  writeLines(sprintf("\\begin{tabular}{%s}\\toprule", align), con)
  writeLines(paste0(header, " \\\\ \\midrule"), con)
  for (r in rows) writeLines(if (r == "\\midrule") r else paste0(r, " \\\\"), con)
  writeLines("\\bottomrule\\end{tabular}", con)
  close(con)
}
fmt <- function(x, d = 3) formatC(x, format = "f", digits = d)
ci_txt <- function(lo, hi, d = 2) sprintf("[%s, %s]", fmt(lo, d), fmt(hi, d))
binmeans <- function(x, y, width = 0.1, lim = 1.5) {        # evenly spaced bins, never straddling the cutoff
  b <- floor(x / width); keep <- abs(x) <= lim
  m <- aggregate(cbind(x, y) ~ b, data.frame(x = x[keep], y = y[keep], b = b[keep]), mean)
  m[order(m$x), ]
}

# --------------------------------------------------------------------------
# The data-generating process
# --------------------------------------------------------------------------
tau <- 0.35
f0 <- function(x) 11 + 0.5 * x + 0.3 * tanh(2.5 * x)        # E[log audit fee | x] without SOX: smooth, curved
sim_sox <- function(N = 4000) {
  x <- rnorm(N, 0, 1.2)                                     # log(float / 75); same float distribution as Session 9
  u <- rnorm(N, 0, 0.25)                                    # persistent firm audit-cost shifter
  D <- as.integer(x >= 0)
  data.frame(x = x, u = u, D = D,
             y = f0(x) + tau * D + u + rnorm(N, 0, 0.2),    # fiscal 2004 log audit fee
             y_pre = f0(x) - 0.06 + u + rnorm(N, 0, 0.2))   # fiscal 2002 log audit fee (before SOX)
}
d <- sim_sox()

# --------------------------------------------------------------------------
# Figure: the sharp design
# --------------------------------------------------------------------------
bm <- binmeans(d$x, d$y)
pdf_open("fig_rd_sharp.pdf")
par(mar = c(4, 4, 1, 1))
plot(bm$x, bm$y, pch = 16, col = ifelse(bm$x >= 0, col_treat, col_ctrl), xlim = c(-1.5, 1.5),
     xlab = "log(public float / $75M)", ylab = "log audit fee, fiscal 2004")
xs <- seq(-1.5, 1.5, by = 0.01)
lines(xs[xs < 0], f0(xs[xs < 0]), lwd = 2, col = col_ctrl)
lines(xs[xs >= 0], f0(xs[xs >= 0]) + tau, lwd = 2, col = col_treat)
lines(xs[xs >= 0], f0(xs[xs >= 0]), lwd = 2, lty = 2, col = col_ctrl)
lines(xs[xs < 0], f0(xs[xs < 0]) + tau, lwd = 2, lty = 2, col = col_treat)
abline(v = 0, lty = 3)
arrows(0.04, f0(0), 0.04, f0(0) + tau, length = 0.08, code = 3, col = col_truth, lwd = 2)
text(0.08, f0(0) + tau / 2, expression(tau == 0.35), adj = 0, col = col_truth)
legend("topleft", bty = "n", cex = 0.9,
       legend = c("bin means (bins of 0.1)", "E[Y(0) | x]", "E[Y(1) | x]", "dashed: never observed"),
       col = c("black", col_ctrl, col_treat, "black"), pch = c(16, NA, NA, NA), lwd = c(NA, 2, 2, 2),
       lty = c(NA, 1, 1, 2))
invisible(dev.off())

# --------------------------------------------------------------------------
# Global polynomials vs local linear; rdrobust; hand local linear
# --------------------------------------------------------------------------
global_poly <- function(d, p) {
  fit <- feols(as.formula(sprintf("y ~ D * poly(x, %d, raw = TRUE)", p)), d, vcov = "hetero")
  c(est = unname(coef(fit)["D"]), se = unname(se(fit)["D"]))
}
local_lin <- function(d, h, yvar = "y") {                    # triangular kernel, separate slopes
  s <- d[abs(d$x) < h, ]; s$k <- 1 - abs(s$x) / h
  fit <- feols(as.formula(paste(yvar, "~ D * x")), s, weights = ~k, vcov = "hetero")
  c(est = unname(coef(fit)["D"]), se = unname(se(fit)["D"]))
}
rd <- rdrobust(d$y, d$x)
h_mse <- rd$bws["h", "left"]
ll <- local_lin(d, h_mse)
rd_h <- rdrobust(d$y, d$x, h = h_mse)
stopifnot(abs(ll["est"] - rd_h$coef["Conventional", 1]) < 1e-8)   # hand WLS = rdrobust's conventional estimate
cat(sprintf("h_MSE = %.3f ; local linear %.4f ; rdrobust conventional %.4f ; bias-corrected %.4f, robust CI [%.3f, %.3f]\n",
            h_mse, ll["est"], rd$coef[1], rd$coef[2], rd$ci["Robust", 1], rd$ci["Robust", 2]))

gp <- t(sapply(1:5, function(p) global_poly(d, p)))
write_tab("tab_rd_poly.tex", "Specification & Estimate & 95\\% CI & Obs.\\ used",
  c(sapply(1:5, function(p) sprintf("Global polynomial, order %d each side & %s & %s & %s", p,
           fmt(gp[p, 1]), ci_txt(gp[p, 1] - 1.96 * gp[p, 2], gp[p, 1] + 1.96 * gp[p, 2]), format(nrow(d), big.mark = ","))),
    "\\midrule",
    sprintf("Local linear, triangular, $h = %s$ (by hand, \\texttt{feols}) & %s & %s & %s", fmt(h_mse, 2),
            fmt(ll["est"]), ci_txt(ll["est"] - 1.96 * ll["se"], ll["est"] + 1.96 * ll["se"]),
            format(sum(abs(d$x) < h_mse), big.mark = ",")),
    sprintf("\\texttt{rdrobust}: bias-corrected, robust CI & %s & %s & %s", fmt(rd$coef[2]),
            ci_txt(rd$ci["Robust", 1], rd$ci["Robust", 2]), format(sum(rd$N_h), big.mark = ",")),
    "\\midrule", sprintf("Truth & %s & & ", fmt(tau))),
  "lccc")

# Implied weights: every estimator here is linear in y, tau_hat = sum_i omega_i y_i
side_weights <- function(xs, p, k = rep(1, length(xs))) {
  X <- outer(xs, 0:p, `^`); W <- diag(k)
  (solve(t(X) %*% W %*% X) %*% t(X) %*% W)[1, ]
}
wt <- function(p, kern = FALSE, h = Inf) {
  om <- numeric(nrow(d)); for (s in c(0, 1)) {
    idx <- which(d$D == s & abs(d$x) < h); k <- if (kern) 1 - abs(d$x[idx]) / h else rep(1, length(idx))
    om[idx] <- side_weights(d$x[idx], p, k) * ifelse(s == 1, 1, -1) }
  om }
o <- order(d$x)
pdf_open("fig_rd_weights.pdf", w = 8, h = 3.6)
par(mfrow = c(1, 3), mar = c(4, 4, 2, 1))
for (spec in list(list(p = 1, k = FALSE, h = Inf, m = "global linear"),
                  list(p = 4, k = FALSE, h = Inf, m = "global quartic"),
                  list(p = 1, k = TRUE, h = h_mse, m = sprintf("local linear, h = %.2f", h_mse)))) {
  om <- wt(spec$p, spec$k, spec$h) * nrow(d)
  plot(d$x[o], om[o], type = "l", lwd = 1.5, col = col_treat, xlab = "x", ylab = "weight x N",
       main = spec$m, cex.main = 1, ylim = c(-40, 40), xlim = c(-3.5, 3.5))
  abline(h = 0, lty = 3); abline(v = 0, lty = 2, col = "grey40")
}
invisible(dev.off())

# Bandwidth tradeoff
hs <- seq(0.1, 2, by = 0.05)
bw <- t(sapply(hs, function(h) local_lin(d, h)))
pdf_open("fig_rd_bandwidth.pdf")
par(mar = c(4, 4, 1, 1))
plot(hs, bw[, 1], type = "n", ylim = range(c(bw[, 1] - 1.96 * bw[, 2], bw[, 1] + 1.96 * bw[, 2], tau)),
     xlab = "bandwidth h (log points of float)", ylab = "local linear estimate of the jump")
polygon(c(hs, rev(hs)), c(bw[, 1] - 1.96 * bw[, 2], rev(bw[, 1] + 1.96 * bw[, 2])), col = "#1f4e7933", border = NA)
lines(hs, bw[, 1], lwd = 2, col = col_treat)
abline(h = tau, col = col_truth, lwd = 2); abline(v = h_mse, lty = 2, col = "grey40")
text(h_mse, max(bw[, 1] + 1.96 * bw[, 2]), sprintf(" MSE-optimal h = %.2f", h_mse), adj = c(0, 1), cex = 0.9)
legend("bottomright", bty = "n", legend = c("estimate, 95% CI", "truth"), col = c(col_treat, col_truth), lwd = 2)
invisible(dev.off())

# --------------------------------------------------------------------------
# Monte Carlo: coverage of conventional vs robust bias-corrected CIs (and a global quartic)
# --------------------------------------------------------------------------
R <- 500
mc <- t(replicate(R, {
  s <- sim_sox(); r <- rdrobust(s$y, s$x); g <- global_poly(s, 4)
  c(conv = r$coef[1], conv_lo = r$ci[1, 1], conv_hi = r$ci[1, 2], bc = r$coef[2],
    rb_lo = r$ci[3, 1], rb_hi = r$ci[3, 2], h = r$bws[1, 1],
    gq = g[["est"]], gq_lo = g[["est"]] - 1.96 * g[["se"]], gq_hi = g[["est"]] + 1.96 * g[["se"]])
}))
cover <- function(lo, hi) mean(lo <= tau & tau <= hi)
mc_row <- function(lab, est, lo, hi) sprintf("%s & %s & %s & %s & %s", lab, fmt(mean(est) - tau), fmt(sd(est)),
                                             fmt(cover(lo, hi), 2), fmt(mean(hi - lo)))
write_tab("tab_rd_coverage.tex", "Estimator and CI & Bias & Std.\\ dev. & Coverage of 95\\% CI & CI length",
  c(mc_row("Global quartic, robust SE", mc[, "gq"], mc[, "gq_lo"], mc[, "gq_hi"]),
    mc_row("Local linear at $h_{MSE}$, conventional CI", mc[, "conv"], mc[, "conv_lo"], mc[, "conv_hi"]),
    mc_row("Local linear at $h_{MSE}$, robust bias-corrected CI", mc[, "conv"], mc[, "rb_lo"], mc[, "rb_hi"])),
  "lcccc")
cat(sprintf("MC (%d reps): mean h %.2f ; coverage conv %.3f, RBC %.3f, global quartic %.3f\n", R, mean(mc[, "h"]),
            cover(mc[, "conv_lo"], mc[, "conv_hi"]), cover(mc[, "rb_lo"], mc[, "rb_hi"]), cover(mc[, "gq_lo"], mc[, "gq_hi"])))

# --------------------------------------------------------------------------
# Manipulation: float management just above $75M (Gao, Wu and Zimmerman 2009)
# Firms with true float in [$75M, $96M] and high audit costs (high u) push reported float just under $75M.
# They stay exempt (D = 0) but keep the audit fees of their true size and cost.
# --------------------------------------------------------------------------
dm <- d
cand <- which(dm$x >= 0 & dm$x < 0.25)
manip <- cand[runif(length(cand)) < plogis(-0.5 + 8 * dm$u[cand])]
dm$x[manip] <- runif(length(manip), -0.12, -0.005)
dm$D[manip] <- 0L
dm$y[manip] <- dm$y[manip] - tau
cat(sprintf("Manipulators: %d of %d eligible firms\n", length(manip), length(cand)))

pdf_open("fig_rd_density.pdf", w = 8, h = 3.6)
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
br <- seq(-1, 1, by = 0.04)
for (s in list(list(x = d$x, m = "no manipulation"), list(x = dm$x, m = "float management"))) {
  xx <- s$x[abs(s$x) < 1]
  h <- hist(xx, breaks = br, plot = FALSE)
  barplot(h$counts, space = 0, col = ifelse(h$mids < 0, col_ctrl, col_treat), border = "white",
          main = s$m, cex.main = 1, ylab = "firms per bin", xlab = "log(public float / $75M)")
  axis(1, at = (c(-1, -0.5, 0, 0.5, 1) + 1) / 0.04, labels = c(-1, -0.5, 0, 0.5, 1))
  abline(v = 1 / 0.04, lty = 2)
}
invisible(dev.off())

dens_p <- function(x) rddensity(x)$test$p_jk
rd_row <- function(s, yvar = "y") { r <- rdrobust(s[[yvar]], s$x); c(r$coef[2], r$ci[3, ]) }
donut <- function(s, delta) s[abs(s$x) >= delta, ]
samples <- list(list(lab = "No manipulation", s = d, dens = TRUE),
                list(lab = "Float management", s = dm, dens = TRUE),
                list(lab = "Float management, donut $|x| < 0.15$ dropped", s = donut(dm, 0.15), dens = FALSE),
                list(lab = "Float management, donut $|x| < 0.30$ dropped", s = donut(dm, 0.30), dens = FALSE))
man_rows <- sapply(samples, function(z) {
  a <- rd_row(z$s); b <- rd_row(z$s, "y_pre")
  sprintf("%s & %s %s & %s %s & %s", z$lab, fmt(a[1]), ci_txt(a[2], a[3]), fmt(b[1]), ci_txt(b[2], b[3]),
          if (z$dens) fmt(dens_p(z$s$x)) else "---") })
write_tab("tab_rd_manip.tex", "Sample & Jump in 2004 fees (truth 0.35) & Jump in 2002 fees (truth 0) & Density test $p$",
          c(man_rows), "lccc")
print(man_rows)

# --------------------------------------------------------------------------
# Burgstahler-Dichev (1997)-style earnings histogram: small losses turned into small profits
# --------------------------------------------------------------------------
e <- rnorm(20000, 0.03, 0.08)
mv <- which(e > -0.02 & e < 0 & runif(length(e)) < 0.5)
e[mv] <- runif(length(mv), 0, 0.01)
pdf_open("fig_rd_bd.pdf", w = 7, h = 3.6)
par(mar = c(4, 4, 1, 1))
br <- seq(-0.15, 0.20, by = 0.005); ee <- e[e > -0.15 & e < 0.20]
h <- hist(ee, breaks = br, plot = FALSE)
barplot(h$counts, space = 0, border = "white", col = ifelse(h$mids < 0, col_ctrl, col_treat),
        ylab = "firm-years per bin", xlab = "net income / beginning market value")
axis(1, at = (c(-0.15, -0.1, -0.05, 0, 0.05, 0.1, 0.15, 0.2) + 0.15) / 0.005, labels = c(-0.15, -0.1, -0.05, 0, 0.05, 0.1, 0.15, 0.2))
abline(v = 0.15 / 0.005, lty = 2)
invisible(dev.off())
bd_p <- rddensity(e)$test$p_jk
cat(sprintf("BD density test p = %.2g\n", bd_p))
writeLines(sprintf("%s", if (bd_p < 0.001) "$<0.001$" else fmt(bd_p)), "tab_rd_bd_p.tex")

# --------------------------------------------------------------------------
# Fuzzy RD: voluntary compliers below the cutoff, extensions above it
# Compliance depends on the cost shifter u (low-cost firms comply early), so OLS on D is biased.
# --------------------------------------------------------------------------
df <- d
Z <- as.integer(df$x >= 0)
df$D <- as.integer(-1.3 + 2.4 * Z - 4 * df$u + rnorm(nrow(df)) > 0)
df$y <- f0(df$x) + tau * df$D + df$u + rnorm(nrow(df), 0, 0.2)
df$Z <- Z
rf <- rdrobust(df$y, df$x, fuzzy = df$D)
hf <- rf$bws["h", "left"]
s <- df[abs(df$x) < hf, ]; s$k <- 1 - abs(s$x) / hf
fs <- feols(D ~ Z * x, s, weights = ~k, vcov = "hetero")
rfm <- feols(y ~ Z * x, s, weights = ~k, vcov = "hetero")
ols <- feols(y ~ D + x + Z:x, s, weights = ~k, vcov = "hetero")
iv <- feols(y ~ x + Z:x | D ~ Z, s, weights = ~k, vcov = "hetero")
wald <- coef(rfm)["Z"] / coef(fs)["Z"]
stopifnot(abs(wald - coef(iv)["fit_D"]) < 1e-8, abs(wald - rf$coef[1]) < 1e-6)   # Wald = 2SLS = rdrobust conventional
cat(sprintf("Fuzzy: share complying below %.2f, above %.2f ; first stage %.3f ; Wald %.3f ; rdrobust fuzzy %.3f\n",
            mean(df$D[df$x < 0]), mean(df$D[df$x >= 0]), coef(fs)["Z"], wald, rf$coef[2]))
ci_c <- function(f, v) ci_txt(coef(f)[v] - 1.96 * se(f)[v], coef(f)[v] + 1.96 * se(f)[v])
write_tab("tab_rd_fuzzy.tex", sprintf("Within $h = %s$, triangular kernel & Estimate & 95\\%% CI", fmt(hf, 2)),
  c(sprintf("OLS: $y$ on compliance $D$ (and $x$ terms) & %s & %s", fmt(coef(ols)["D"]), ci_c(ols, "D")),
    sprintf("First stage: jump in $\\Pr(D \\mid x)$ & %s & %s", fmt(coef(fs)["Z"]), ci_c(fs, "Z")),
    sprintf("Reduced form: jump in $\\E[y \\mid x]$ & %s & %s", fmt(coef(rfm)["Z"]), ci_c(rfm, "Z")),
    sprintf("Wald ratio = 2SLS with $Z = \\mathbf{1}\\{x \\ge 0\\}$ as instrument & %s & %s", fmt(wald), ci_c(iv, "fit_D")),
    sprintf("\\texttt{rdrobust(..., fuzzy = D)}: bias-corrected, robust CI & %s & %s", fmt(rf$coef[2]), ci_txt(rf$ci[3, 1], rf$ci[3, 2])),
    "\\midrule", sprintf("Truth (every firm's effect) & %s & ", fmt(tau))),
  "lcc")

bmD <- binmeans(df$x, df$D); bmY <- binmeans(df$x, df$y)
pdf_open("fig_rd_fuzzy.pdf", w = 8, h = 3.6)
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
plot(bmD$x, bmD$y, pch = 16, col = ifelse(bmD$x >= 0, col_treat, col_ctrl), ylim = c(0, 1), xlim = c(-1.5, 1.5),
     xlab = "log(public float / $75M)", ylab = "share with auditor attestation", main = "first stage", cex.main = 1)
abline(v = 0, lty = 3)
plot(bmY$x, bmY$y, pch = 16, col = ifelse(bmY$x >= 0, col_treat, col_ctrl), xlim = c(-1.5, 1.5),
     xlab = "log(public float / $75M)", ylab = "log audit fee", main = "reduced form", cex.main = 1)
abline(v = 0, lty = 3)
invisible(dev.off())

# --------------------------------------------------------------------------
# Binscatter with a control: residualizing x is wrong; bin dummies + control is right
# --------------------------------------------------------------------------
n <- 3000
xb <- runif(n, -2, 2); w <- 0.8 * xb + rnorm(n, 0, 0.6)
mu <- function(x) x^2
yb <- mu(xb) + 1.0 * w + rnorm(n, 0, 0.7)
q <- cut(xb, quantile(xb, 0:20 / 20), include.lowest = TRUE)
ry <- resid(lm(yb ~ w)); rx <- resid(lm(xb ~ w))
qr <- cut(rx, quantile(rx, 0:20 / 20), include.lowest = TRUE)
naive <- data.frame(x = tapply(rx, qr, mean) + mean(xb), y = tapply(ry, qr, mean) + mean(yb))
fitb <- lm(yb ~ 0 + q + w)
right <- data.frame(x = tapply(xb, q, mean), y = coef(fitb)[1:20] + coef(fitb)["w"] * mean(w))
pdf_open("fig_rd_binscatter.pdf", w = 8, h = 3.6)
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
xs <- seq(-2, 2, by = 0.01); ctr <- mean(yb) - mean(mu(xb) + w)
for (z in list(list(b = naive, m = "residualize y and x on w, then bin"),
               list(b = right, m = "bin dummies + w in one regression"))) {
  plot(z$b$x, z$b$y, pch = 16, col = col_treat, xlim = c(-2, 2), ylim = c(-1, 5), main = z$m, cex.main = 1,
       xlab = "x", ylab = "y, holding w at its mean")
  lines(xs, mu(xs) + mean(w) + ctr, col = col_truth, lwd = 2)
}
legend("top", bty = "n", legend = c("binned estimate", "true mu(x)"), col = c(col_treat, col_truth),
       pch = c(16, NA), lwd = c(NA, 2), cex = 0.9)
invisible(dev.off())

cat("done.\n")
