# Simulations and figures for Session 9 (Lecture 9a: Difference-in-Differences).
# All data are simulated with known ground truth.
#
# Run from this directory:  Rscript did_sims.R
# Produces fig_did_*.pdf and tab_did_*.tex, which the deck \includegraphics / \input.

suppressPackageStartupMessages({
  library(fixest)
  library(data.table)
})
set.seed(20261029)

pdf_open <- function(f, w = 7, h = 4.2) pdf(f, width = w, height = h, pointsize = 11)
col_treat <- "#1f4e79"; col_ctrl <- "#808080"; col_truth <- "#bb0000"; col_alt <- "#e69f00"

# helper: write a booktabs tabular fragment
write_tab <- function(file, header, rows, align) {
  con <- file(file, "w")
  writeLines(sprintf("\\begin{tabular}{%s}\\toprule", align), con)
  writeLines(paste0(header, " \\\\ \\midrule"), con)
  for (r in rows) writeLines(if (r == "\\midrule") r else paste0(r, " \\\\"), con)
  writeLines("\\bottomrule\\end{tabular}", con)
  close(con)
}
fmt <- function(x, d = 3) formatC(x, format = "f", digits = d)
fmt_se <- function(x, d = 3) paste0("(", fmt(x, d), ")")

# --------------------------------------------------------------------------
# Part A. A SOX-404-style mandate. Firms with public float >= $75M
# ("accelerated filers") must comply from fiscal 2004; smaller firms are exempt.
# Outcome: log audit fees. Larger firms are on a steeper audit-fee trend, so
# parallel trends fails unconditionally but holds conditional on size.
# True effect: +0.35 in 2004, decaying to +0.20 as auditors learn.
# --------------------------------------------------------------------------
years <- 2000:2007; Y0 <- 2004
tau_sox <- c(`2004` = 0.35, `2005` = 0.28, `2006` = 0.22, `2007` = 0.20)
sim_sox <- function(N = 2000, kappa = 0.06, sd_eps = 0.12) {
  lfloat <- rnorm(N, log(75), 1.2)                      # log public float, $M
  size <- (lfloat - mean(lfloat)) / sd(lfloat)          # standardized size
  d <- CJ(firm = 1:N, year = years)
  d[, `:=`(size = size[firm], acc = as.integer(lfloat[firm] >= log(75)))]
  d[, alpha := 11 + 0.5 * size + rnorm(N, 0, 0.4)[firm]] # bigger firms pay more
  gam <- cumsum(c(0, rnorm(length(years) - 1, 0.03, 0.02)))
  d[, post := as.integer(year >= Y0)]
  d[, tau := 0]
  d[acc == 1 & post == 1, tau := tau_sox[as.character(year)]]
  d[, y0 := alpha + gam[year - min(years) + 1] + kappa * size * (year - (Y0 - 1)) + rnorm(.N, 0, sd_eps)]
  d[, y := y0 + tau]
  d[, D := acc * post]
  d[]
}
sox <- sim_sox()
cat(sprintf("SOX: %d firms, %.0f%% accelerated filers; mean size | acc = %.2f, | exempt = %.2f\n",
            uniqueN(sox$firm), 100 * mean(sox[year == 2000]$acc),
            mean(sox[year == 2000 & acc == 1]$size), mean(sox[year == 2000 & acc == 0]$size)))

# ---- The 2x2 (fiscal 2003 vs 2004), four ways ----
two <- sox[year %in% c(2003, 2004)]
cm <- two[, .(y = mean(y)), by = .(acc, year)]
g <- function(a, yr) cm[acc == a & year == yr]$y
dd_means <- (g(1, 2004) - g(1, 2003)) - (g(0, 2004) - g(0, 2003))
w <- dcast(two, firm + acc + size ~ year, value.var = "y")
setnames(w, c("2003", "2004"), c("y03", "y04"))
w[, dy := y04 - y03]
fit_fd <- feols(dy ~ acc, w, vcov = "hetero")
fit_int <- feols(y ~ acc * post, two, vcov = ~firm)
fit_twfe <- feols(y ~ D | firm + year, two, vcov = ~firm)
ests4 <- c(dd_means, coef(fit_fd)["acc"], coef(fit_int)["acc:post"], coef(fit_twfe)["D"])
stopifnot(max(abs(ests4 - dd_means)) < 1e-10)
cat(sprintf("2x2 DiD (four routes agree): %.4f ; truth in 2004: %.2f\n", dd_means, tau_sox["2004"]))
write_tab("tab_did_fourways.tex", "Route & Estimate & Std.\\ error",
  c(sprintf("(1) Cell means: $(\\bar y_{T,04}-\\bar y_{T,03})-(\\bar y_{C,04}-\\bar y_{C,03})$ & %s & ---", fmt(dd_means, 4)),
    sprintf("(2) First differences: $\\Delta y_i$ on $D_i$ (robust) & %s & %s", fmt(ests4[2], 4), fmt_se(se(fit_fd)["acc"])),
    sprintf("(3) Interaction: $y_{it}$ on $D_i$, $Post_t$, $D_i \\times Post_t$ (cluster firm) & %s & %s",
            fmt(ests4[3], 4), fmt_se(se(fit_int)["acc:post"])),
    sprintf("(4) Two-way FE: $y_{it}$ on $D_{it}$ $\\mid$ firm, year (cluster firm) & %s & %s",
            fmt(ests4[4], 4), fmt_se(se(fit_twfe)["D"])),
    "\\midrule",
    sprintf("True effect on accelerated filers in 2004 & %s & ", fmt(tau_sox[["2004"]], 4))),
  "lcc")

# ---- Comparisons against the truth ----
post_gap <- feols(y ~ acc, two[year == 2004], vcov = "hetero")
before_after <- feols(dy ~ 1, w[acc == 1], vcov = "hetero")
fit_cond <- feols(dy ~ acc + size, w, vcov = "hetero")    # = DiD with size x year controls
write_tab("tab_did_sox.tex", "Comparison & Estimate & Std.\\ error & Truth",
  c(sprintf("Post-period gap: accelerated $-$ exempt, 2004 & %s & %s & %s",
            fmt(coef(post_gap)["acc"]), fmt_se(se(post_gap)["acc"]), fmt(tau_sox[["2004"]], 2)),
    sprintf("Before--after, accelerated filers, 2003$\\to$2004 & %s & %s & %s",
            fmt(coef(before_after)[1]), fmt_se(se(before_after)[1]), fmt(tau_sox[["2004"]], 2)),
    sprintf("Difference-in-differences & %s & %s & %s",
            fmt(coef(fit_fd)["acc"]), fmt_se(se(fit_fd)["acc"]), fmt(tau_sox[["2004"]], 2)),
    sprintf("DiD with size $\\times$ year controls & %s & %s & %s",
            fmt(coef(fit_cond)["acc"]), fmt_se(se(fit_cond)["acc"]), fmt(tau_sox[["2004"]], 2))),
  "lccc")
cat(sprintf("Post gap %.3f ; before-after %.3f ; DiD %.3f ; DiD + size trend %.3f\n",
            coef(post_gap)["acc"], coef(before_after)[1], coef(fit_fd)["acc"], coef(fit_cond)["acc"]))

# ---- Raw trends, with the treated group's true counterfactual ----
tr <- sox[, .(y = mean(y), y0 = mean(y0)), by = .(acc, year)][order(acc, year)]
tr[, `:=`(y = y - y[year == 2003], y0 = y0 - y0[year == 2003]), by = acc]
pdf_open("fig_did_sox_trends.pdf")
par(mar = c(4, 4, 1, 1))
plot(NA, xlim = range(years), ylim = range(c(tr$y, tr$y0)), xlab = "fiscal year",
     ylab = "mean log audit fee, relative to 2003")
abline(v = Y0 - 0.5, lty = 2, col = "grey40"); abline(h = 0, lty = 3)
with(tr[acc == 0], lines(year, y, type = "b", pch = 16, lwd = 2, col = col_ctrl))
with(tr[acc == 1], lines(year, y, type = "b", pch = 16, lwd = 2, col = col_treat))
with(tr[acc == 1], lines(year, y0, lwd = 2, lty = 2, col = col_truth))
legend("topleft", bty = "n",
       legend = c("accelerated filers (comply from 2004)", "exempt firms (float < $75M)",
                  "accelerated filers without SOX (true counterfactual)"),
       col = c(col_treat, col_ctrl, col_truth), lwd = 2, lty = c(1, 1, 2), pch = c(16, 16, NA))
invisible(dev.off())

# ---- Event studies on the full panel: unconditional vs size x year ----
fit_es_u <- feols(y ~ i(year, acc, ref = 2003) | firm + year, sox, vcov = ~firm)
fit_es_c <- feols(y ~ i(year, acc, ref = 2003) + i(year, size, ref = 2003) | firm + year, sox, vcov = ~firm)
es_tab <- function(fit) {
  ct <- coeftable(fit); ct <- ct[grepl("::.*:acc$", rownames(ct)), , drop = FALSE]
  yr <- as.integer(sub("year::(\\d+):acc", "\\1", rownames(ct)))
  out <- rbind(data.table(year = yr, b = ct[, 1], s = ct[, 2]), data.table(year = 2003L, b = 0, s = 0))
  out[order(year)]
}
truth_path <- c(rep(0, sum(years < Y0)), tau_sox)
plot_sox_es <- function(e, main) {
  plot(e$year, e$b, pch = 16, col = col_treat, ylim = c(-0.35, 0.65), main = main,
       xlab = "fiscal year", ylab = "estimate relative to 2003", cex.main = 1)
  segments(e$year, e$b - 1.96 * e$s, e$year, e$b + 1.96 * e$s, col = col_treat)
  lines(years, truth_path, col = col_truth, lwd = 2)
  abline(h = 0, lty = 3); abline(v = Y0 - 0.5, lty = 2, col = "grey40")
}
pdf_open("fig_did_sox_es.pdf", w = 8, h = 3.8)
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
plot_sox_es(es_tab(fit_es_u), "firm + year FE")
legend("topleft", bty = "n", legend = c("estimate, 95% CI", "truth"), col = c(col_treat, col_truth),
       pch = c(16, NA), lwd = c(1, 2), cex = 0.9)
plot_sox_es(es_tab(fit_es_c), "adding size x year")
invisible(dev.off())
e_u <- es_tab(fit_es_u); e_c <- es_tab(fit_es_c)
cat(sprintf("ES 2000 coef: unconditional %.3f, conditional %.3f ; 2004: %.3f vs %.3f (truth 0.35)\n",
            e_u[year == 2000]$b, e_c[year == 2000]$b, e_u[year == 2004]$b, e_c[year == 2004]$b))

# ---- Stylized 2x2 picture ----
pdf_open("fig_did_2x2.pdf", w = 6.5, h = 4)
par(mar = c(3, 4, 1, 1))
yc <- c(10.0, 10.3); yt <- c(10.8, 11.45); ycf <- yt[1] + diff(yc)
plot(NA, xlim = c(0.7, 2.6), ylim = c(9.9, 11.6), xaxt = "n", xlab = "", ylab = "mean outcome")
axis(1, at = 1:2, labels = c("before", "after"))
lines(1:2, yc, type = "b", pch = 16, lwd = 2, col = col_ctrl)
lines(1:2, yt, type = "b", pch = 16, lwd = 2, col = col_treat)
lines(1:2, c(yt[1], ycf), lwd = 2, lty = 2, col = col_truth)
arrows(2.1, ycf, 2.1, yt[2], code = 3, length = 0.06, lwd = 2)
text(2.15, mean(c(ycf, yt[2])), "ATT", adj = 0, font = 2)
text(2.03, ycf - 0.1, "treated without treatment\n(the counterfactual)", adj = c(0, 1), cex = 0.8, col = col_truth)
text(2.03, yt[2] + 0.06, "treated", adj = 0, cex = 0.8, col = col_treat)
text(2.03, yc[2], "control", adj = 0, cex = 0.8, col = col_ctrl)
invisible(dev.off())

# --------------------------------------------------------------------------
# Part B. Parallel trends depends on the scale. Small and large stores both
# grow 4% a period; a promotion at t = 4 has no effect on anyone.
# --------------------------------------------------------------------------
tt <- 1:7
small <- 100 * 1.04^(tt - 1); large <- 300 * 1.04^(tt - 1)
dd_lev <- (large[4] - large[3]) - (small[4] - small[3])
dd_log <- round((log(large[4]) - log(large[3])) - (log(small[4]) - log(small[3])), 10) + 0
pdf_open("fig_did_logs.pdf", w = 8, h = 3.6)
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
plot(tt, large, type = "b", pch = 16, lwd = 2, col = col_treat, ylim = c(80, 520),
     xlab = "period", ylab = "weekly sales ($000)", main = "levels", cex.main = 1)
lines(tt, small, type = "b", pch = 16, lwd = 2, col = col_ctrl); abline(v = 3.5, lty = 2, col = "grey40")
legend("topleft", bty = "n", cex = 0.85, legend = c("large stores (promoted)", "small stores",
       sprintf("DiD, period 3 to 4: %+.1f", dd_lev)), col = c(col_treat, col_ctrl, NA), lwd = 2, pch = c(16, 16, NA))
plot(tt, log(large), type = "b", pch = 16, lwd = 2, col = col_treat, ylim = log(c(80, 400)),
     xlab = "period", ylab = "log weekly sales", main = "logs", cex.main = 1)
lines(tt, log(small), type = "b", pch = 16, lwd = 2, col = col_ctrl); abline(v = 3.5, lty = 2, col = "grey40")
legend("bottomright", bty = "n", cex = 0.85, legend = sprintf("DiD, period 3 to 4: %+.3f", dd_log))
invisible(dev.off())
cat(sprintf("Levels vs logs: DiD in levels %.2f, in logs %.4f (truth 0)\n", dd_lev, dd_log))

# --------------------------------------------------------------------------
# Part C. Ashenfelter dip. Firms hire a turnaround consultant in year 6 if their
# year-5 log sales are in the bottom fifth. The consultant does nothing.
# Transitory shocks are AR(1) with rho = 0.6, so bad years partly recover.
# --------------------------------------------------------------------------
sim_dip <- function(N = 4000, T = 9, rho = 0.6, sd_v = 0.12) {
  alpha <- rnorm(N, 0, 0.3)
  u <- matrix(0, N, T); u[, 1] <- rnorm(N, 0, sd_v / sqrt(1 - rho^2))
  for (t in 2:T) u[, t] <- rho * u[, t - 1] + rnorm(N, 0, sd_v)
  gam <- cumsum(rnorm(T, 0.02, 0.01))
  y <- alpha + matrix(gam, N, T, byrow = TRUE) + u
  hire <- as.integer(y[, 5] < quantile(y[, 5], 0.2))
  d <- data.table(firm = rep(1:N, T), year = rep(1:T, each = N), y = as.vector(y), hire = rep(hire, T))
  d[]
}
dip <- sim_dip()
dm <- dip[, .(y = mean(y)), by = .(hire, year)][order(hire, year)]
dm[, y1 := y - y[year == 1], by = hire]   # each group relative to its own year 1
dd_base <- function(b, post = 7) {
  (dm[hire == 1 & year == post]$y - dm[hire == 1 & year == b]$y) -
    (dm[hire == 0 & year == post]$y - dm[hire == 0 & year == b]$y)
}
dip_rows <- sapply(c(5, 3, 1), dd_base)
pdf_open("fig_did_dip.pdf")
par(mar = c(4, 4, 1, 1))
plot(NA, xlim = c(1, 9), ylim = range(dm$y1) + c(0, 0.04), xlab = "year (consultant hired at the start of year 6)",
     ylab = "mean log sales, relative to year 1")
abline(v = 5.5, lty = 2, col = "grey40")
with(dm[hire == 0], lines(year, y1, type = "b", pch = 16, lwd = 2, col = col_ctrl))
with(dm[hire == 1], lines(year, y1, type = "b", pch = 16, lwd = 2, col = col_treat))
legend("topleft", bty = "n", legend = c("hired a consultant (bottom fifth in year 5)", "did not"),
       col = c(col_treat, col_ctrl), lwd = 2, pch = 16)
invisible(dev.off())
write_tab("tab_did_dip.tex", "Base year & DiD, year 7 vs base & Truth",
  c(sprintf("Year 5 (the bad year) & %s & 0", fmt(dip_rows[1])),
    sprintf("Year 3 & %s & 0", fmt(dip_rows[2])),
    sprintf("Year 1 & %s & 0", fmt(dip_rows[3]))),
  "lcc")
cat(sprintf("Dip DiD by base year 5/3/1: %.3f %.3f %.3f (truth 0)\n", dip_rows[1], dip_rows[2], dip_rows[3]))

# --------------------------------------------------------------------------
# Part D. Placebo laws (Bertrand, Duflo and Mullainathan 2004). A state-by-year
# panel of log e-commerce sales with AR(1) errors; half the states "pass a law"
# in a random year between 6 and 15. The law does nothing. How often does a
# 5% test reject?
# --------------------------------------------------------------------------
bdm_once <- function(S, T = 20, rho = 0.8) {
  e <- matrix(0, S, T); e[, 1] <- rnorm(S, 0, 1 / sqrt(1 - rho^2 + (rho == 1)))
  for (t in 2:T) e[, t] <- rho * e[, t - 1] + rnorm(S)
  d <- data.table(state = rep(1:S, T), t = rep(1:T, each = S), e = as.vector(e))
  start <- rep(Inf, S); tr <- sample(S, S / 2); start[tr] <- sample(6:15, length(tr), replace = TRUE)
  d[, y := rnorm(S)[state] + 0.1 * t + e]
  d[, D := as.integer(t >= start[state])]
  f <- feols(y ~ D | state + t, d)
  sapply(list(iid = "iid", hetero = "hetero", cluster = ~state),
         function(v) unname(pvalue(summary(f, vcov = v))["D"]) < 0.05)   # fixest's own p-values
}
bdm_rate <- function(S, rho, R = 1000) rowMeans(replicate(R, bdm_once(S, rho = rho)))
b50_0 <- bdm_rate(50, 0); b50_8 <- bdm_rate(50, 0.8); b10_8 <- bdm_rate(10, 0.8)
pct <- function(x) sprintf("%.0f\\%%", 100 * x)
write_tab("tab_did_bdm.tex", "Standard errors & \\shortstack{50 states\\\\$\\rho = 0$} & \\shortstack{50 states\\\\$\\rho = 0.8$} & \\shortstack{10 states\\\\$\\rho = 0.8$}",
  c(sprintf("Conventional (iid) & %s & %s & %s", pct(b50_0["iid"]), pct(b50_8["iid"]), pct(b10_8["iid"])),
    sprintf("Heteroskedasticity-robust & %s & %s & %s", pct(b50_0["hetero"]), pct(b50_8["hetero"]), pct(b10_8["hetero"])),
    sprintf("Clustered by state & %s & %s & %s", pct(b50_0["cluster"]), pct(b50_8["cluster"]), pct(b10_8["cluster"]))),
  "lccc")
cat("BDM rejection rates (nominal 5%):\n"); print(round(rbind(b50_0, b50_8, b10_8), 3))
