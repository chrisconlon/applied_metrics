# Simulations and figures for Session 8 (Lecture 9a: Panel Data and Fixed Effects;
# Lecture 9b: Event Studies). All data are simulated with known ground truth.
#
# Run from this directory:  Rscript panel_sims.R
# Produces fig_*.pdf and tab_*.tex, which the decks \includegraphics / \input.

suppressPackageStartupMessages({
  library(fixest)
  library(data.table)
})
set.seed(20261022)

pdf_open <- function(f, w = 7, h = 4.2) pdf(f, width = w, height = h, pointsize = 11)
col_treat <- "#1f4e79"; col_ctrl <- "#b0b0b0"; col_truth <- "#bb0000"

# helper: write a booktabs tabular fragment
write_tab <- function(file, header, rows, align) {
  con <- file(file, "w")
  writeLines(sprintf("\\begin{tabular}{%s}\\toprule", align), con)
  writeLines(paste0(header, " \\\\ \\midrule"), con)
  for (r in rows) writeLines(paste0(r, " \\\\"), con)
  writeLines("\\bottomrule\\end{tabular}", con)
  close(con)
}
fmt <- function(x, d = 3) formatC(x, format = "f", digits = d)
fmt_se <- function(x, d = 3) paste0("(", fmt(x, d), ")")

# --------------------------------------------------------------------------
# Part A. Management practices panel: firm quality drives both adoption and
# productivity. True effect of practices on log productivity: beta = 0.10.
# --------------------------------------------------------------------------
beta_true <- 0.10
sim_panel <- function(N = 200, T = 8, rho = 0.7, sd_alpha = 0.5, sd_eps = 0.3,
                      sd_x = 1, me_sd = 0) {
  alpha <- rnorm(N, 0, sd_alpha)                 # firm quality (unobserved)
  gamma <- rnorm(T, 0, 0.1)                      # year shocks
  d <- CJ(firm = 1:N, year = 1:T)
  # practices: correlated with quality (better-run firms adopt more), plus
  # firm-specific persistent component and year-to-year variation
  d[, alpha := alpha[firm]]
  d[, x := rho * alpha + sqrt(1 - rho^2) * (rnorm(N)[firm] * 0.6) + rnorm(.N, 0, sd_x * 0.8)]
  d[, y := 1 + beta_true * x + alpha + gamma[year] + rnorm(.N, 0, sd_eps)]
  if (me_sd > 0) d[, x := x + rnorm(.N, 0, me_sd)]  # measurement error in x
  d[]
}
d <- sim_panel()

# Random effects by FGLS (Swamy-Arora), written out so the mechanics are visible.
re_fit <- function(d, yvar = "y", xvar = "x", id = "firm", time = "year") {
  y <- d[[yvar]]; x <- d[[xvar]]; g <- d[[id]]
  T_i <- ave(rep(1, nrow(d)), g, FUN = sum)
  within <- feols(as.formula(sprintf("%s ~ %s | %s", yvar, xvar, id)), d)
  s2_eps <- sum(resid(within)^2) / (nrow(d) - length(unique(g)) - 1)
  ybar <- ave(y, g); xbar <- ave(x, g)
  between <- lm(ybar ~ xbar, subset = !duplicated(g))
  Tbar <- mean(unique(T_i))
  s2_alpha <- max(0, sum(resid(between)^2) / (between$df.residual) - s2_eps / Tbar)
  theta <- 1 - sqrt(s2_eps / (s2_eps + T_i * s2_alpha))
  yq <- y - theta * ybar; xq <- x - theta * xbar
  fit <- lm(yq ~ xq)
  list(coef = unname(coef(fit)[2]), se = unname(sqrt(diag(vcov(fit)))[2]),
       s2_eps = s2_eps, s2_alpha = s2_alpha, theta = mean(theta), fit = fit)
}

fit_pooled <- feols(y ~ x, d, vcov = ~firm)
fit_fe     <- feols(y ~ x | firm + year, d, vcov = ~firm)
fit_fd     <- feols(d(y) ~ d(x), d, panel.id = ~firm + year, vcov = ~firm)
fit_re     <- re_fit(d)
d[, xbar := mean(x), by = firm]
fit_mund   <- feols(y ~ x + xbar | year, d, vcov = ~firm)

# Hausman statistic (classical vcovs, one regressor)
fe_cl <- feols(y ~ x | firm + year, d, vcov = "iid")
H <- (coef(fe_cl)["x"] - fit_re$coef)^2 / (vcov(fe_cl)["x", "x"] - fit_re$se^2)
p_H <- 1 - pchisq(H, 1)
# Mundlak (robust Hausman): t-test on xbar
t_mund <- coeftable(fit_mund)["xbar", "t value"]

cat(sprintf("Pooled %.3f  FE %.3f  FD %.3f  RE %.3f (theta=%.2f)  Mundlak %.3f  Hausman=%.1f (p=%.3g)  t(xbar)=%.1f\n",
            coef(fit_pooled)["x"], coef(fit_fe)["x"], coef(fit_fd)[["d(x)"]], fit_re$coef,
            fit_re$theta, coef(fit_mund)["x"], H, p_H, t_mund))

write_tab("tab_mp_estimators.tex",
  " & Pooled OLS & First diff. & Within (FE) & Random eff. & Mundlak",
  c(sprintf("$\\widehat\\beta$ (truth $= %.2f$) & %s & %s & %s & %s & %s", beta_true,
            fmt(coef(fit_pooled)["x"]), fmt(coef(fit_fd)[["d(x)"]]), fmt(coef(fit_fe)["x"]),
            fmt(fit_re$coef), fmt(coef(fit_mund)["x"])),
    sprintf("SE (cluster firm) & %s & %s & %s & %s & %s",
            fmt_se(se(fit_pooled)["x"]), fmt_se(se(fit_fd)[["d(x)"]]), fmt_se(se(fit_fe)["x"]),
            fmt_se(fit_re$se), fmt_se(se(fit_mund)["x"])),
    sprintf("Coef.\\ on $\\bar x_i$ & & & & & %s %s", fmt(coef(fit_mund)["xbar"]), fmt_se(se(fit_mund)["xbar"])),
    sprintf("Firm FE & no & (differenced) & yes & quasi ($\\hat\\theta=%.2f$) & no", fit_re$theta),
    sprintf("$N \\times T$ & %d & %d & %d & %d & %d", nobs(fit_pooled), nobs(fit_fd), nobs(fit_fe), nobs(fit_pooled), nobs(fit_mund))),
  "lccccc")

write_tab("tab_hausman.tex",
  "Test & Statistic & $p$-value & Verdict",
  c(if (H < 0) sprintf("Hausman $(\\hat\\beta_{FE}-\\hat\\beta_{RE})^2 / (\\hat V_{FE}-\\hat V_{RE})$ & %.1f & --- & undefined: $\\hat V_{FE} < \\hat V_{RE}$", H)
    else sprintf("Hausman $(\\hat\\beta_{FE}-\\hat\\beta_{RE})^2 / (\\hat V_{FE}-\\hat V_{RE})$ & %.1f & %s & %s", H,
            ifelse(p_H < 0.001, "$<0.001$", fmt(p_H)), ifelse(p_H < 0.05, "reject RE", "cannot reject RE")),
    sprintf("Mundlak $t$-test on $\\bar x_i$ (cluster-robust) & %.1f & %s & reject RE", t_mund,
            ifelse(abs(t_mund) > 3.3, "$<0.001$", fmt(2 * pnorm(-abs(t_mund)))))),
  "lccc")

# Fig: pooled vs within, a few firms highlighted
pdf_open("fig_mp_pooled_vs_fe.pdf")
par(mar = c(4, 4, 1, 1))
show <- sample(unique(d$firm), 8)
plot(d$x, d$y, pch = 16, cex = 0.4, col = "grey80",
     xlab = "management practices score (x)", ylab = "log productivity (y)")
abline(coef(fit_pooled), lwd = 3, col = col_truth)
cols <- hcl.colors(8, "Dark 3")
for (j in seq_along(show)) {
  dj <- d[firm == show[j]]
  points(dj$x, dj$y, pch = 16, cex = 0.9, col = cols[j])
  b <- coef(fit_fe)["x"]; a <- mean(dj$y) - b * mean(dj$x)
  segments(min(dj$x), a + b * min(dj$x), max(dj$x), a + b * max(dj$x), col = cols[j], lwd = 2)
}
legend("topleft", bty = "n",
       legend = c(sprintf("pooled OLS slope = %.2f", coef(fit_pooled)["x"]),
                  sprintf("within-firm slope = %.2f (truth %.2f)", coef(fit_fe)["x"], beta_true)),
       col = c(col_truth, "black"), lwd = c(3, 2))
dev.off()

# Fig: within-transformed scatter
pdf_open("fig_mp_within.pdf")
par(mar = c(4, 4, 1, 1))
d[, `:=`(xw = x - mean(x), yw = y - mean(y)), by = firm]
plot(d$xw, d$yw, pch = 16, cex = 0.4, col = "grey60",
     xlab = expression(x[it] - bar(x)[i]), ylab = expression(y[it] - bar(y)[i]))
abline(0, coef(fit_fe)["x"], lwd = 3, col = col_treat)
abline(0, beta_true, lwd = 2, lty = 2, col = col_truth)
legend("topleft", bty = "n", legend = c(sprintf("within slope = %.3f", coef(fit_fe)["x"]), "truth = 0.10"),
       col = c(col_treat, col_truth), lwd = c(3, 2), lty = c(1, 2))
dev.off()

# Monte Carlo: sampling distributions of the estimators
R <- 500
mc <- t(replicate(R, {
  dd <- sim_panel()
  c(pooled = unname(coef(feols(y ~ x, dd))["x"]),
    fe = unname(coef(feols(y ~ x | firm + year, dd))["x"]),
    re = re_fit(dd)$coef)
}))
pdf_open("fig_mp_montecarlo.pdf")
par(mar = c(4, 4, 1, 1))
dens <- lapply(colnames(mc), function(k) density(mc[, k]))
plot(NA, xlim = range(mc), ylim = c(0, max(sapply(dens, function(z) max(z$y)))),
     xlab = expression(hat(beta)), ylab = "density (500 simulated panels)")
cl <- c(pooled = col_truth, fe = col_treat, re = "#e69f00")
for (k in seq_along(dens)) lines(dens[[k]], lwd = 2, col = cl[k])
abline(v = beta_true, lty = 2)
legend("topright", bty = "n", legend = c("pooled OLS", "within (FE)", "random effects", "truth"),
       col = c(cl, "black"), lwd = c(2, 2, 2, 1), lty = c(1, 1, 1, 2))
dev.off()

# RE bias as corr(alpha, x) varies; FE unaffected
rhos <- seq(0, 0.9, by = 0.1)
bias <- t(sapply(rhos, function(r) {
  est <- replicate(100, { dd <- sim_panel(rho = r)
    c(fe = unname(coef(feols(y ~ x | firm + year, dd))["x"]), re = re_fit(dd)$coef,
      pooled = unname(coef(feols(y ~ x, dd))["x"])) })
  rowMeans(est) - beta_true
}))
pdf_open("fig_re_bias_rho.pdf")
par(mar = c(4, 4, 1, 1))
matplot(rhos, bias, type = "b", pch = 16, lwd = 2, lty = 1, col = c(col_treat, "#e69f00", col_truth),
        xlab = expression(paste("strength of ", Cov(alpha[i], x[it]), " (design parameter ", rho, ")")),
        ylab = expression(paste("mean bias of ", hat(beta))))
abline(h = 0, lty = 2)
legend("topleft", bty = "n", legend = c("within (FE)", "random effects", "pooled OLS"),
       col = c(col_treat, "#e69f00", col_truth), lwd = 2, pch = 16)
dev.off()

# theta (quasi-demeaning weight) vs T for several variance ratios
pdf_open("fig_re_theta.pdf", w = 6, h = 4)
par(mar = c(4, 4, 1, 1))
Ts <- 1:30; ratios <- c(0.25, 1, 4)
plot(NA, xlim = c(1, 30), ylim = c(0, 1), xlab = "periods per unit, T",
     ylab = expression(paste("RE quasi-demeaning weight  ", theta)))
for (j in seq_along(ratios)) lines(Ts, 1 - sqrt(1 / (1 + Ts * ratios[j])), lwd = 2, col = hcl.colors(3, "Dark 3")[j])
abline(h = 1, lty = 2); text(28, 0.96, "FE (within)", pos = 1, cex = 0.8)
abline(h = 0, lty = 2); text(28, 0.04, "pooled OLS", pos = 3, cex = 0.8)
legend("right", bty = "n", title = expression(sigma[alpha]^2 / sigma[epsilon]^2),
       legend = ratios, col = hcl.colors(3, "Dark 3"), lwd = 2)
dev.off()

# Measurement error: FE attenuates more than pooled (within variation is noisier)
me_grid <- c(0, 0.25, 0.5, 0.75, 1)
att <- t(sapply(me_grid, function(m) {
  est <- replicate(100, { dd <- sim_panel(rho = 0, me_sd = m)
    c(pooled = unname(coef(feols(y ~ x, dd))["x"]), fe = unname(coef(feols(y ~ x | firm + year, dd))["x"])) })
  rowMeans(est)
}))
write_tab("tab_measurement_error.tex",
  "SD of measurement error in $x$ & 0 & 0.25 & 0.5 & 0.75 & 1.0",
  c(paste0("Pooled OLS ($\\rho=0$, so unbiased without error) & ", paste(fmt(att[, "pooled"], 3), collapse = " & ")),
    paste0("Within (FE) & ", paste(fmt(att[, "fe"], 3), collapse = " & "))),
  "lccccc")

# --------------------------------------------------------------------------
# Part B. Event study, single treatment date, never-treated controls.
# Firms adopt an algorithmic-pricing tool in quarter 13; effect ramps in.
# --------------------------------------------------------------------------
N_es <- 400; T_es <- 24; E0 <- 13
tau_true <- function(k) ifelse(k >= 0, 0.05 * pmin(1, (k + 1) / 4), 0)
sim_es <- function(N = N_es, T = T_es, E0 = 13, trend = 0, anticipate = 0, sd_eps = 0.08) {
  d <- CJ(firm = 1:N, t = 1:T)
  d[, treated := as.integer(firm <= N / 2)]
  d[, alpha := rnorm(N, 0, 0.3)[firm]]
  gam <- cumsum(rnorm(T, 0.005, 0.02))  # common macro path
  d[, k := ifelse(treated == 1, t - E0, NA_integer_)]
  d[, tau := 0]
  d[treated == 1, tau := tau_true(k)]
  if (anticipate > 0) d[treated == 1 & k < 0 & k >= -anticipate, tau := 0.0125]
  d[, y := alpha + gam[t] + tau + trend * treated * (t - E0) + rnorm(.N, 0, sd_eps)]
  d[, k_reg := ifelse(treated == 1, k, -1)]  # controls get the reference value
  d[]
}
es <- sim_es()
fit_es <- feols(y ~ i(k_reg, ref = -1) | firm + t, es, vcov = ~firm)
fit_static <- feols(y ~ post_treat | firm + t, es[, post_treat := treated * (t >= E0)], vcov = ~firm)
ct <- coeftable(fit_es)
ks <- as.integer(sub("k_reg::", "", rownames(ct)))
cat(sprintf("Static TWFE post coefficient: %.4f ; mean of true tau_k, k>=0: %.4f ; mean of estimated: %.4f\n",
            coef(fit_static)[1], mean(tau_true(0:(T_es - E0))), mean(ct[ks >= 0, 1])))

plot_es <- function(fit, truth = NULL, ylim = NULL, main = "", show_static = NULL) {
  ct <- coeftable(fit); ks <- as.integer(sub("k_reg::", "", rownames(ct)))
  o <- order(ks); ks <- ks[o]; b <- ct[o, 1]; s <- ct[o, 2]
  ks <- c(ks[ks < -1], -1, ks[ks > -1]); b <- append(b, 0, after = sum(ks < -1)); s <- append(s, 0, after = sum(ks < -1))
  if (is.null(ylim)) ylim <- range(c(b - 2 * s, b + 2 * s, truth), na.rm = TRUE)
  par(mar = c(4, 4, if (nchar(main)) 2.5 else 1, 1))
  plot(ks, b, pch = 16, col = col_treat, ylim = ylim, xlab = "quarters relative to adoption (event time k)",
       ylab = expression(hat(tau)[k]), main = main)
  segments(ks, b - 1.96 * s, ks, b + 1.96 * s, col = col_treat)
  abline(h = 0, lty = 3); abline(v = -0.5, lty = 2, col = "grey40")
  if (!is.null(truth)) lines(ks, truth, col = col_truth, lwd = 2)
  if (!is.null(show_static)) abline(h = show_static, col = "#e69f00", lwd = 2, lty = 4)
}
pdf_open("fig_es_single.pdf")
plot_es(fit_es, truth = tau_true(sort(unique(es$k))), show_static = coef(fit_static)[1])
legend("topleft", bty = "n", legend = c("estimate, 95% CI (cluster firm)", "truth", "static TWFE coefficient"),
       col = c(col_treat, col_truth, "#e69f00"), pch = c(16, NA, NA), lwd = c(1, 2, 2), lty = c(1, 1, 4))
dev.off()

# Pre-trend violation: differential linear trend small enough that pre coefficients
# are individually insignificant, yet post estimates are biased.
es_tr <- sim_es(trend = 0.002)
fit_tr <- feols(y ~ i(k_reg, ref = -1) | firm + t, es_tr, vcov = ~firm)
pre_names <- grep("k_reg::-", names(coef(fit_tr)), value = TRUE)
pre_names <- setdiff(pre_names, "k_reg::-1")
w_pre <- wald(fit_tr, keep = "k_reg::-", print = FALSE)
cat(sprintf("Pre-trend design: joint Wald p = %.3f; individually significant pre coefs: %d of %d\n",
            w_pre$p, sum(abs(coeftable(fit_tr)[pre_names, 3]) > 1.96), length(pre_names)))
pdf_open("fig_es_pretrend.pdf")
plot_es(fit_tr, truth = tau_true(sort(unique(es_tr$k))))
lines(sort(unique(es_tr$k)), tau_true(sort(unique(es_tr$k))) + 0.002 * (sort(unique(es_tr$k)) + 1),
      col = "grey30", lwd = 2, lty = 5)
legend("topleft", bty = "n",
       legend = c("estimate, 95% CI", "true treatment effect", "what the regression is fitting (effect + trend)"),
       col = c(col_treat, col_truth, "grey30"), pch = c(16, NA, NA), lwd = c(1, 2, 2), lty = c(1, 1, 5))
dev.off()

# Power of the joint pre-test against a linear differential trend
slopes <- seq(0, 0.006, by = 0.0005)
power <- sapply(slopes, function(s) mean(replicate(150, {
  dd <- sim_es(trend = s); f <- feols(y ~ i(k_reg, ref = -1) | firm + t, dd, vcov = ~firm)
  wald(f, keep = "k_reg::-", print = FALSE)$p < 0.05 })))
bias_at_k8 <- slopes * 9   # bias of tau_8 (relative to k=-1) implied by the trend
pdf_open("fig_es_pretest_power.pdf", w = 7, h = 4)
par(mar = c(4, 4, 1, 4.2))
plot(slopes, power, type = "b", pch = 16, lwd = 2, col = col_treat, ylim = c(0, 1),
     xlab = "differential trend per quarter (slope)", ylab = "power of joint pre-trend test (5% level)")
abline(h = 0.05, lty = 3)
par(new = TRUE)
plot(slopes, bias_at_k8, type = "l", lwd = 2, col = col_truth, lty = 2, axes = FALSE, xlab = "", ylab = "")
axis(4, col = col_truth, col.axis = col_truth); mtext(expression(paste("bias in ", hat(tau)[8])), side = 4, line = 2.6, col = col_truth)
legend("topleft", bty = "n", legend = c("power of pre-test", "bias in post estimate at k = 8 (right axis)"),
       col = c(col_treat, col_truth), lwd = 2, lty = c(1, 2), pch = c(16, NA))
dev.off()

# Anticipation: firms start changing behavior two quarters before adoption
es_ant <- sim_es(anticipate = 2)
fit_ant <- feols(y ~ i(k_reg, ref = -1) | firm + t, es_ant, vcov = ~firm)
pdf_open("fig_es_anticipation.pdf")
plot_es(fit_ant, truth = es_ant[treated == 1, .(tau = tau[1]), by = k][order(k)]$tau)
legend("topleft", bty = "n", legend = c("estimate, 95% CI", "truth (with anticipation at k = -2, -1)"),
       col = c(col_treat, col_truth), pch = c(16, NA), lwd = c(1, 2))
dev.off()

# All-treated design (no never-treated group): event-time dummies collinear with FE
es_all <- sim_es()[treated == 1]
msg <- tryCatch({ feols(y ~ i(k_reg, ref = -1) | firm + t, es_all); "no error" },
                error = function(e) conditionMessage(e))
cat("All-treated, common date, unit + time FE:\n", msg, "\n")

# --------------------------------------------------------------------------
# Part C. Earnings-announcement event study (CAR). Market model estimated on
# days -250..-11, event window -10..+10. Surprise S_i moves the day-0 return
# (0.02 * S) and a post-announcement drift (0.001 * S per day for 10 days).
# --------------------------------------------------------------------------
N_c <- 200; days <- -250:10
S <- rnorm(N_c)                                    # standardized earnings surprise
beta_i <- rnorm(N_c, 1, 0.3); a_i <- rnorm(N_c, 0, 2e-4)
# Each firm's announcement falls on a different calendar date, so market returns
# are drawn independently per firm. (The workbook puts every event on the same day.)
car <- CJ(firm = 1:N_c, day = days)
car[, `:=`(S = S[firm], beta = beta_i[firm], a = a_i[firm], Rm = rnorm(.N, 4e-4, 0.010))]
car[, ar_true := 0]
car[day == 0, ar_true := 0.02 * S]
car[day >= 1 & day <= 10, ar_true := 0.001 * S]
car[, R := a + beta * Rm + ar_true + rnorm(.N, 0, 0.02)]

# market model on the estimation window, then abnormal returns on the event window
est <- car[day <= -11, .(a_hat = coef(lm(R ~ Rm))[1], b_hat = coef(lm(R ~ Rm))[2],
                         s_hat = summary(lm(R ~ Rm))$sigma), by = firm]
car <- merge(car, est, by = "firm")
car[, AR := R - a_hat - b_hat * Rm]
ev <- car[day >= -10 & day <= 10]
ev[, CAR := cumsum(AR), by = firm]
ev[, tercile := cut(S, quantile(S, c(0, 1/3, 2/3, 1)), include.lowest = TRUE, labels = c("negative", "middle", "positive"))]

# one firm
best <- which.max(S); f1 <- car[firm == best]
pdf_open("fig_car_onefirm.pdf")
par(mar = c(4, 4, 1, 1))
sub <- f1[day >= -60]
plot(sub$day, 100 * sub$R, type = "h", col = "grey60", xlab = "trading days relative to announcement",
     ylab = "daily return (%)")
rect(-10.5, -100, 10.5, 100, col = adjustcolor(col_treat, 0.08), border = NA)
points(sub$day, 100 * (sub$a_hat + sub$b_hat * sub$Rm), pch = 16, cex = 0.5, col = col_treat)
abline(v = 0, lty = 2)
legend("topleft", bty = "n", legend = c("actual return", "market-model prediction", "event window"),
       col = c("grey60", col_treat, adjustcolor(col_treat, 0.3)), lwd = c(2, NA, 8), pch = c(NA, 16, NA))
dev.off()

# average abnormal return and CAAR by surprise tercile
agg <- ev[, .(AAR = mean(AR), se = sd(AR) / sqrt(.N), CAAR = mean(CAR), se_c = sd(CAR) / sqrt(.N)), by = .(tercile, day)]
pdf_open("fig_car_aar.pdf", w = 9, h = 4)
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
cl3 <- c(negative = col_truth, middle = "grey50", positive = col_treat)
plot(NA, xlim = c(-10, 10), ylim = 100 * range(agg$AAR + 2 * agg$se, agg$AAR - 2 * agg$se),
     xlab = "event day", ylab = "average abnormal return (%)", main = "Average AR by surprise tercile")
abline(h = 0, lty = 3); abline(v = 0, lty = 2)
for (g in names(cl3)) { z <- agg[tercile == g][order(day)]
  lines(z$day, 100 * z$AAR, col = cl3[g], lwd = 2); points(z$day, 100 * z$AAR, pch = 16, col = cl3[g], cex = 0.7) }
legend("topleft", bty = "n", legend = paste(names(cl3), "surprise"), col = cl3, lwd = 2)
plot(NA, xlim = c(-10, 10), ylim = 100 * range(agg$CAAR + 2 * agg$se_c, agg$CAAR - 2 * agg$se_c),
     xlab = "event day", ylab = "cumulative average AR (%)", main = "CAAR with 95% bands")
abline(h = 0, lty = 3); abline(v = 0, lty = 2)
for (g in names(cl3)) { z <- agg[tercile == g][order(day)]
  polygon(c(z$day, rev(z$day)), 100 * c(z$CAAR - 1.96 * z$se_c, rev(z$CAAR + 1.96 * z$se_c)),
          col = adjustcolor(cl3[g], 0.15), border = NA)
  lines(z$day, 100 * z$CAAR, col = cl3[g], lwd = 2) }
dev.off()

# CAR windows and tests
win <- function(k1, k2) ev[day >= k1 & day <= k2, .(CAR = sum(AR), s_hat = s_hat[1], S = S[1]), by = firm]
rows <- lapply(list(c(-10, -2), c(-1, 1), c(0, 0), c(1, 10), c(0, 10)), function(w) {
  z <- win(w[1], w[2]); L <- w[2] - w[1] + 1
  t_cs <- mean(z$CAR) / (sd(z$CAR) / sqrt(nrow(z)))            # cross-sectional t
  t_std <- mean(z$CAR / (z$s_hat * sqrt(L))) * sqrt(nrow(z))    # standardized (Patell-style)
  pos <- z[S > 0]; neg <- z[S < 0]
  sprintf("$[%d, %d]$ & %s & %s & %s & %s & %s", w[1], w[2], fmt(100 * mean(z$CAR), 2), fmt(t_cs, 2), fmt(t_std, 2),
          fmt(100 * mean(pos$CAR), 2), fmt(100 * mean(neg$CAR), 2))
})
write_tab("tab_car_windows.tex",
  "Window & CAAR (\\%) & $t$ (cross-section) & $t$ (standardized) & CAAR, $S>0$ & CAAR, $S<0$",
  rows, "lccccc")

z <- win(0, 10)
erc <- lm(CAR ~ S, z); erc_ct <- coeftable(feols(CAR ~ S, z, vcov = "hetero"))
cat(sprintf("ERC regression: slope %.4f (truth 0.030), robust SE %.4f\n", erc_ct["S", 1], erc_ct["S", 2]))
write_tab("tab_car_erc.tex",
  " & $\\widehat{\\text{CAR}}_i(0,10)$ on $S_i$",
  c(sprintf("Slope on surprise $S_i$ (truth $= 0.030$) & %s %s", fmt(erc_ct["S", 1], 4), fmt_se(erc_ct["S", 2], 4)),
    sprintf("Intercept & %s %s", fmt(erc_ct["(Intercept)", 1], 4), fmt_se(erc_ct["(Intercept)", 2], 4)),
    sprintf("$N$ firms, $R^2$ & %d, %s", nrow(z), fmt(summary(erc)$r.squared, 2))),
  "lc")

pdf_open("fig_car_erc.pdf", w = 6, h = 4)
par(mar = c(4, 4, 1, 1))
plot(z$S, 100 * z$CAR, pch = 16, cex = 0.6, col = "grey50", xlab = "earnings surprise (standardized)",
     ylab = "CAR[0, +10] (%)")
abline(100 * coef(erc), lwd = 3, col = col_treat); abline(0, 3, lwd = 2, lty = 2, col = col_truth)
legend("topleft", bty = "n", legend = c("OLS fit", "truth: 3% per SD of surprise"),
       col = c(col_treat, col_truth), lwd = c(3, 2), lty = c(1, 2))
dev.off()

cat("done.\n")
