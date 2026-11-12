# Simulations and figures for Session 9 (Lecture 9b: Staggered Adoption and Modern DiD).
# All data are simulated with known ground truth. Everything is fixest + data.table:
# the Goodman-Bacon decomposition, Callaway-Sant'Anna and imputation are coded by hand
# so the mechanics are visible, and checked against two-way FE and sunab().
#
# Run from this directory:  Rscript staggered_sims.R
# Produces fig_stag_*.pdf and tab_stag_*.tex, which the deck \includegraphics / \input.

suppressPackageStartupMessages({
  library(fixest)
  library(data.table)
})
set.seed(20261029)

pdf_open <- function(f, w = 7, h = 4.2) pdf(f, width = w, height = h, pointsize = 11)
col_treat <- "#1f4e79"; col_ctrl <- "#808080"; col_truth <- "#bb0000"; col_alt <- "#e69f00"
col_wave <- c("#1f4e79", "#4f8fc0", "#e69f00", "#8c6d31")

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
# The design. A platform rolls a new recommendation algorithm out city by city:
# 300 cities, 20 months, four waves (months 6, 9, 12, 15) plus never-treated
# cities, 60 each. Outcome: log orders per user. The effect ramps in over eight
# months and is larger for earlier waves (the platform launched first where it
# expected the most). Nothing happens before a city's launch.
# --------------------------------------------------------------------------
T_ <- 20; waves <- c(6, 9, 12, 15)
theta <- c(`1` = 0.15, `6` = 0.12, `9` = 0.09, `12` = 0.06, `15` = 0.04)  # long-run effect by wave
tau_true <- function(g, e) {
  out <- numeric(length(g)); ok <- is.finite(g) & e >= 0
  out[ok] <- theta[as.character(g[ok])] * pmin(e[ok] + 1, 8) / 8
  out
}
sim_rollout <- function(groups = c(waves, Inf), n_per = 60, sd_eps = 0.05) {
  G <- rep(groups, each = n_per); N <- length(G)
  d <- CJ(id = 1:N, t = 1:T_)
  d[, g := G[id]]
  d[, alpha := rnorm(N, 0, 0.3)[id]]
  gam <- cumsum(rnorm(T_, 0.01, 0.02))
  d[, e := t - g]                        # -Inf for never-treated
  d[, D := as.integer(t >= g)]
  d[, tau := tau_true(g, e)]
  d[, y := alpha + gam[t] + tau + rnorm(.N, 0, sd_eps)]
  d[]
}
d <- sim_rollout()
att_true <- d[D == 1, mean(tau)]
truth_e <- d[D == 1, .(tau = mean(tau)), by = e][order(e)]
cat(sprintf("True ATT (mean effect over treated city-months): %.4f\n", att_true))

# ---- Rollout picture ----
grp_lab <- c(paste("wave", waves), "never")
pdf_open("fig_stag_rollout.pdf", w = 7, h = 2.6)
par(mar = c(4, 6, 0.5, 1))
plot(NA, xlim = c(0.5, T_ + 0.5), ylim = c(0.5, 5.5), yaxt = "n", xlab = "month", ylab = "")
axis(2, at = 5:1, labels = grp_lab, las = 1)
for (j in 1:5) {
  g <- c(waves, Inf)[j]
  rect((1:T_) - 0.45, 6 - j - 0.4, (1:T_) + 0.45, 6 - j + 0.4, border = NA,
       col = ifelse((1:T_) >= g, col_treat, "grey88"))
}
dev.off()

# --------------------------------------------------------------------------
# What does static two-way FE estimate? FWL: beta = sum D~ y / sum D~ D, where D~ is
# D residualized on city and month effects. With y = alpha + gamma + tau*D + e, the
# estimand is sum_{D=1} w_it tau_it with w_it = D~_it / sum_{D=1} D~_it.
# --------------------------------------------------------------------------
fit_static <- feols(y ~ D | id + t, d, vcov = ~id)
twfe_weights <- function(dd) {
  dd[, Dt := resid(feols(D ~ 1 | id + t, dd))]
  dd[D == 1, w := Dt / sum(Dt)]
  dd[]
}
d <- twfe_weights(d)
est_noiseless <- coef(feols(tau ~ D | id + t, d))["D"]
stopifnot(abs(d[D == 1, sum(w * tau)] - est_noiseless) < 1e-10)
neg_rows <- d[D == 1, mean(w < 0)]; neg_wt <- d[D == 1, sum(abs(w[w < 0])) / sum(abs(w))]
cat(sprintf("Static TWFE %.4f (SE %.4f) vs true ATT %.4f; estimand %.4f; negative weights on %.1f%% of treated rows, %.1f%% of absolute weight\n",
            coef(fit_static), se(fit_static), att_true, est_noiseless, 100 * neg_rows, 100 * neg_wt))

cw <- d[D == 1, .(w = sum(w)), by = .(g, t)]
pdf_open("fig_stag_weights.pdf")
par(mar = c(4, 4.5, 1, 1))
plot(NA, xlim = c(1, T_), ylim = range(c(0, cw$w)), xlab = "month",
     ylab = "weight TWFE puts on the cell's effect")
abline(h = 0, lty = 3)
for (j in seq_along(waves)) with(cw[g == waves[j]][order(t)], lines(t, w, type = "b", pch = 16, lwd = 2, col = col_wave[j]))
legend("topright", bty = "n", legend = paste("wave", waves), col = col_wave, lwd = 2, pch = 16, ncol = 2)
dev.off()

# --------------------------------------------------------------------------
# Goodman-Bacon (2021) decomposition, by hand. Every 2x2 between a timing group and
# the never-treated, and between each pair of timing groups (earlier vs. later before
# the later group is treated; later vs. earlier after the earlier group is treated).
# Weights from Goodman-Bacon's Theorem 1; check they reproduce two-way FE exactly.
# --------------------------------------------------------------------------
m <- dcast(d[, .(y = mean(y)), by = .(g, t)], g ~ t, value.var = "y")
M <- as.matrix(m[, -1]); rownames(M) <- m$g
mm <- function(g, ts) mean(M[as.character(g), ts])
N <- uniqueN(d$id); n_g <- d[t == 1, .N, by = g][, setNames(N / ..N, g)]
n_g <- d[t == 1, .N, by = g]; n_g <- setNames(n_g$N / N, n_g$g)
Dbar <- function(k) (T_ - k + 1) / T_
VD <- mean(d$Dt^2)
bacon <- list()
for (k in waves) {   # treated vs never-treated
  pre <- 1:(k - 1); post <- k:T_
  est <- (mm(k, post) - mm(k, pre)) - (mm(Inf, post) - mm(Inf, pre))
  nk <- n_g[as.character(k)]; nU <- n_g["Inf"]; nkU <- nk / (nk + nU)
  s <- (nk + nU)^2 * nkU * (1 - nkU) * Dbar(k) * (1 - Dbar(k)) / VD
  bacon[[length(bacon) + 1]] <- data.table(type = "vs never treated", k = k, l = Inf, est = est, s = s)
}
for (k in waves) for (l in waves[waves > k]) {
  nk <- n_g[as.character(k)]; nl <- n_g[as.character(l)]; nkl <- nk / (nk + nl)
  Dk <- Dbar(k); Dl <- Dbar(l)
  pre <- 1:(k - 1); mid <- k:(l - 1); post <- l:T_
  est_k <- (mm(k, mid) - mm(k, pre)) - (mm(l, mid) - mm(l, pre))        # later group not yet treated
  s_k <- ((nk + nl) * (1 - Dl))^2 * nkl * (1 - nkl) * ((Dk - Dl) / (1 - Dl)) * ((1 - Dk) / (1 - Dl)) / VD
  est_l <- (mm(l, post) - mm(l, mid)) - (mm(k, post) - mm(k, mid))     # earlier group already treated
  s_l <- ((nk + nl) * Dk)^2 * nkl * (1 - nkl) * (Dl / Dk) * ((Dk - Dl) / Dk) / VD
  bacon[[length(bacon) + 1]] <- data.table(type = "earlier vs later (not yet treated)", k = k, l = l, est = est_k, s = s_k)
  bacon[[length(bacon) + 1]] <- data.table(type = "later vs earlier (already treated)", k = l, l = k, est = est_l, s = s_l)
}
bacon <- rbindlist(bacon)
cat(sprintf("Bacon: weights sum to %.10f; weighted 2x2s = %.10f; TWFE = %.10f\n",
            sum(bacon$s), sum(bacon$s * bacon$est), coef(fit_static)))
stopifnot(abs(sum(bacon$s) - 1) < 1e-8, abs(sum(bacon$s * bacon$est) - coef(fit_static)) < 1e-8)
bac_type <- bacon[, .(weight = sum(s), avg = sum(s * est) / sum(s)), by = type]
print(bac_type)
short_lab <- c(`vs never treated` = "vs.\\ never treated", `earlier vs later (not yet treated)` = "vs.\\ not yet treated",
               `later vs earlier (already treated)` = "vs.\\ already treated")
write_tab("tab_stag_bacon.tex", "Control group & $N$ & Weight & Avg.",
  sapply(bac_type$type, function(ty) sprintf("%s & %d & %s & %s", short_lab[[ty]], nrow(bacon[type == ty]),
         fmt(bac_type[type == ty]$weight, 2), fmt(bac_type[type == ty]$avg))),
  "lccc")

pdf_open("fig_stag_bacon.pdf")
par(mar = c(4, 4, 1, 1))
pch_t <- c(`vs never treated` = 16, `earlier vs later (not yet treated)` = 17, `later vs earlier (already treated)` = 15)
col_t <- c(`vs never treated` = col_treat, `earlier vs later (not yet treated)` = col_ctrl, `later vs earlier (already treated)` = col_truth)
plot(bacon$s, bacon$est, pch = pch_t[bacon$type], col = col_t[bacon$type], cex = 1.4,
     xlab = "weight in the two-way FE estimate", ylab = "2x2 DiD estimate",
     ylim = range(c(bacon$est, 0, att_true)) + c(-0.01, 0.05))
abline(h = 0, lty = 3)
abline(h = att_true, col = col_truth, lty = 2); abline(h = coef(fit_static), col = col_alt, lty = 4, lwd = 2)
legend("top", bty = "n", cex = 0.8, ncol = 2,
       legend = c(names(pch_t), sprintf("true ATT = %.3f", att_true), sprintf("two-way FE = %.3f", coef(fit_static))),
       pch = c(pch_t, NA, NA), col = c(col_t, col_truth, col_alt), lty = c(NA, NA, NA, 2, 4), lwd = c(NA, NA, NA, 1, 2))
dev.off()

# --------------------------------------------------------------------------
# Dynamic two-way FE event study (never-treated get the reference value) vs truth
# --------------------------------------------------------------------------
d[, rel := ifelse(is.finite(g), t - g, -1000)]
fit_es <- feols(y ~ i(rel, ref = c(-1, -1000)) | id + t, d, vcov = ~id)
es_coefs <- function(fit, pattern = "^rel::") {
  ct <- coeftable(fit); ct <- ct[grepl(pattern, rownames(ct)), , drop = FALSE]
  data.table(e = as.integer(sub(".*::", "", rownames(ct))), b = ct[, 1], s = ct[, 2])
}
es_twfe <- rbind(es_coefs(fit_es), data.table(e = -1L, b = 0, s = 0))[order(e)]
# The common shortcut: bin event time at -6 and +6 (all earlier/later months pooled)
d[, rel_b := ifelse(is.finite(g), pmin(pmax(t - g, -6), 6), -1000)]
fit_es_b <- feols(y ~ i(rel_b, ref = c(-1, -1000)) | id + t, d, vcov = ~id)
es_bin <- rbind(es_coefs(fit_es_b, "^rel_b::"), data.table(e = -1L, b = 0, s = 0))[order(e)]
p_pre_full <- wald(fit_es, keep = "^rel::-([2-6])$", print = FALSE)$p
p_pre_bin <- wald(fit_es_b, keep = "^rel_b::-([2-6])$", print = FALSE)$p
cat(sprintf("Joint test of leads -6..-2: fully saturated p = %.3f ; binned endpoints p = %.2g\n", p_pre_full, p_pre_bin))

# Sun and Abraham via sunab(): never-treated coded as a cohort outside the sample
d[, g_sa := ifelse(is.finite(g), g, 10000)]
fit_sa <- feols(y ~ sunab(g_sa, t) | id + t, d, vcov = ~id)
es_sa <- rbind(es_coefs(fit_sa, "^t::"), data.table(e = -1L, b = 0, s = 0))[order(e)]
sa_att <- coeftable(summary(fit_sa, agg = "ATT"))["ATT", ]

# --------------------------------------------------------------------------
# Callaway and Sant'Anna by hand: ATT(g,t) as 2x2s against a fixed base period g-1
# (the "universal" base, comparable to an event-study regression), with never-treated
# or not-yet-treated controls; then aggregate with cohort-size weights.
# --------------------------------------------------------------------------
wide <- function(dd) {
  w <- dcast(dd, id + g ~ t, value.var = "y")
  list(Y = as.matrix(w[, -(1:2)]), G = w$g)
}
cs_attgt <- function(Y, G, control = "never") {
  res <- vector("list", 0)
  for (g in waves) {
    ig <- which(G == g); b <- g - 1
    for (tt in setdiff(1:T_, b)) {
      ic <- if (control == "never") which(!is.finite(G)) else which(G > max(tt, b) & G != g)
      if (length(ic) == 0) next
      res[[length(res) + 1]] <- list(g = g, t = tt, e = tt - g, n = length(ig),
        att = mean(Y[ig, tt] - Y[ig, b]) - mean(Y[ic, tt] - Y[ic, b]))
    }
  }
  rbindlist(res)
}
agg_dyn <- function(a) a[, .(b = weighted.mean(att, n)), by = e][order(e)]
agg_simple <- function(a) a[e >= 0, weighted.mean(att, n)]
W <- wide(d)
cs_nev <- cs_attgt(W$Y, W$G, "never"); cs_nyt <- cs_attgt(W$Y, W$G, "notyet")
dyn_nev <- agg_dyn(cs_nev)
chk <- merge(dyn_nev, es_sa, by = "e")
cat(sprintf("Max |sunab - hand-coded CS (never-treated)| over event times: %.2e; overall %.6f vs %.6f\n",
            max(abs(chk$b.x - chk$b.y)), agg_simple(cs_nev), sa_att[1]))
stopifnot(max(abs(chk$b.x - chk$b.y)) < 1e-8, abs(agg_simple(cs_nev) - sa_att[1]) < 1e-8)

# --------------------------------------------------------------------------
# Imputation (Borusyak, Jaravel and Spiess 2024): fit city + month effects on the
# untreated observations only, predict Y(0) for treated observations, average.
# --------------------------------------------------------------------------
imputation <- function(dd) {
  f0 <- feols(y ~ 1 | id + t, dd[D == 0])
  tr <- dd[D == 1]
  tr[, tauhat := y - predict(f0, newdata = tr)]
  list(att = tr[, mean(tauhat)], dyn = tr[, .(b = mean(tauhat)), by = e][order(e)])
}
imp <- imputation(d)

# Cluster bootstrap (resample cities) for the hand-coded estimators
B <- 199
boot <- t(replicate(B, {
  ids <- sample(unique(d$id), replace = TRUE)
  db <- d[data.table(id = ids, nid = seq_along(ids)), on = "id", allow.cartesian = TRUE]
  db[, id := nid]
  Wb <- wide(db)
  c(cs_nev = agg_simple(cs_attgt(Wb$Y, Wb$G, "never")),
    cs_nyt = agg_simple(cs_attgt(Wb$Y, Wb$G, "notyet")),
    imp = imputation(db)$att)
}))
bse <- apply(boot, 2, sd)

est_rows <- list(
  c("Static two-way FE", coef(fit_static), se(fit_static)),
  c("Sun--Abraham (\\texttt{sunab}), never-treated controls", sa_att[1], sa_att[2]),
  c("Callaway--Sant'Anna, never-treated controls", agg_simple(cs_nev), bse["cs_nev"]),
  c("Callaway--Sant'Anna, not-yet-treated controls", agg_simple(cs_nyt), bse["cs_nyt"]),
  c("Imputation (Borusyak--Jaravel--Spiess)", imp$att, bse["imp"]))
write_tab("tab_stag_att.tex", "Estimator & Overall ATT & Std.\\ error & Bias",
  c(sapply(est_rows, function(r) sprintf("%s & %s & %s & %s", r[1], fmt(as.numeric(r[2])),
         fmt_se(as.numeric(r[3])), sprintf("%+.3f", as.numeric(r[2]) - att_true))),
    "\\midrule",
    sprintf("Truth: mean effect over treated city-months & %s & & ", fmt(att_true))),
  "lccc")
for (r in est_rows) cat(sprintf("%-55s %.4f (%.4f)\n", r[1], as.numeric(r[2]), as.numeric(r[3])))

# ---- Event-time horse race ----
rng <- -6:10
truth_plot <- c(rep(0, sum(rng < 0)), truth_e[e %in% rng[rng >= 0]]$tau)
rng_b <- -6:6
truth_b <- c(rep(0, sum(rng_b < 0)), truth_e[e %in% rng_b[rng_b >= 0]]$tau)
plot_es_panel <- function(p, truth, rr, main) {
  plot(p$e, p$b, pch = 16, col = col_alt, ylim = c(-0.03, 0.13), main = main, cex.main = 1,
       xlab = "months relative to launch", ylab = "estimate")
  segments(p$e, p$b - 1.96 * p$s, p$e, p$b + 1.96 * p$s, col = col_alt)
  lines(rr, truth, col = col_truth, lwd = 2)
  abline(h = 0, lty = 3); abline(v = -0.5, lty = 2, col = "grey40")
}
pdf_open("fig_stag_es_twfe.pdf", w = 8, h = 3.8)
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
plot_es_panel(es_twfe[e %in% rng], truth_plot, rng, "every lead and lag")
legend("topleft", bty = "n", cex = 0.85, legend = c("two-way FE event study, 95% CI", "truth"),
       col = c(col_alt, col_truth), pch = c(16, NA), lwd = c(1, 2))
plot_es_panel(es_bin[e %in% rng_b], truth_b, rng_b, "endpoints binned at -6 and +6")
dev.off()

pdf_open("fig_stag_horse.pdf")
par(mar = c(4, 4, 1, 1))
series <- list(
  list(es_bin[e %in% rng], col_alt, 16, -0.2, "two-way FE event study (binned)"),
  list(es_sa[e %in% rng], col_treat, 17, 0, "Sun-Abraham = Callaway-Sant'Anna (never-treated)"),
  list(agg_dyn(cs_nyt)[e %in% rng], col_ctrl, 15, 0.2, "Callaway-Sant'Anna (not-yet-treated)"),
  list(imp$dyn[e %in% rng], "#2a9d8f", 18, 0.35, "imputation (post-period)"))
yl <- range(unlist(lapply(series, function(s) s[[1]]$b)), truth_plot)
plot(NA, xlim = range(rng) + c(-0.5, 0.5), ylim = yl + c(-0.01, 0.01), xlab = "months relative to launch", ylab = "estimate")
abline(h = 0, lty = 3); abline(v = -0.5, lty = 2, col = "grey40")
lines(rng, truth_plot, col = col_truth, lwd = 2)
for (s in series) points(s[[1]]$e + s[[4]], s[[1]]$b, col = s[[2]], pch = s[[3]], cex = 1.1)
legend("topleft", bty = "n", cex = 0.85, legend = c("truth", sapply(series, `[[`, 5)),
       col = c(col_truth, sapply(series, `[[`, 2)), pch = c(NA, sapply(series, `[[`, 3)), lwd = c(2, NA, NA, NA, NA))
dev.off()
cat(sprintf("TWFE event study: mean |pre coef|, e in -6..-2: %.4f ; post e=0..10 mean %.4f vs truth %.4f\n",
            es_twfe[e %in% -6:-2, mean(abs(b))], es_twfe[e %in% 0:10, mean(b)], mean(truth_plot[rng >= 0])))

# ---- Cohort ATT(g,t) against the truth ----
pdf_open("fig_stag_attgt.pdf", w = 8, h = 3.6)
par(mfrow = c(1, 4), mar = c(4, 3, 2, 0.5), oma = c(0, 1.5, 0, 0))
for (j in seq_along(waves)) {
  a <- cs_nev[g == waves[j]][order(t)]
  plot(a$t, a$att, pch = 16, col = col_wave[j], ylim = c(-0.03, 0.14), main = paste("wave", waves[j]),
       xlab = "month", ylab = "", cex.main = 1)
  lines(1:T_, tau_true(rep(waves[j], T_), 1:T_ - waves[j]), col = col_truth, lwd = 2)
  abline(h = 0, lty = 3); abline(v = waves[j] - 0.5, lty = 2, col = "grey40")
}
mtext("ATT(g, t), never-treated controls", side = 2, outer = TRUE, line = 0.2, cex = 0.8)
dev.off()

# --------------------------------------------------------------------------
# Variant: add always-treated cities (launched before the sample starts) in place of
# half of the never-treated. TWFE uses them only as controls, and they are moving.
# --------------------------------------------------------------------------
d_aw <- sim_rollout(groups = c(1, waves, Inf), n_per = 50)
d_aw <- twfe_weights(d_aw)
aw_twfe <- coef(feols(y ~ D | id + t, d_aw))
aw_truth <- d_aw[D == 1 & g > 1, mean(tau)]
aw_imp <- imputation(d_aw[g > 1])$att
cat(sprintf("Always-treated variant: TWFE %.4f vs truth (non-always cohorts) %.4f; imputation dropping always-treated %.4f; negative weights on %.1f%% of treated rows\n",
            aw_twfe, aw_truth, aw_imp, 100 * d_aw[D == 1, mean(w < 0)]))
