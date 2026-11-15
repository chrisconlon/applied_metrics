# Simulations for Lecture 11a (Matching): the slides on OLS with heterogeneous effects, overlap and trimming,
# balancing weights, and what happens when conditional independence fails.
# Same paid-membership design as the Session 11 workbook (Fall/L10 - More Quasi Experiments/inclass_session11.Rmd).
#
# Run from this directory:  Rscript matching_sims.R
# Produces fig_match_overlap.pdf and tab_match_{ols,overlap,unobs}.tex. Base R only.

set.seed(20261112)
pdf_open <- function(f, w = 7, h = 4.2) pdf(f, width = w, height = h, pointsize = 11)
col_treat <- "#1f4e79"; col_ctrl <- "#808080"
write_tab <- function(file, header, rows, align) {
  con <- file(file, "w")
  writeLines(sprintf("\\begin{tabular}{%s}\\toprule", align), con)
  writeLines(paste0(header, " \\\\ \\midrule"), con)
  for (r in rows) writeLines(if (r == "\\midrule") r else paste0(r, " \\\\"), con)
  writeLines("\\bottomrule\\end{tabular}", con)
  close(con)
}
fmt <- function(x, d = 1) formatC(round(x, d) + 0, format = "f", digits = d)   # + 0 turns -0 into 0

# --------------------------------------------------------------------------
# Paid membership: customers join based on past spend, tenure and app use.
# Effect on next-quarter spend ($) grows with past spend. gU > 0 adds an unobserved confounder U.
# --------------------------------------------------------------------------
sim_members <- function(N = 4000, sel = 1.2, gU = 0) {
  past <- rnorm(N); tenure <- rexp(N, 1 / 3); app <- rbinom(N, 1, 0.4); U <- rnorm(N)
  D <- rbinom(N, 1, plogis(-1 + sel * past + 0.15 * tenure + 0.8 * app + gU * U))
  tau_i <- 30 + 20 * past
  y0 <- 200 + 60 * past + 15 * past^2 + 5 * tenure + 20 * app + 25 * (gU > 0) * U + rnorm(N, 0, 40)
  data.frame(past, tenure, app, D, tau_i, y = y0 + tau_i * D)
}

# Estimators. Propensity score: logit on the observed covariates. Outcome model: by arm, quadratic in past.
estimate <- function(m, trim = FALSE) {
  m$ps <- fitted(glm(D ~ past + tenure + app, binomial, m))
  if (trim) m <- m[m$ps >= 0.1 & m$ps <= 0.9, ]
  t1 <- m$D == 1
  mu <- function(arm) predict(lm(y ~ past + I(past^2) + tenure + app, m[m$D == arm, ]), m)
  mu1 <- mu(1); mu0 <- mu(0)
  w_att <- ifelse(t1, 1, m$ps / (1 - m$ps)); w_ate <- ifelse(t1, 1 / m$ps, 1 / (1 - m$ps))
  wm <- function(w) weighted.mean(m$y[t1], w[t1]) - weighted.mean(m$y[!t1], w[!t1])
  # Entropy balancing (Hainmueller 2012) for the ATT: control weights exp(lambda'x), chosen so the weighted
  # control means of past, past^2, tenure and app equal the treated means exactly.
  X <- cbind(m$past, m$past^2, m$tenure, m$app); Xc <- sweep(X[!t1, ], 2, colMeans(X[t1, ]))
  lam <- optim(rep(0, 4), function(l) log(sum(exp(Xc %*% l))), method = "BFGS")$par
  w_eb <- as.vector(exp(Xc %*% lam))
  c(ipw_att = wm(w_att), ipw_ate = wm(w_ate),
    ra_att = mean(m$y[t1] - mu0[t1]),
    aipw_ate = mean(mu1 - mu0 + m$D * (m$y - mu1) / m$ps - (1 - m$D) * (m$y - mu0) / (1 - m$ps)),
    eb_att = mean(m$y[t1]) - weighted.mean(m$y[!t1], w_eb),
    att = mean(m$tau_i[t1]), ate = mean(m$tau_i), max_w = max(w_ate), share_kept = nrow(m))
}

# --------------------------------------------------------------------------
# 1. OLS with heterogeneous effects (Angrist 1998; Sloczynski 2022). One very large sample.
# --------------------------------------------------------------------------
big <- sim_members(N = 400000)
t1 <- big$D == 1
ATT <- mean(big$tau_i[t1]); ATU <- mean(big$tau_i[!t1]); ATE <- mean(big$tau_i)
ols1 <- coef(lm(y ~ D + past + tenure + app, big))["D"]
ols2 <- coef(lm(y ~ D + past + I(past^2) + tenure + app, big))["D"]
rho <- mean(big$D)
cat(sprintf("Share joining %.2f ; ATE %.1f ATT %.1f ATU %.1f ; OLS %.1f ; OLS + past^2 %.1f\n", rho, ATE, ATT, ATU, ols1, ols2))
wt1 <- function(b) (b - ATU) / (ATT - ATU)            # implied weight on the ATT
write_tab("tab_match_ols.tex", "Quantity & Value & Implied weight on ATT",
  c(sprintf("ATT (members, %d\\%% of customers) & %s & 1", round(100 * rho), fmt(ATT)),
    sprintf("ATU (non-members) & %s & 0", fmt(ATU)),
    sprintf("ATE & %s & %s", fmt(ATE), fmt(rho, 2)),
    "\\midrule",
    sprintf("OLS: $y$ on $D$ and linear controls & %s & %s", fmt(ols1), fmt(wt1(ols1), 2)),
    sprintf("OLS: adding $\\text{past}^2$ & %s & %s", fmt(ols2), fmt(wt1(ols2), 2))),
  "lcc")

# --------------------------------------------------------------------------
# 2. Overlap: estimated propensity scores under moderate and strong selection
# --------------------------------------------------------------------------
pdf_open("fig_match_overlap.pdf", w = 8, h = 3.4)
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
for (s in c(1.2, 3)) {
  m <- sim_members(sel = s); p <- fitted(glm(D ~ past + tenure + app, binomial, m)); br <- seq(0, 1, by = 0.025)
  h1 <- hist(p[m$D == 1], br, plot = FALSE); h0 <- hist(p[m$D == 0], br, plot = FALSE)
  plot(NA, xlim = c(0, 1), ylim = c(-max(h0$counts), max(h1$counts)), xlab = "estimated propensity score",
       ylab = "customers (members up, non-members down)", main = sprintf("selection on past spend: %.1f", s),
       cex.main = 1, yaxt = "n")
  axis(2, at = pretty(c(-max(h0$counts), max(h1$counts))), labels = abs(pretty(c(-max(h0$counts), max(h1$counts)))))
  rect(br[-length(br)], 0, br[-1], h1$counts, col = col_treat, border = "white")
  rect(br[-length(br)], -h0$counts, br[-1], 0, col = col_ctrl, border = "white")
  abline(h = 0); abline(v = c(0.1, 0.9), lty = 2)
}
invisible(dev.off())

# --------------------------------------------------------------------------
# 3. Monte Carlo: overlap and trimming (200 draws each)
# --------------------------------------------------------------------------
R <- 200
mc <- function(sel, trim = FALSE, gU = 0) t(replicate(R, {
  m <- sim_members(sel = sel, gU = gU)
  if (trim) {   # truth for the trimmed population
    m$ps <- fitted(glm(D ~ past + tenure + app, binomial, m)); keep <- m$ps >= 0.1 & m$ps <= 0.9
    e <- estimate(m, trim = TRUE); e["ate"] <- mean(m$tau_i[keep]); e["share_kept"] <- mean(keep); e
  } else estimate(m) }))
row_b <- function(r, est, truth) c(bias = mean(r[, est] - r[, truth]), sd = sd(r[, est]))
mods <- mc(1.2); strong <- mc(3); strong_tr <- mc(3, trim = TRUE)
ov_row <- function(lab, est, truth) {
  a <- row_b(mods, est, truth); b <- row_b(strong, est, truth)
  sprintf("%s & %s & %s & %s & %s", lab, fmt(a[1]), fmt(a[2]), fmt(b[1]), fmt(b[2])) }
tr <- row_b(strong_tr, "aipw_ate", "ate")
write_tab("tab_match_overlap.tex", "& \\multicolumn{2}{c}{Selection 1.2} & \\multicolumn{2}{c}{Selection 3} \\\\ Estimator (truth) & Bias & Std.\\ dev. & Bias & Std.\\ dev.",
  c(ov_row("IPW (ATE)", "ipw_ate", "ate"), ov_row("AIPW (ATE)", "aipw_ate", "ate"), ov_row("IPW (ATT)", "ipw_att", "att"),
    ov_row("Regression adjustment (ATT)", "ra_att", "att"),
    "\\midrule",
    sprintf("Median of the largest ATE weight & %s & & %s & ", fmt(median(mods[, "max_w"]), 0), fmt(median(strong[, "max_w"]), 0)),
    sprintf("AIPW after trimming to $0.1 \\le \\hat\\pi \\le 0.9$ (vs.\\ trimmed ATE) & & & %s & %s", fmt(tr[1]), fmt(tr[2])),
    sprintf("\\quad share of customers kept; ATE of those kept & & & %s & %s", fmt(mean(strong_tr[, "share_kept"]), 2), fmt(mean(strong_tr[, "ate"])))),
  "lcccc")
cat(sprintf("Strong selection: IPW-ATE sd %.1f vs moderate %.1f ; trimmed keeps %.2f, its ATE %.1f vs full %.1f\n",
            sd(strong[, "ipw_ate"]), sd(mods[, "ipw_ate"]), mean(strong_tr[, "share_kept"]), mean(strong_tr[, "ate"]), mean(strong[, "ate"])))

# --------------------------------------------------------------------------
# 4. Conditional independence fails: an unobserved taste U raises both joining and spending
# --------------------------------------------------------------------------
unobs <- mc(1.2, gU = 0.8)
un_row <- function(lab, est, truth) {
  a <- row_b(mods, est, truth); b <- row_b(unobs, est, truth)
  sprintf("%s & %s & %s", lab, fmt(a[1]), fmt(b[1])) }
write_tab("tab_match_unobs.tex", "Estimator (target) & Bias: CIA holds & Bias: $U$ omitted",
  c(un_row("Regression adjustment (ATT)", "ra_att", "att"), un_row("IPW (ATT)", "ipw_att", "att"),
    un_row("Entropy balancing (ATT)", "eb_att", "att"), un_row("AIPW (ATE)", "aipw_ate", "ate")),
  "lcc")
print(round(rbind(cia = colMeans(mods[, 1:5] - mods[, c("att", "ate", "att", "ate", "att")]),
                  unobs = colMeans(unobs[, 1:5] - unobs[, c("att", "ate", "att", "ate", "att")])), 2))
cat("done.\n")
