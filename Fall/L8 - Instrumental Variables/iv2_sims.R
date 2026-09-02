# Simulations, figures, and tables for Session 7 (L8b - Instrumental Variables II:
# weak instruments, LATE, judge designs, shift-share). Dependencies: fixest only.
# All data simulated with known ground truth.  Run:  Rscript iv2_sims.R
suppressPackageStartupMessages(library(fixest)); set.seed(20261015)
pdf_open <- function(f, w = 7, h = 4.2) pdf(f, width = w, height = h, pointsize = 11)
col1 <- "#1f4e79"; col2 <- "#bb0000"; col3 <- "#e69f00"
write_tab <- function(file, header, rows, align) {
  con <- file(file, "w"); writeLines(sprintf("\\begin{tabular}{%s}\\toprule", align), con)
  writeLines(paste0(header, " \\\\ \\midrule"), con); for (r in rows) writeLines(paste0(r, " \\\\"), con)
  writeLines("\\bottomrule\\end{tabular}", con); close(con) }
fmt <- function(x, d = 2) formatC(x, format = "f", digits = d); pse <- function(x, d = 2) paste0("(", fmt(x, d), ")")

# ==========================================================================
# Part A. LATE: an encouragement design. A retailer emails a random half of its
# customers to nudge them onto the loyalty app. Three types of customer.
# ==========================================================================
N <- 10000
type <- sample(c("always", "complier", "never"), N, replace = TRUE, prob = c(0.2, 0.4, 0.4))
Z <- rbinom(N, 1, 0.5)
D <- as.integer(type == "always" | (type == "complier" & Z == 1))
tau <- c(always = 2.0, complier = 0.8, never = 0.5)[type]          # effect of the app on spending, by type
base <- c(always = 3, complier = 1, never = 0.5)[type]              # heavy users adopt anyway and spend more anyway
tenure <- c(always = 4, complier = 2, never = 1)[type] + rnorm(N)   # an observed covariate that differs by type
y <- base + tau * D + rnorm(N)
d <- data.frame(y, D, Z, tenure, type, tau)
ols <- feols(y ~ D, d); rf <- feols(y ~ Z, d); fs <- feols(D ~ Z, d); iv <- feols(y ~ 1 | D ~ Z, d)
ate <- mean(tau); att <- mean(tau[D == 1]); late <- mean(tau[type == "complier"])
# Abadie's complier characterization: E[X | complier] = (E[X D | Z=1] - E[X D | Z=0]) / (E[D|Z=1] - E[D|Z=0])
kap_num <- mean(d$tenure[Z == 1] * D[Z == 1]) - mean(d$tenure[Z == 0] * D[Z == 0]); kap_den <- mean(D[Z == 1]) - mean(D[Z == 0])
comp_tenure_hat <- kap_num / kap_den; comp_tenure_true <- mean(d$tenure[type == "complier"])
cat(sprintf("A: OLS %.3f | ITT %.3f | FS %.3f | Wald/2SLS %.3f (se %.3f) | LATE %.2f ATE %.2f ATT %.2f | complier tenure: kappa %.2f truth %.2f (always %.2f, never %.2f)\n",
            coef(ols)[2], coef(rf)[2], coef(fs)[2], coef(iv)[2], se(iv)[2], late, ate, att, comp_tenure_hat, comp_tenure_true,
            mean(d$tenure[type == "always"]), mean(d$tenure[type == "never"])))
write_tab("tab_late.tex",
  "Quantity & Estimate & What it is",
  c(sprintf("OLS, $y$ on $D$ & %s %s & effect on the treated $+$ selection (always-takers spend more anyway)", fmt(coef(ols)[2]), pse(se(ols)[2])),
    sprintf("Reduced form, $y$ on $Z$ & %s %s & intent-to-treat: effect of the \\emph{email}", fmt(coef(rf)[2]), pse(se(rf)[2])),
    sprintf("First stage, $D$ on $Z$ & %s %s & share of compliers (truth $0.40$)", fmt(coef(fs)[2]), pse(se(fs)[2])),
    sprintf("2SLS $=$ Wald ratio & %s %s & LATE: effect for compliers (truth $%.2f$)", fmt(coef(iv)[2]), pse(se(iv)[2]), late),
    sprintf("ATE (truth) & %s & average over everyone, including never-takers", fmt(ate)),
    sprintf("ATT (truth) & %s & average over app users: always-takers and encouraged compliers", fmt(att)),
    sprintf("Complier mean tenure & %s & Abadie's $\\kappa$ estimate (truth %s; always-takers %s, never-takers %s)", fmt(comp_tenure_hat), fmt(comp_tenure_true), fmt(4), fmt(1))),
  "lcl")

# ==========================================================================
# Part B. Judge design: refund requests randomly routed to customer-service agents
# with different generosity. Outcome: spending over the next year.
# ==========================================================================
Nr <- 20000; J <- 50
q <- rnorm(Nr)                                   # merit of the claim, seen by the agent, not by us
agent <- sample(1:J, Nr, replace = TRUE); gen <- rnorm(J, -0.5, 0.5)   # agent generosity (cutoff = -gen)
R <- as.integer(q + gen[agent] + rnorm(Nr, 0, 0.5) > 0)                # refund granted
tau_r <- 20 - 15 * q                              # goodwill effect largest for weak claims
spend <- 100 + tau_r * R + 30 * q + rnorm(Nr, 0, 20)
dr <- data.frame(spend, R, agent, q, tau_r)
tot <- tapply(R, agent, sum); n_j <- tapply(R, agent, length); dr$len <- (tot[agent] - R) / (n_j[agent] - 1)
ols_r <- feols(spend ~ R, dr, cluster = ~agent); fs_r <- feols(R ~ len, dr, cluster = ~agent); iv_r <- feols(spend ~ 1 | R ~ len, dr, cluster = ~agent)
band <- dr$q + max(gen) + 1 > 0 & dr$q + min(gen) - 1 < 0   # rough complier band (q within reach of some agent's cutoff)
cat(sprintf("B: refund rate %.2f | OLS %.1f | FS %.2f (F=%.0f) | 2SLS %.1f (se %.1f) | ATE %.1f | E[tau | q in [%.1f,%.1f]] %.1f\n",
            mean(R), coef(ols_r)[2], coef(fs_r)[2], fitstat(iv_r, "ivf")[[1]]$stat, coef(iv_r)[2], se(iv_r)[2], mean(tau_r), -max(gen), -min(gen),
            mean(tau_r[dr$q > -max(gen) & dr$q < -min(gen)])))
# monotonicity check (Frandsen-Lefgren-Leslie flavor): first stage should be positive in every subgroup of the claim
dr$size <- cut(rnorm(Nr) + 0.3 * q, breaks = c(-Inf, -0.5, 0.5, Inf), labels = c("small", "medium", "large"))
fs_sub <- sapply(levels(dr$size), function(s) coef(feols(R ~ len, dr[dr$size == s, ]))[2])
cat(sprintf("   first stage by claim size: %s\n", paste(sprintf("%s %.2f", names(fs_sub), fs_sub), collapse = ", ")))
write_tab("tab_judge.tex",
  " & OLS, spend on refund & First stage, refund on leniency & 2SLS",
  c(sprintf("Coefficient & %s & %s & %s", fmt(coef(ols_r)[2], 1), fmt(coef(fs_r)[2]), fmt(coef(iv_r)[2], 1)),
    sprintf(" & %s & %s & %s", pse(se(ols_r)[2], 1), pse(se(fs_r)[2], 3), pse(se(iv_r)[2], 1)),
    sprintf("Truth & ATE $= %s$; effect on refunded $= %s$ & --- & LATE (marginal claims) $\\approx %s$", fmt(mean(tau_r), 1), fmt(mean(tau_r[R == 1]), 1),
            fmt(mean(tau_r[dr$q > -max(gen) & dr$q < -min(gen)]), 1)),
    sprintf("First-stage $F$ & & %s & ", fmt(fitstat(iv_r, "ivf")[[1]]$stat, 0)),
    sprintf("First stage by claim size & \\multicolumn{3}{c}{small %s, medium %s, large %s (all positive: no sign of defiers)}", fmt(fs_sub[1]), fmt(fs_sub[2]), fmt(fs_sub[3]))),
  "lccc")
# the canonical judge-IV picture: binned first stage
pdf_open("fig_judge_fs.pdf", w = 6.5, h = 4)
par(mar = c(4, 4, 1, 1))
bins <- cut(dr$len, quantile(dr$len, seq(0, 1, 0.05)), include.lowest = TRUE)
plot(tapply(dr$len, bins, mean), tapply(dr$R, bins, mean), pch = 16, col = col1, xlab = "agent's leave-one-out refund rate (leniency)",
     ylab = "P(refund granted)")
abline(coef(fs_r), lwd = 2, col = col2)
text(min(dr$len), max(tapply(dr$R, bins, mean)), sprintf("slope = %.2f", coef(fs_r)[2]), pos = 4, col = col2)
dev.off()

# ==========================================================================
# Part C. Shift-share: local credit supply from banks' sector exposure.
# Markets differ in which sectors their banks lend to; national sector credit
# shocks g_k hit markets in proportion. Effect of local credit growth on firm entry: 0.4.
# ==========================================================================
M <- 1000; K <- 6
dom <- sample(1:K, M, replace = TRUE); sh <- matrix(0.4 / (K - 1), M, K); sh[cbind(1:M, dom)] <- 0.6
g <- rnorm(K); eta <- rnorm(M)
B <- as.vector(sh %*% g)
x <- B + 0.5 * eta + rnorm(M, 0, 0.3); y <- 0.4 * x + eta + rnorm(M, 0, 0.3)
dm <- data.frame(y, x, B, eta)
ols_m <- feols(y ~ x, dm); iv_m <- feols(y ~ 1 | x ~ B, dm, vcov = "hetero")
# Rotemberg weights (GPSS): alpha_k = g_k * (s_k' x~) / sum_k g_k (s_k' x~), with x~ demeaned
xt <- x - mean(x); num <- sapply(1:K, function(k) g[k] * sum(sh[, k] * xt)); alpha <- num / sum(num)
beta_k <- sapply(1:K, function(k) coef(feols(y ~ 1 | x ~ z, data.frame(y, x, z = sh[, k] * g[k])))[2])   # just-identified per-sector estimates
cat(sprintf("C: OLS %.3f | 2SLS %.3f (se %.3f, F=%.0f) | sum(alpha_k beta_k) = %.3f\n", coef(ols_m)[2], coef(iv_m)[2], se(iv_m)[2], fitstat(iv_m, "ivf")[[1]]$stat, sum(alpha * beta_k)))
o <- order(-abs(alpha))
write_tab("tab_rotemberg.tex",
  "Sector & shock $g_k$ & Rotemberg weight $\\alpha_k$ & just-identified $\\hat\\beta_k$",
  c(sapply(o, function(k) sprintf("%d & %s & %s & %s", k, fmt(g[k]), fmt(alpha[k]), fmt(beta_k[k]))),
    sprintf("\\midrule 2SLS with $B_m$ & & $\\sum_k \\alpha_k = 1$ & %s $= \\sum_k \\alpha_k \\hat\\beta_k$", fmt(coef(iv_m)[2]))),
  "cccc")
# inference: cluster-by-market vs shock-level variation. Simulate size of the market-robust t-test under H0 (beta=0).
sz <- replicate(500, { g <- rnorm(K); B <- as.vector(sh %*% g); eta <- rnorm(M); x <- B + 0.5 * eta + rnorm(M, 0, 0.4); y <- 0 * x + eta + rnorm(M, 0, 0.4)
  # a sector-level common shock in y, correlated across markets sharing a dominant sector (the AKM problem)
  y <- y + 0.5 * rnorm(K)[dom]
  f <- feols(y ~ 1 | x ~ B, data.frame(y, x, B), vcov = "hetero"); abs(coeftable(f)[2, 3]) > 1.96 })
cat(sprintf("   size of nominal 5%% robust t-test under H0 with sector-level shocks in y: %.2f\n", mean(sz)))
write_tab("tab_ss_size.tex",
  "Design & Nominal size & Actual rejection rate under $H_0$",
  c(sprintf("Market-level robust SE, sector shocks in the outcome & 0.05 & %s", fmt(mean(sz)))),
  "lcc")

# ==========================================================================
# Part D. Weak instruments after 2020: size of the 2SLS t-test at F = 10 vs F = 104,
# and the Anderson-Rubin test, under H0: beta = 0, with endogeneity.
# ==========================================================================
size_at <- function(pi, n = 1000, R = 2000) {
  out <- replicate(R, { z <- rnorm(n); u <- rnorm(n); v <- 0.95 * u + sqrt(1 - 0.95^2) * rnorm(n)
    x <- pi * z + v; y <- 0 * x + u; f <- feols(y ~ 1 | x ~ z, data.frame(y, x, z))
    tt <- abs(coeftable(f)[2, 3]) > 1.96; Fst <- fitstat(f, "ivf")[[1]]$stat
    ar <- abs(coeftable(feols(y ~ z, data.frame(y, z)))[2, 3]) > 1.96           # AR test of beta = 0: regress y - 0*x on z
    c(tt, Fst, ar) })
  c(t_size = mean(out[1, ]), meanF = mean(out[2, ]), ar_size = mean(out[3, ]))
}
w10 <- size_at(sqrt(10 / 1000)); w104 <- size_at(sqrt(104 / 1000))
cat(sprintf("D: F~%.0f: t-test size %.3f, AR size %.3f | F~%.0f: t-test size %.3f, AR size %.3f\n", w10[2], w10[1], w10[3], w104[2], w104[1], w104[3]))
write_tab("tab_weak_size.tex",
  "Mean first-stage $F$ & 2SLS $t$-test, actual size & Anderson--Rubin test, actual size",
  c(sprintf("%s & %s & %s", fmt(w10[2], 0), fmt(w10[1], 3), fmt(w10[3], 3)),
    sprintf("%s & %s & %s", fmt(w104[2], 0), fmt(w104[1], 3), fmt(w104[3], 3))),
  "ccc")
cat("done.\n")
