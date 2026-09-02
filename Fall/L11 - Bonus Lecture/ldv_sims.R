# Simulations, figures, and tables for Session 13: limited dependent variables.
#   L13a - Censoring and Tobit      (Part A)
#   L13b - Duration Models          (Part B)
#   L13c - Count Data               (Part C)
# All data are simulated with known ground truth.  Run:  Rscript ldv_sims.R
# Produces fig_*.pdf and tab_*.tex used by the decks.

# Dependencies: fixest, plus survival (ships with R). Nothing else.
suppressPackageStartupMessages({ library(survival); library(fixest) })
set.seed(20261203)
pdf_open <- function(f, w = 7, h = 4.2) pdf(f, width = w, height = h, pointsize = 11)
col1 <- "#1f4e79"; col2 <- "#bb0000"; col3 <- "#e69f00"; grey <- "grey55"
write_tab <- function(file, header, rows, align) {
  con <- file(file, "w")
  writeLines(sprintf("\\begin{tabular}{%s}\\toprule", align), con)
  writeLines(paste0(header, " \\\\ \\midrule"), con)
  for (r in rows) writeLines(paste0(r, " \\\\"), con)
  writeLines("\\bottomrule\\end{tabular}", con); close(con)
}
fmt <- function(x, d = 3) formatC(x, format = "f", digits = d)
pse <- function(x, d = 3) paste0("(", fmt(x, d), ")")

# ==========================================================================
# Part A. Censoring: spending on a product category with many zeros.
#   y* = b0 + b1 x + e,  y = max(0, y*).   Truth: b1 = 1.5, sigma = 2.
# ==========================================================================
N <- 2000; b0 <- -0.5; b1 <- 1.5; sig <- 2
x <- rnorm(N); ystar <- b0 + b1 * x + rnorm(N, 0, sig); y <- pmax(0, ystar)
d <- data.frame(x, y, ystar, pos = y > 0)
cat(sprintf("Tobit design: %.0f%% zeros\n", 100 * mean(y == 0)))
ols_all <- lm(y ~ x, d); ols_pos <- lm(y ~ x, d, subset = pos); ols_star <- lm(ystar ~ x, d)
tob <- survreg(Surv(y, y > 0, type = "left") ~ x, d, dist = "gaussian")   # Tobit = left-censored normal
pro <- feglm(pos ~ x, d, family = binomial(link = "probit"))            # probit on the zero/positive split
b_t <- coef(tob); s_t <- tob$scale
cat(sprintf("OLS all %.3f | OLS y>0 %.3f | OLS y* %.3f | Tobit %.3f (sigma %.3f) | probit b/s %.3f\n",
            coef(ols_all)[2], coef(ols_pos)[2], coef(ols_star)[2], b_t[2], s_t, coef(pro)[2]))
write_tab("tab_tobit.tex",
  " & OLS on $y$ (all) & OLS on $y$ ($y > 0$ only) & Tobit MLE & Probit on $\\mathbb{1}\\{y>0\\}$ & OLS on $y^*$ (infeasible)",
  c(sprintf("Slope on $x$ (truth $= 1.5$) & %s & %s & %s & %s & %s", fmt(coef(ols_all)[2]), fmt(coef(ols_pos)[2]),
            fmt(b_t[2]), paste0(fmt(coef(pro)[2]), " $= \\beta/\\sigma$"), fmt(coef(ols_star)[2])),
    sprintf(" & %s & %s & %s & %s & %s", pse(sqrt(diag(vcov(ols_all)))[2]), pse(sqrt(diag(vcov(ols_pos)))[2]),
            pse(sqrt(diag(vcov(tob)))[2]), pse(se(pro)[2]), pse(sqrt(diag(vcov(ols_star)))[2])),
    sprintf("Intercept (truth $= -0.5$) & %s & %s & %s & %s & %s", fmt(coef(ols_all)[1]), fmt(coef(ols_pos)[1]),
            fmt(b_t[1]), fmt(coef(pro)[1]), fmt(coef(ols_star)[1])),
    sprintf("$\\hat\\sigma$ (truth $= 2$) & %s & %s & %s & --- & %s", fmt(summary(ols_all)$sigma), fmt(summary(ols_pos)$sigma),
            fmt(s_t), fmt(summary(ols_star)$sigma)),
    sprintf("$N$ & %d & %d & %d & %d & %d", N, sum(d$pos), N, N, N)),
  "lccccc")

pdf_open("fig_tobit_scatter.pdf")
par(mar = c(4, 4, 1, 1))
plot(d$x, d$ystar, pch = 16, cex = 0.35, col = "grey80", xlab = "x (standardized income)", ylab = "spending")
points(d$x, d$y, pch = 16, cex = 0.35, col = ifelse(d$pos, col1, col2))
abline(b0, b1, lwd = 2, lty = 2); abline(coef(ols_all), lwd = 2, col = col3); abline(coef(ols_pos), lwd = 2, col = col2)
xs <- seq(-3, 3, length.out = 200); z <- (b_t[1] + b_t[2] * xs) / s_t
lines(xs, pnorm(z) * (b_t[1] + b_t[2] * xs) + s_t * dnorm(z), lwd = 3, col = col1)
legend("topleft", bty = "n", cex = 0.85,
       legend = c("latent y* (grey), observed y (blue = positive, red = censored at 0)", "truth: E[y* | x]",
                  "OLS on all y", "OLS on y > 0", "Tobit: E[y | x]"),
       col = c(NA, "black", col3, col2, col1), lwd = c(NA, 2, 2, 2, 3), lty = c(NA, 2, 1, 1, 1))
dev.off()

# marginal effects: three objects
pdf_open("fig_tobit_me.pdf", w = 7, h = 3.8)
par(mar = c(4, 4, 1, 1))
z <- (b_t[1] + b_t[2] * xs) / s_t; lam <- dnorm(z) / pnorm(z)
plot(xs, rep(b_t[2], 200), type = "l", lwd = 2, lty = 2, ylim = c(0, 1.7), xlab = "x", ylab = "marginal effect of x")
lines(xs, b_t[2] * pnorm(z), lwd = 3, col = col1)
lines(xs, b_t[2] * (1 - z * lam - lam^2), lwd = 2, col = col2)
legend("topleft", bty = "n", cex = 0.9,
       legend = c(expression(paste("on the latent ", y^"*", ":  ", beta)),
                  expression(paste("on observed ", y, ":  ", beta %.% Phi(x*minute*beta/sigma))),
                  expression(paste("on ", y, " given ", y > 0, ":  ", beta %.% (1 - z*lambda(z) - lambda(z)^2)))),
       col = c("black", col1, col2), lwd = c(2, 3, 2), lty = c(2, 1, 1))
dev.off()

# censoring vs truncation picture
pdf_open("fig_tobit_trunc.pdf", w = 8, h = 3.2)
par(mfrow = c(1, 3), mar = c(4, 4, 2.5, 1))
hist(d$ystar, breaks = 40, col = "grey85", border = "white", main = "latent y* (never seen)", xlab = "")
hist(d$y, breaks = 40, col = col1, border = "white", main = "censored: y = max(0, y*)", xlab = "")
hist(d$y[d$pos], breaks = 40, col = col2, border = "white", main = "truncated: only y > 0 recorded", xlab = "")
dev.off()

# ==========================================================================
# Part B. Durations: subscription churn.  Weibull proportional hazards,
#   h(t|x) = p t^(p-1) exp(x'b), p = 0.7 (early churn), b_price = 0.5, b_usage = -0.4.
#   Observation ends at 24 months; some customers drop out of the panel early.
# ==========================================================================
Nd <- 3000; p_true <- 0.7; b_price <- 0.5; b_usage <- -0.4
price <- rbinom(Nd, 1, 0.5); usage <- rnorm(Nd)
lin <- b_price * price + b_usage * usage
T_true <- (-log(runif(Nd)) / exp(lin))^(1 / p_true) * 17     # scale: median spell ~ 10 months
C <- pmin(24, ifelse(runif(Nd) < 0.15, runif(Nd, 1, 24), 24))  # administrative + random censoring
time <- pmin(T_true, C); event <- as.integer(T_true <= C)
dd <- data.frame(time, event, price, usage, T_true)
cat(sprintf("Duration design: %.0f%% censored, median true spell %.1f months\n", 100 * mean(event == 0), median(T_true)))

km <- survfit(Surv(time, event) ~ price, dd)
cox <- coxph(Surv(time, event) ~ price + usage, dd)
wei <- survreg(Surv(time, event) ~ price + usage, dd, dist = "weibull")
ex  <- survreg(Surv(time, event) ~ price + usage, dd, dist = "exponential")
# survreg is AFT: log T = a + x'g + scale * W.  PH coefficient = -g / scale; Weibull shape p = 1/scale.
b_wei <- -coef(wei)[-1] / wei$scale; b_ex <- -coef(ex)[-1]
se_wei <- sqrt(diag(vcov(wei)))[2:3] / wei$scale; se_ex <- sqrt(diag(vcov(ex)))[2:3]
ols_unc <- lm(log(time) ~ price + usage, dd, subset = event == 1)     # drops censored
ols_all <- lm(log(time) ~ price + usage, dd)                          # treats censored as events
cat(sprintf("Cox %.3f/%.3f | Weibull %.3f/%.3f (p=%.2f) | Exp %.3f/%.3f\n", coef(cox)[1], coef(cox)[2],
            b_wei[1], b_wei[2], 1 / wei$scale, b_ex[1], b_ex[2]))
write_tab("tab_duration.tex",
  " & Cox PH & Weibull PH & Exponential PH & OLS $\\ln T$, uncensored only & OLS $\\ln T$, censored as exits",
  c(sprintf("Price increase (truth $\\beta = 0.5$; HR $= %.2f$) & %s & %s & %s & %s & %s", exp(0.5),
            fmt(coef(cox)[1]), fmt(b_wei[1]), fmt(b_ex[1]), fmt(coef(ols_unc)[2]), fmt(coef(ols_all)[2])),
    sprintf(" & %s & %s & %s & %s & %s", pse(sqrt(diag(vcov(cox)))[1]), pse(se_wei[1]), pse(se_ex[1]),
            pse(sqrt(diag(vcov(ols_unc)))[2]), pse(sqrt(diag(vcov(ols_all)))[2])),
    sprintf("Usage (truth $\\beta = -0.4$) & %s & %s & %s & %s & %s", fmt(coef(cox)[2]), fmt(b_wei[2]), fmt(b_ex[2]),
            fmt(coef(ols_unc)[3]), fmt(coef(ols_all)[3])),
    sprintf(" & %s & %s & %s & %s & %s", pse(sqrt(diag(vcov(cox)))[2]), pse(se_wei[2]), pse(se_ex[2]),
            pse(sqrt(diag(vcov(ols_unc)))[3]), pse(sqrt(diag(vcov(ols_all)))[3])),
    sprintf("Shape $p$ (truth $= 0.7$) & --- & %s & 1 (imposed) & --- & ---", fmt(1 / wei$scale, 2)),
    "Baseline hazard & unrestricted & Weibull & constant & (AFT, wrong sign) & (AFT, wrong sign)"),
  "lccccc")

pdf_open("fig_dur_km.pdf")
par(mar = c(4, 4, 1, 1))
plot(km, col = c(col1, col2), lwd = 2, conf.int = TRUE, mark.time = TRUE, xlab = "months since signup",
     ylab = "share still subscribed, S(t)")
legend("topright", bty = "n", legend = c("no price increase", "price increase"), col = c(col1, col2), lwd = 2)
dev.off()

pdf_open("fig_dur_hazards.pdf", w = 8, h = 3.4)
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
t <- seq(0.1, 24, length.out = 200)
plot(NA, xlim = c(0, 24), ylim = c(0, 0.35), xlab = "t", ylab = "hazard h(t)", main = "Weibull hazards")
for (pp in c(0.7, 1, 1.5)) lines(t, (pp / 10) * (t / 10)^(pp - 1), lwd = 2, col = c(col2, "black", col1)[match(pp, c(0.7, 1, 1.5))])
legend("topright", bty = "n", legend = c("p = 0.7: falling (early churn)", "p = 1: constant (exponential)", "p = 1.5: rising (wear-out)"),
       col = c(col2, "black", col1), lwd = 2)
plot(NA, xlim = c(0, 24), ylim = c(0, 1), xlab = "t", ylab = "survival S(t)", main = "Implied survival")
for (pp in c(0.7, 1, 1.5)) lines(t, exp(-(t / 10)^pp), lwd = 2, col = c(col2, "black", col1)[match(pp, c(0.7, 1, 1.5))])
dev.off()

# unobserved heterogeneity: two types with constant hazards -> falling population hazard
pdf_open("fig_dur_hetero.pdf", w = 7, h = 3.8)
par(mar = c(4, 4, 1, 1))
hA <- 0.04; hB <- 0.30; wA <- 0.5
t <- seq(0, 36, length.out = 300)
SA <- exp(-hA * t); SB <- exp(-hB * t); Spop <- wA * SA + (1 - wA) * SB
hpop <- (wA * hA * SA + (1 - wA) * hB * SB) / Spop
plot(t, hpop, type = "l", lwd = 3, ylim = c(0, 0.32), xlab = "months", ylab = "hazard")
abline(h = hA, col = col1, lwd = 2, lty = 2); abline(h = hB, col = col2, lwd = 2, lty = 2)
legend("right", bty = "n", legend = c("loyal type: constant hazard 0.04", "flighty type: constant hazard 0.30",
                                        "population hazard: falls, though nobody's does"),
       col = c(col1, col2, "black"), lwd = c(2, 2, 3), lty = c(2, 2, 1))
dev.off()

# ==========================================================================
# Part C. Counts: purchases per customer over a quarter.
#   lambda = exposure * exp(-0.5 + 0.6 promo + 0.2 tenure) * nu,  nu ~ Gamma(1/alpha, 1/alpha), alpha = 0.8
#   plus 25% "never-buyers" (structural zeros).   Truth: promo effect 0.6 among buyers.
# ==========================================================================
Nc <- 4000; alpha_true <- 0.8; pi0 <- 0.25
promo <- rbinom(Nc, 1, 0.5); tenure <- rnorm(Nc); months <- sample(1:3, Nc, replace = TRUE)
nu <- rgamma(Nc, shape = 1 / alpha_true, rate = 1 / alpha_true)
lam <- months * exp(-0.5 + 0.6 * promo + 0.2 * tenure) * nu
never <- rbinom(Nc, 1, pi0)
yc <- ifelse(never == 1, 0L, rpois(Nc, lam))
dc <- data.frame(y = yc, promo, tenure, months)
cat(sprintf("Count design: mean %.2f, var %.2f, share zero %.2f\n", mean(yc), var(yc), mean(yc == 0)))

pois <- fepois(y ~ promo + tenure, dc, offset = ~log(months), vcov = "iid")
pois_rob <- fepois(y ~ promo + tenure, dc, offset = ~log(months), vcov = "hetero")
nb <- fenegbin(y ~ promo + tenure, dc, offset = ~log(months))
theta_nb <- nb$theta
# zero-inflated negative binomial by hand: P(y=0) = pi + (1-pi) f_NB(0); P(y=k) = (1-pi) f_NB(k)
X <- cbind(1, dc$promo, dc$tenure); off <- log(dc$months)
zinb_nll <- function(th) {
  b <- th[1:3]; la <- exp(X %*% b + off); a <- exp(th[4]); pi <- plogis(th[5])
  f <- dnbinom(dc$y, size = 1 / a, mu = la)
  -sum(ifelse(dc$y == 0, log(pi + (1 - pi) * f), log(1 - pi) + log(f)))
}
zinb <- optim(c(coef(nb)[1:3], log(1 / theta_nb), 0), zinb_nll, method = "BFGS", hessian = TRUE)
zinb_se <- sqrt(diag(solve(zinb$hessian)))
ll <- c(pois = as.numeric(logLik(pois)), nb = as.numeric(logLik(nb)), zinb = -zinb$value)
k  <- c(3, 4, 5)
aic <- -2 * ll + 2 * k; bic <- -2 * ll + log(Nc) * k
# Vuong test NB vs ZINB (non-nested in the usual sense): m_i = log f_zinb - log f_nb
b <- zinb$par[1:3]; la_z <- exp(X %*% b + off); a_z <- exp(zinb$par[4]); pi_z <- plogis(zinb$par[5])
f_z <- ifelse(dc$y == 0, pi_z + (1 - pi_z) * dnbinom(0, size = 1 / a_z, mu = la_z), (1 - pi_z) * dnbinom(dc$y, size = 1 / a_z, mu = la_z))
f_nb <- dnbinom(dc$y, size = theta_nb, mu = fitted(nb))
m <- log(f_z) - log(f_nb); vuong <- sqrt(Nc) * mean(m) / sd(m)
cat(sprintf("Poisson %.3f | NB %.3f (alpha %.2f) | ZINB %.3f (alpha %.2f, pi %.2f) | Vuong z = %.2f\n",
            coef(pois)[2], coef(nb)[2], 1 / theta_nb, b[2], a_z, pi_z, vuong))
write_tab("tab_count_models.tex",
  " & Poisson & Poisson, robust SE & Negative binomial & Zero-inflated NB",
  c(sprintf("Promo (truth $= 0.6$ among buyers) & %s & %s & %s & %s", fmt(coef(pois)[2]), fmt(coef(pois_rob)[2]), fmt(coef(nb)[2]), fmt(b[2])),
    sprintf(" & %s & %s & %s & %s", pse(se(pois)[2]), pse(se(pois_rob)[2]), pse(se(nb)[2]), pse(zinb_se[2])),
    sprintf("Tenure (truth $= 0.2$) & %s & %s & %s & %s", fmt(coef(pois)[3]), fmt(coef(pois_rob)[3]), fmt(coef(nb)[3]), fmt(b[3])),
    sprintf(" & %s & %s & %s & %s", pse(se(pois)[3]), pse(se(pois_rob)[3]), pse(se(nb)[3]), pse(zinb_se[3])),
    sprintf("Overdispersion $\\alpha$ (truth $= 0.8$) & 0 (imposed) & --- & %s & %s", fmt(1 / theta_nb, 2), fmt(a_z, 2)),
    sprintf("Never-buyer share $\\pi$ (truth $= 0.25$) & --- & --- & --- & %s", fmt(pi_z, 2)),
    sprintf("Log-likelihood & %s & (same) & %s & %s", fmt(ll[1], 1), fmt(ll[2], 1), fmt(ll[3], 1)),
    sprintf("AIC / BIC & %s / %s & & %s / %s & %s / %s", fmt(aic[1], 0), fmt(bic[1], 0), fmt(aic[2], 0), fmt(bic[2], 0), fmt(aic[3], 0), fmt(bic[3], 0)),
    sprintf("Vuong, ZINB vs.\\ NB & & & \\multicolumn{2}{c}{$z = %.2f$ (favors ZINB)} ", vuong)),
  "lcccc")

# observed vs fitted count distributions
pdf_open("fig_count_fit.pdf", w = 7, h = 3.8)
par(mar = c(4, 4, 1, 1))
ks <- 0:12; obs <- sapply(ks, function(j) mean(dc$y == j))
pp <- sapply(ks, function(j) mean(dpois(j, fitted(pois))))
pn <- sapply(ks, function(j) mean(dnbinom(j, size = theta_nb, mu = fitted(nb))))
pz <- sapply(ks, function(j) mean(ifelse(j == 0, pi_z + (1 - pi_z) * dnbinom(0, size = 1 / a_z, mu = la_z), (1 - pi_z) * dnbinom(j, size = 1 / a_z, mu = la_z))))
barplot(obs, names.arg = ks, col = "grey85", border = "white", ylim = c(0, max(obs, pp) * 1.1), xlab = "purchases in the quarter", ylab = "share")
mids <- seq(0.7, by = 1.2, length.out = length(ks))
lines(mids, pp, type = "b", pch = 16, col = col3, lwd = 2); lines(mids, pn, type = "b", pch = 16, col = col1, lwd = 2)
lines(mids, pz, type = "b", pch = 16, col = col2, lwd = 2)
legend("topright", bty = "n", legend = c("observed", "Poisson fit", "negative binomial fit", "zero-inflated NB fit"),
       fill = c("grey85", NA, NA, NA), border = c("white", NA, NA, NA), col = c(NA, col3, col1, col2), lwd = c(NA, 2, 2, 2), pch = c(NA, 16, 16, 16))
dev.off()

# Panel: customer FE, Poisson pseudo-ML vs log(1+y) OLS
Np <- 1500; Tq <- 8
pd <- expand.grid(id = 1:Np, q = 1:Tq)
pd$eta <- rnorm(Np, 0, 0.8)[pd$id]                               # customer heterogeneity, correlated with promo targeting
pd$promo <- rbinom(nrow(pd), 1, plogis(-0.5 + 0.8 * pd$eta))     # heavy buyers get more promos
pd$lam <- exp(-0.8 + 0.6 * pd$promo + pd$eta)
pd$y <- rpois(nrow(pd), pd$lam)
f_pois_fe <- fepois(y ~ promo | id, pd, vcov = ~id)
f_pois_nofe <- fepois(y ~ promo, pd, vcov = ~id)
f_log_fe <- feols(log1p(y) ~ promo | id, pd, vcov = ~id)
f_log_pos <- feols(log(y) ~ promo | id, pd[pd$y > 0, ], vcov = ~id)
cat(sprintf("Panel: Poisson FE %.3f | Poisson no FE %.3f | log1p FE %.3f | log y>0 FE %.3f (share zero %.2f)\n",
            coef(f_pois_fe)[1], coef(f_pois_nofe)[1], coef(f_log_fe)[1], coef(f_log_pos)[1], mean(pd$y == 0)))
write_tab("tab_count_fe.tex",
  " & Poisson, no FE & Poisson + customer FE & OLS $\\ln(1+y)$ + FE & OLS $\\ln y$ ($y>0$) + FE",
  c(sprintf("Promo (truth $= 0.6$ log points) & %s & %s & %s & %s", fmt(coef(f_pois_nofe)[1]), fmt(coef(f_pois_fe)[1]),
            fmt(coef(f_log_fe)[1]), fmt(coef(f_log_pos)[1])),
    sprintf(" & %s & %s & %s & %s", pse(se(f_pois_nofe)[1]), pse(se(f_pois_fe)[1]), pse(se(f_log_fe)[1]), pse(se(f_log_pos)[1])),
    sprintf("Observations & %d & %d & %d & %d", nobs(f_pois_nofe), nobs(f_pois_fe), nobs(f_log_fe), nobs(f_log_pos)),
    sprintf("Share of zeros in $y$ & \\multicolumn{4}{c}{%.2f} ", mean(pd$y == 0))),
  "lcccc")
cat("done.\n")
