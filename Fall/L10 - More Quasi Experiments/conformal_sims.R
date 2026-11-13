# Conformal inference for synthetic control (Chernozhukov, Wuthrich and Zhu 2021, JASA) vs placebo-in-space.
# Same soda-tax DGP as sdid_sims.R (treated city inside the hull): 24 quarters, tax cuts sales 10% from quarter 17.
# Two donor pools: all 40 donors, and the first 8. Moving-block (cyclic) and iid permutations; the block
# version matches augsynth:::compute_permute_test_stats(type = "block") exactly. Dependencies: quadprog.
suppressPackageStartupMessages(library(quadprog)); set.seed(66)
J <- 40; Tq <- 24; T0 <- 16
lam <- cbind(seq(0, 2, length.out = Tq), rnorm(Tq)); mu <- matrix(runif((J + 1) * 2), J + 1, 2); mu[1, ] <- c(0.75, 0.3)
Y <- 100 + 5 * lam %*% t(mu) + matrix(rnorm(Tq * (J + 1)), Tq, J + 1)
truth <- -0.1 * mean(Y[(T0 + 1):Tq, 1]); Y[(T0 + 1):Tq, 1] <- 0.9 * Y[(T0 + 1):Tq, 1]

sc_w <- function(y1, Y0) { k <- ncol(Y0)                    # simplex-constrained least squares
  solve.QP(crossprod(Y0) + diag(1e-8, k), crossprod(Y0, y1), cbind(1, diag(k)), c(1, rep(0, k)), meq = 1)$solution }
post <- (T0 + 1):Tq

# Placebo in space: post/pre RMSPE ratio for each unit treated in turn, p = rank / (J + 1)
placebo_p <- function(Y) { r <- sapply(1:ncol(Y), function(i) { w <- sc_w(Y[1:T0, i], Y[1:T0, -i, drop = FALSE])
  g <- Y[, i] - Y[, -i, drop = FALSE] %*% w; sqrt(mean(g[post]^2)) / sqrt(mean(g[1:T0]^2)) }); mean(r >= r[1]) }

# Conformal: impose H0 (effect = theta every post quarter), fit weights on ALL quarters, permute residuals over time
conformal_p <- function(Y, theta, type = "block") {
  y1 <- Y[, 1] - theta * (seq_len(Tq) > T0)                 # 1. remove the hypothesized effect
  u <- y1 - Y[, -1, drop = FALSE] %*% sc_w(y1, Y[, -1, drop = FALSE])  # 2. fit on all Tq quarters, 3. residuals
  S <- function(u) mean(abs(u[post]))                       # 4. test statistic on the post-period residuals
  Sp <- if (type == "block") sapply(0:(Tq - 1), function(s) S(u[((seq_len(Tq) - 1 + s) %% Tq) + 1]))  # 5a. all Tq cyclic shifts
        else { set.seed(7); sapply(1:2000, function(b) S(sample(u))) }                              # 5b. 2,000 iid shuffles
  mean(Sp >= S(u)) }
conformal_ci <- function(Y, grid, type, alpha = 0.10) range(grid[sapply(grid, function(th) conformal_p(Y, th, type)) > alpha])

grid <- seq(-25, 10, by = 0.1)
row <- function(Y) { w <- sc_w(Y[1:T0, 1], Y[1:T0, -1, drop = FALSE]); est <- mean((Y[, 1] - Y[, -1, drop = FALSE] %*% w)[post])
  cb <- conformal_ci(Y, grid, "block"); ci <- conformal_ci(Y, grid, "iid")
  c(est = est, placebo = placebo_p(Y), p_block = conformal_p(Y, 0, "block"), lo_b = cb[1], hi_b = cb[2],
    p_iid = conformal_p(Y, 0, "iid"), lo_i = ci[1], hi_i = ci[2]) }
res <- rbind(all40 = row(Y), first8 = row(Y[, 1:9]))
print(round(res, 3)); cat("truth", round(truth, 2), "\n")

fmt <- function(x, d = 1) formatC(x, format = "f", digits = d)
pf <- function(x) if (x < 0.001) "$<0.001$" else fmt(x, 3)
r <- function(lab, f) sprintf("%s & %s & %s \\\\", lab, f(res[1, ]), f(res[2, ]))
con <- file("tab_conformal.tex", "w")
writeLines(c("\\begin{tabular}{lcc}\\toprule",
  sprintf(" & %d donors & 8 donors \\\\ \\midrule", J),
  r("SC estimate (truth $%s$)" |> sprintf(fmt(truth)), function(x) fmt(x["est"])),
  r("Placebo in space $p$ \\quad (floor $1/(J+1)$)", function(x) fmt(x["placebo"], 3)),
  r("Conformal $p$, moving block \\quad (floor $1/T$)", function(x) fmt(x["p_block"], 3)),
  r("\\quad 90\\% CI", function(x) sprintf("[%s, %s]", fmt(x["lo_b"]), fmt(x["hi_b"]))),
  r("Conformal $p$, iid permutations", function(x) pf(x["p_iid"])),
  r("\\quad 90\\% CI", function(x) sprintf("[%s, %s]", fmt(x["lo_i"]), fmt(x["hi_i"]))),
  "\\bottomrule\\end{tabular}"), con); close(con)
cat("done.\n")
