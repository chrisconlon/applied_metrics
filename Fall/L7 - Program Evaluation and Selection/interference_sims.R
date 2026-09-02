# Interference in a marketplace experiment (Session 5b, "when units interfere").
# Buyers in each market draw from a shared, limited inventory. A coupon (treatment) raises a buyer's
# purchase propensity. When supply binds, treated buyers take units that control buyers would have
# bought, so a buyer-level A/B test overstates the global effect. Dependencies: none.
set.seed(20261001)
pdf_open <- function(f, w = 7, h = 4.2) pdf(f, width = w, height = h, pointsize = 11)
col1 <- "#1f4e79"; col2 <- "#bb0000"; col3 <- "#e69f00"
p0 <- 0.20; delta <- 0.10; B <- 200                    # buyers per market; control propensity; coupon lift
run_market <- function(treat, supply) {                # treat: 0/1 vector over buyers; returns sales per buyer
  want <- rbinom(length(treat), 1, p0 + delta * treat)
  order_ <- sample(seq_along(treat)); sold <- integer(length(treat)); left <- supply
  for (i in order_) { if (want[i] == 1 && left > 0) { sold[i] <- 1L; left <- left - 1 } }
  sold
}
sim <- function(ratio, M = 200) {                      # ratio = supply / expected control demand
  supply <- round(ratio * p0 * B)
  naive <- gte <- clus <- numeric(M); cl_treat <- rbinom(M, 1, 0.5)
  for (m in 1:M) {
    tr <- rbinom(B, 1, 0.5); s <- run_market(tr, supply)
    naive[m] <- mean(s[tr == 1]) - mean(s[tr == 0])               # buyer-level A/B
    gte[m] <- mean(run_market(rep(1, B), supply)) - mean(run_market(rep(0, B), supply))   # all-on vs all-off
    clus[m] <- mean(run_market(rep(cl_treat[m], B), supply))      # market-level randomization
  }
  c(ratio = ratio, naive = mean(naive), gte = mean(gte),
    cluster = mean(clus[cl_treat == 1]) - mean(clus[cl_treat == 0]),
    se_naive = sd(naive) / sqrt(M), se_cluster = sqrt(var(clus[cl_treat == 1]) / sum(cl_treat) + var(clus[cl_treat == 0]) / sum(1 - cl_treat)))
}
ratios <- c(0.5, 0.75, 1, 1.25, 1.5, 2, 3)
res <- t(sapply(ratios, sim)); print(round(res, 4))
pdf_open("fig_interference.pdf")
par(mar = c(4, 4, 1, 1))
plot(res[, "ratio"], res[, "naive"], type = "b", pch = 16, lwd = 2, col = col2, ylim = c(-0.02, 0.12), log = "x",
     xlab = "supply / expected demand without the coupon (log scale)", ylab = "estimated effect of the coupon on sales per buyer")
lines(res[, "ratio"], res[, "gte"], type = "b", pch = 16, lwd = 3, col = "black")
lines(res[, "ratio"], res[, "cluster"], type = "b", pch = 1, lwd = 2, col = col1)
abline(h = delta, lty = 3); abline(h = 0, lty = 3)
legend("topleft", bty = "n", legend = c("buyer-level A/B test (naive)", "global treatment effect (truth)", "market-level randomization"),
       col = c(col2, "black", col1), lwd = c(2, 3, 2), pch = c(16, 16, 1))
dev.off()
write_tab <- function(file, header, rows, align) { con <- file(file, "w"); writeLines(sprintf("\\begin{tabular}{%s}\\toprule", align), con)
  writeLines(paste0(header, " \\\\ \\midrule"), con); for (r in rows) writeLines(paste0(r, " \\\\"), con); writeLines("\\bottomrule\\end{tabular}", con); close(con) }
fmt <- function(x, d = 3) formatC(x, format = "f", digits = d)
sel <- res[res[, "ratio"] %in% c(0.75, 1, 2), ]
write_tab("tab_interference.tex",
  "Supply / control demand & Buyer-level A/B & Global effect (truth) & Market-level randomization & SE, buyer-level & SE, market-level",
  apply(sel, 1, function(r) sprintf("%s & %s & %s & %s & %s & %s", fmt(r["ratio"], 2), fmt(r["naive"]), fmt(r["gte"]), fmt(r["cluster"]), fmt(r["se_naive"], 4), fmt(r["se_cluster"], 4))),
  "lccccc")
# design effect: MDE as a function of clusters, for a fixed number of buyers
pdf_open("fig_design_effect.pdf", w = 6.5, h = 4)
par(mar = c(4, 4, 1, 1))
n_tot <- 40000; icc <- c(0.01, 0.05, 0.2); G <- c(10, 20, 50, 100, 200, 500, 1000)
plot(NA, xlim = c(10, 1000), ylim = c(0, 0.1), log = "x", xlab = "number of clusters randomized (fixed 40,000 buyers)", ylab = "minimum detectable effect (80% power)")
sd_y <- sqrt(p0 * (1 - p0))
for (k in seq_along(icc)) { m <- n_tot / G; deff <- 1 + (m - 1) * icc[k]; mde <- 2.8 * sd_y * sqrt(4 * deff / n_tot)
  lines(G, mde, lwd = 2, col = c(col1, col3, col2)[k]) }
abline(h = 2.8 * sd_y * sqrt(4 / n_tot), lty = 2); text(300, 2.8 * sd_y * sqrt(4 / n_tot), "buyer-level randomization", pos = 3, cex = 0.8)
legend("topright", bty = "n", title = "within-cluster correlation", legend = icc, col = c(col1, col3, col2), lwd = 2)
dev.off()
cat("done.\n")
