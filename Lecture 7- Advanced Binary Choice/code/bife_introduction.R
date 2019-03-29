## ---- eval=FALSE---------------------------------------------------------
#  glm(y ~ X + factor(i), family = binomial())

## ---- eval=FALSE---------------------------------------------------------
#  time_bife   <- system.time(bife(y ~ x + d | id, model = "logit", bias_corr = "ana"))[3]
#  time_clogit <- if(require("survival")) system.time(clogit(y ~ x + d + strata(id)))[3]
#  time_glm    <- system.time(glm(y ~ x + d + 0 + factor(id), family = binomial()))[3]

## ---- echo=FALSE, results='asis'-----------------------------------------
# Load 'bife'
library("bife")

# Load results --- store_N and store_T
time_n <- time_n
time_t <- time_t

# N and T vector
N_vector <- rep(100, 10)
T_vector <- rep(10, 10)

# Bind results
results <- cbind("N" = time_n[, 1], "T" = T_vector, time_n[, 2:4], "N" = N_vector, time_t)

# Print results
knitr::kable(results)

## ---- echo=FALSE, warning=FALSE, message=FALSE, fig.show='hold'----------
# Load package
if (require("ggplot2")) {

  # Colour palette for colour-blind
  cb.Palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
  # Transform to data.frame
  time_n <- data.frame(time_n)
  time_t <- data.frame(time_t)
  
  # Regression N
  plot_N <- data.frame(N = time_n[["N"]])
  plot_N[["bife_corr"]] <- fitted(lm(bife_corr ~ N, data = time_n))
  plot_N[["clogit"]] <- fitted(lm(clogit ~ N, data = time_n))
  plot_N[["glm"]] <-
  fitted(lm(glm ~ N + I(N ^ 2) + I(N ^ 3), data = time_n))
  
  # Regression T
  plot_T <- data.frame(T = time_t[["T"]])
  plot_T[["bife_corr"]] <- fitted(lm(bife_corr ~ T, data = time_t))
  plot_T[["clogit"]] <-
  fitted(lm(clogit ~ T + I(T ^ 2) + I(T ^ 3), data = time_t))
  plot_T[["glm"]] <- fitted(lm(glm ~ T, data = time_t))
  
  # Plot N
  p <- ggplot(plot_N) +
  ylab(NULL) +
  xlim(100, 1000) +
  ylim(0, 1) +
  theme_bw() +
  theme(legend.justification = c(1, 1),
  legend.position = c(1, 1)) +
  geom_line(aes(N, bife_corr, colour = "bife_corr"), size = 0.5) +
  geom_line(aes(N, clogit, colour = "clogit"), size = 0.5) +
  geom_line(aes(N, glm, colour = "glm"), size = 0.5) +
  scale_color_manual("", values = cb.Palette)
  
  # Plot T
  q <- ggplot(plot_T) +
  ylab(NULL) +
  xlim(10, 100) +
  ylim(0, 1) +
  theme_bw() +
  theme(legend.justification = c(0, 1),
  legend.position = c(0, 1)) +
  geom_line(aes(T, bife_corr, colour = "bife_corr"), size = 0.5) +
  geom_line(aes(T, clogit, colour = "clogit"), size = 0.5) +
  geom_line(aes(T, glm, colour = "glm"), size = 0.5) +
  scale_color_manual("", values = cb.Palette)
  
  # Print
  p
  q
}

## ---- echo=FALSE, results='asis'-----------------------------------------
# Load results
results_psid <- results_psid

# Change the order of the cols and rename cols
results_psid <- cbind(results_psid[, 1], results_psid[, 3], results_psid[, 2], results_psid[, 4])
colnames(results_psid) <- c("bife",  "glm", "bife_corr", "clogit")

# Print results
knitr::kable(results_psid)

## ---- echo=FALSE, warning=FALSE------------------------------------------
# Load data
psid <- psid

## ------------------------------------------------------------------------
mod_logit <- bife(LFP ~ AGE + I(INCH / 1000) + KID1 + KID2 + KID3 | ID, data = psid, bias_corr = "ana")
summary(mod_logit)

## ------------------------------------------------------------------------
apeff_bife(mod_logit, discrete = c("KID1", "KID2", "KID3"), bias_corr = "ana")

## ------------------------------------------------------------------------
mod_probit <- bife(LFP ~ AGE + I(INCH / 1000) + KID1 + KID2 + KID3 | ID, 
                   data = psid, bias_corr = "ana", model = "probit")
summary(mod_probit)

## ------------------------------------------------------------------------
apeff_bife(mod_probit, discrete = c("KID1", "KID2", "KID3"), bias_corr = "ana")

## ---- echo=FALSE, results='asis'-----------------------------------------
# Load results
results_acs <- results_acs

# Change the order of the cols and rename cols
results_acs <- cbind(results_acs[, 1], results_acs[, 3], results_acs[, 2], results_acs[, 4])
colnames(results_acs) <- c("bife",  "glm", "bife_corr", "clogit")

# Print results
knitr::kable(results_acs)

## ------------------------------------------------------------------------
# Load data
acs <- acs

print(try(if(require("survival")) clogit(LFP ~ AGEP + I(PINCP / 1000) + FER + strata(ST), data = acs)))

## ------------------------------------------------------------------------
mod_logit <- bife(LFP ~ AGEP + I(PINCP / 1000) + FER | ST, data = acs, bias_corr = "no")
summary(mod_logit)

## ------------------------------------------------------------------------
apeff_bife(mod_logit, discrete = "FER")

