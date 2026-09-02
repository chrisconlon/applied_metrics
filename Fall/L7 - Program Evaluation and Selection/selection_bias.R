# ---------------------------
# Setup
# ---------------------------
set.seed(2025)
library(MASS)         # for mvrnorm
library(sampleSelection)
library(stargazer)

n <- 2000

# regressors
educ  <- rnorm(n, 12, 2)
exper <- rnorm(n, 10, 5)

# create correlated errors (epsilon for wage, u for selection) with rho != 0
rho <- 0.6                   # correlation between wage error and selection error
sigma_eps <- 1
Sigma <- matrix(c(sigma_eps^2, rho*sigma_eps,
                  rho*sigma_eps, 1), nrow = 2)
errors <- mvrnorm(n, mu = c(0,0), Sigma = Sigma)
eps <- errors[,1]   # wage disturbances
u   <- errors[,2]   # selection disturbances

# ---------------------------
# Variant A: NO exclusion restriction
# selection and outcome share same regressors
# ---------------------------
s_star_A <- 0.5 + 0.3*educ - 0.2*exper + u
select_A <- as.integer(s_star_A > 0)

wage_star <- 2 + 0.10*educ + 0.05*exper + eps
wage_A <- ifelse(select_A==1, wage_star, NA)

ols_A  <- lm(wage_A ~ educ + exper)                            # OLS on observed
heck_A <- selection(select_A ~ educ + exper,                   # Heckman (ML)
                    wage_A ~ educ + exper, method = "ml")

# ---------------------------
# Variant B: WITH an exclusion restriction
# add a variable 'kids' that affects selection but not wage
# ---------------------------
kids <- rbinom(n, 1, 0.3)   # 0/1 indicator: has young children

s_star_B <- 0.5 + 0.3*educ - 0.2*exper - 0.8*kids + u   # kids reduces labor supply
select_B <- as.integer(s_star_B > 0)

wage_B <- ifelse(select_B==1, wage_star, NA)

ols_B  <- lm(wage_B ~ educ + exper)
heck_B <- selection(select_B ~ educ + exper + kids,
                    wage_B ~ educ + exper, method = "ml")

# ---------------------------
# Summaries & LaTeX tables
# ---------------------------
summary(ols_A)
summary(heck_A)

summary(ols_B)
summary(heck_B)

# Make LaTeX tables side-by-side: OLS vs Heckman (variant B with exclusion)
stargazer(ols_B, heck_A, heck_B,
          type = "latex",
          title = "OLS vs Heckman (with exclusion restriction)",
          column.labels = c("OLS (observed)", "Heckman (MLE)","Heckman (Exclusion)"),
          dep.var.labels = "Wage",
          digits = 3,
          header = FALSE,
          no.space = TRUE,
          single.row = TRUE,
          out = "ols_vs_heck_with_exclusion.tex")
