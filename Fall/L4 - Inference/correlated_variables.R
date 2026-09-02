
library(car)
library(sandwich)

# Set the seed for reproducibility
set.seed(12345)

# Number of observations
n <- 200

# Generate random data
AGE <- sample(22:65, n, replace = TRUE)  # Random integer values between 18 and 65
EXP <- AGE - 22 - rbinom(n=n,size=1,prob=0.4) # same as age-22 but one less for some observations
EXP[EXP < 0] <- 0 # replace negative values with 0
LWAGE = 2.5 + .02*AGE + .03*EXP + .5*rnorm(n)

# create data frame
df <- data.frame(LWAGE,AGE,EXP)

reg <- lm(LWAGE ~ AGE + EXP, data = df) # estimate OLS
summary(reg)


linearHypothesis(reg,c("AGE = 0", "EXP = 0"))


linearHypothesis(reg,c("AGE = 0", "EXP = 0"), vcov = vcovHC(reg, type = "HC1"))


linearHypothesis(reg,c("AGE + EXP = 0"), vcov = vcovHC(reg, type = "HC1"))




# coeftest(reg, vcov = vcovHC(reg, type = "HC1"))

c("x1 = 0", "x2 = 0")


robust_summary <- function(model) {
  # Calculate robust standard errors
  robust_se <- sqrt(diag(vcovHC(model, type = "HC1")))
  
  # Use the standard summary and replace the standard errors
  summary_lm <- summary(model)
  
  # Replace the standard errors and t-values
  summary_lm$coefficients[, 2] <- robust_se
  summary_lm$coefficients[, 3] <- summary_lm$coefficients[, 1] / robust_se
  summary_lm$coefficients[, 4] <- 2 * pt(-abs(summary_lm$coefficients[, 3]), df = summary_lm$df[2])
  
  return(summary_lm)
}

# Display the custom summary with robust standard errors
robust_summary(reg)