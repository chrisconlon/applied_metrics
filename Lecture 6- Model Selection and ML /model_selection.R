# Data Dictionary is here: https://www.ssc.wisc.edu/~bhansen/econometrics/cps09mar_description.pdf

df <- read.table(url("https://www.ssc.wisc.edu/~bhansen/econometrics/cps09mar.txt"),
          col.names=c("age","female","hisp","education","earnings","hours","week",
                      "union","uncov","region","race","marital")) %>% 
          mutate(race = as.factor(race),
          	region = as.factor(region),
          	marital = as.factor(marital),
          	some_college = (education >= 14),
          	college = (education > 15),
          	exper = (age - education - 6),
          	wage = earnings/(week*hours),
          	) %>%
          filter(hisp==1 & female==1)

summary(df$wage)
summary(lm(log(earnings) ~ college ,data=df))

# from https://www.ssc.wisc.edu/~bhansen/crete2019/BruceHansenLecture1.pdf
# Use the sample of Hispanic women (n = 3003)
# Create the wage variable as annual earnings divided by number of weeks worked and average number of hours worked per week. Create the dependent variable by taking the natural logarithm
# Create the experience variable as age less years of education, less 6. Divide by its maximum variable in the sample to standardize between 0 and 1. (Otherwise the polynomial regression will become numerically unstable)
# Create the other variables as needed


# Consider the following models

#Model 1: Education enters as a dummy variable for college graduate. Experience enters as a quadratic
#Model 2: Education enters linearly. Experience enters as a quadratic
#Model 3: Education enters flexibly, with indicators for each level. Experience enters as a quadratic
#Model 4: Experience enters as a cubic polynomial 
#Model 5: Experience enters as a quartic polynomial 
#Model 6: Experience enters as a 5th order polynomial 
#Model 7: Experience enters as a 6th order polynomial
#Model 8: Ridge regression for the full model including region dummies and marital status (report best choice of lambda)
#Model 9: LASSO regression for full model (report best choice of lambda)
#Model 10: LASSO with all variables as second order interactions


# Using Model 7 -- plot the expected wage for college graduates and non-college graduates

# Create a table of the BIC, AIC, and 10-fold CV MSE values for all models. Based on this evidence what is your preferred model? 
# How does the estimate for returns to college compare? (remember we are using log-wage!)
