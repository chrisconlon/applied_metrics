suppressMessages(library(tidyverse))
suppressMessages(library(fixest))
suppressMessages(library(marginaleffects))

# first, the Cornwell and Rupert regression
data <- read.csv('./cornwell-rupert.csv') %>%
  mutate(ED_LEVEL=cut(ED,c(0,8,11,12,15,16,17),
        labels = c("NOHS", "SOMEHS", "HS", "SOMECOL","COL","POST"),
        right=TRUE))

# check that we did it correctly
table(data$ED,data2$ED_LEVEL)

reg_1 <- feols(LWAGE ~ ED + EXP + I(EXP^2) + WKS + OCC + SOUTH + SMSA
  + MS + UNION + FEM, data = data)

# dropping the constant
reg_2 <- feols(LWAGE ~ -1 + i(ED_LEVEL) + EXP + I(EXP^2) + WKS + OCC +
                 SOUTH + SMSA + MS + UNION + FEM, data = data)

# not dropping the constant -- which category is omitted?
reg_3 <- feols(LWAGE ~ 1+ i(ED_LEVEL) + EXP + I(EXP^2) + WKS + OCC +
                 SOUTH + SMSA + MS + UNION + FEM, data = data)

# change the omitted category -- how do coefficients change?
reg_4 <- feols(LWAGE ~ 1+ i(ED_LEVEL,ref="COL") + EXP + I(EXP^2) +
                 WKS + OCC + SOUTH + SMSA + MS + UNION + FEM, data = data)

etable(list(reg_1,reg_2,reg_3,reg_4), export='./table_1.png')

suppressMessages(library(car))
suppressMessages(library(sandwich))

# separate male and female categories
data3 <- data2 %>% mutate(MALE = ifelse(FEM == 1, 0, 1))
reg_5 <- feols(LWAGE ~ -1 + ED + EXP + I(EXP^2) + WKS + OCC + SOUTH + SMSA
+ MS + UNION + FEM + MALE, data = data3)

# now with intercept and different (but equivalent) hypothesis test
reg_6 <- feols(LWAGE ~ 1 + ED + EXP + I(EXP^2)  + WKS + OCC + SOUTH + SMSA
+ MS  + UNION + FEM, data = data3)


print(linearHypothesis(reg_5, c("FEM = MALE"), 
      vcoev = vcovHC(reg_5, type = "HC1")))
print(linearHypothesis(reg_6, c("FEM = 0"), 
      vcov = vcovHC(reg_6, type = "HC1")))



library(marginaleffects)
avg_slopes(reg_2, variables = "EXP", vcov = "HC1")

# don't use the I() to construct interactions
reg_wrong <- feols(LWAGE ~ -1 + i(ED_LEVEL) + EXP + EXP2 + WKS + OCC +
                     SOUTH + SMSA + MS + UNION + FEM, 
                   data = data %>% mutate(EXP2 = EXP^2))
avg_slopes(reg_wrong, variables = "EXP", vcov = "HC1")



# quick bootstrap comparison
n <- dim(data)[1]
results <- rep(0,1000)
for (i in 1:1000){
  my_weights <-  rmultinom(1,n,rep(1/n,n))/n
  my_reg <- feols(LWAGE ~ -1 + i(ED_LEVEL) + EXP + I(EXP^2) + WKS + OCC +
                    SOUTH + SMSA + MS + UNION + FEM, 
                  data = data, 
                  # multiplier bootstrap
                  weights=my_weights)
  results[i]<- my_reg$coefficients[7] + 
    sum(2 * my_reg$coefficients[8] * my_weights *data$EXP)
}
print(mean(results))
# [1] 0.01342801
print(quantile(results,c(0.025,.975)))
#2.5%      97.5% 
#   0.01219617 0.01463365 
hist(results,20)

feols(LWAGE ~ -1 + i(ED_LEVEL) +  i(FEM,poly(EXP,2)) + EXP  + WKS + OCC +
        SOUTH + SMSA + MS + UNION, 
      data = data)
      

reg_7 <- feols(LWAGE ~ ED + EXP + I(EXP^2) + WKS + OCC + SOUTH + SMSA
+ MS + UNION + FEM, data %>% filter(LWAGE>=6))
summary(reg_7)

table(data$ED)
low_wage_data <- subset(data, LWAGE<6)
table(low_wage_data$ED)


noise <- sample(-1:1,dim(data)[1],replace=T)

reg_8 <- feols(LWAGE ~ ED_NOISY + EXP + I(EXP^2) + WKS + OCC + SOUTH + SMSA
+ MS + FEM + UNION, data = data %>% mutate(ED_NOISY = + noise))
summary(reg_8)

etable(list(reg_1,reg_8), export='./table_noise.png')



data <- cbind(data, EDFEM=data$ED*data$FEM)
reg_9 <- lm(LWAGE ~ ED + EDFEM + EXP + EXP2 + WKS + OCC + SOUTH + SMSA
+ MS + FEM + UNION, data = data)
summary(reg_9)


reg_10 <- lm(LWAGE ~ ED + ED:FEM + EXP + EXP2 + WKS + OCC + SOUTH + SMSA
+ MS + FEM + UNION, data = data)
summary(reg_10)


reg_11 <- lm(LWAGE ~ ED*FEM + EXP + EXP2 + WKS + OCC + SOUTH + SMSA
+ MS  + UNION, data = data)
summary(reg_11)




## Correlation Exercise
# Set the seed for reproducibility
set.seed(12345)

# Number of observations
n <- 200
# Generate random data
# Random integer values between 18 and 65
AGE <- sample(22:65, n, replace = TRUE)
# same as age-22 but one less for some observations
EXP <- AGE - 22 - rbinom(n=n,size=1,prob=0.4)
EXP[EXP < 0] <- 0 # replace negative values with 0
LWAGE = 2.5 + .02*AGE + .03*EXP + .5*rnorm(n)
# create data frame
df <- data.frame(LWAGE,AGE,EXP)

# estimate OLS
reg <- feols(LWAGE ~ AGE + EXP, data = df) 
summary(reg)
linearHypothesis(reg,c("AGE+EXP=0"), 
                 vcov = vcovHC(reg, type = "HC1"))


