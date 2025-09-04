

# first, the Cornwell and Rupert regression
suppressMessages(library(tidyverse))
setwd('~/Dropbox/Econometrics-PhD-me/L3 - Linear Regression')
data <- read.csv('cornwell-rupert.csv')


data <- cbind(data, EXP2=data$EXP^2)

reg_1 <- lm(LWAGE ~ ED + EXP + EXP2 + WKS + OCC + SOUTH + SMSA
+ MS + FEM + UNION, data = data)
summary(reg_1)
