library(tidyverse);library(fixest)
set.seed(10)

# Create data with 10 groups and 10 time periods
df <- crossing(id = 1:10, t = 1:10) %>%
  # Add an event in period 6 with a one-period positive effect
  mutate(Y = rnorm(n()) + 1*(t == 6))

# Use i() in feols to include time dummies,
# specifying that we want to drop t = 5 as the reference
m <- feols(Y ~ i(t, ref = 5), data = df,
           cluster = 'id')

# Plot the results, except for the Contant,# and add a line joining 
# them and a space and line for the reference group
coefplot(m, drop = '(Constant)',
         pt.join = TRUE, ref = c('t:5' = 6), ref.line = TRUE)
