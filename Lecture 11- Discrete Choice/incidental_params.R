# x = x.base + fe
# z = fe +  1 * x  + u
# y = logit(z)

fe.sd <- 1 # Specify the standard deviation of the fixed effed
x.sd  <- 1 # Specify the base standard deviation of x
nperson <- 5000 # Number of persons
nobs <- 5      # Number of observations per person
panels <- fabricate(
  individuals = add_level(N = nperson, id_fe = rnorm(N,0,fe.sd)),
  periods = add_level(N = nobs, nest = FALSE),
  obs = cross_levels(
    by = join(individuals, periods),
    # put the FE into X so there is something to de-mean
    x = id_fe + rnorm(N,0,x.sd),
    z = 1*id_fe  + 1*x, # + rnorm(N,0,1) -- adding this breaks bias correction
    logit_prob = exp(z)/(1+exp(z)),
    yl= logit_prob>runif(N)
  )
)

# ignore the FE
summary(glm(yl ~ x, data = panels, family = "binomial"))
# LSDV approach (very slow)
# summary(glm(yl ~ x+factor(id_fe), data = panels, family = "binomial"))
# Do the LSDV quickly
res<-bife(yl ~ x | id_fe, data = panels, 'logit')
summary(res)
summary(bias_corr(res))
# Chamberlain conditional approach
summary(clogit(yl~ x +strata(id_fe), data=panels))