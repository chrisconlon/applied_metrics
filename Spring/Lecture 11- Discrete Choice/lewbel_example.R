
	# Let's generate some data
	nrep <- 100000
	x <- rep(c(-1.8,-0.9,-0.92,-2.1,-1.92,10),nrep)
	t <- rep(c(0,0,0,1,1,1),nrep)

	# individual treatment effects are zero or 1 so that ATE =1/6
	te <- rep(c(0,0,0,0,1,0),nrep)
	u <- rnorm(length(x),0,1)

	# Let's simulate a probit
	y <- (1 + x + te * t  + u > 0)*1.0

	# Fit some models
	lpm<-lm(y ~ x + t)
	myprobit <- glm(y ~ x+t, family = binomial(link = "probit"))
	mylogit <- glm(y ~ x+t, family = binomial(link = "logit"))
	
	# relative coefficients (LPM gets -1 , logit/probit give 0.6)
	mfx_lpm <- lpm$coefficients[3]/lpm$coefficients[2]
	mfx_logit <- mylogit$coefficients[3]/mylogit$coefficients[2]
	mfx_probit <- myprobit$coefficients[3]/myprobit$coefficients[2]
	
	# That didn't go well...
	summary(lpm)