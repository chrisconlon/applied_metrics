

clt_function <- function(n_sample,p){
  # create an empty vector for the means binomial samples
  clt <- NULL
  n <- n_sample   # sample size for each simulation
  # take the mean of samples of the uniform distribution. repeat 1,000 times
  for (i in 1:1000) {
    clt <-  c(clt, rbinom(1000,n_sample,p)/n_sample)
  }
  theoretical_mean <- p
  hist(clt, xlim=c(0, 1), xlab='Sample Mean', main=paste0("Histogram of Sample Means (n=", eval(n_sample),")"), col='beige')
  abline(v=mean(clt), lwd=3, col='green')
  abline(v=theoretical_mean, lwd=3, col='red')
  legend(c("Sample", "Theoretical"),x='topright', lwd=c(3,3), col=c('green', 'red'))
}

clt_function(10,.4)
clt_function(20,.4)
clt_function(100,.4)
clt_function(1000,.4)


clt_function_scaled <- function(n_sample,p){
  # create an empty vector for the means binomial samples
  clt <- NULL
  n <- n_sample   # sample size for each simulation
  # take the mean of samples of the uniform distribution. repeat 1,000 times
  for (i in 1:1000) {
    clt <-  c(clt, (rbinom(1000,n_sample,p)/n_sample-p)*sqrt(n_sample))
  }
  theoretical_mean <- 0
  hist(clt, xlim=c(-3, 3), xlab='Sample Mean', main=paste0("Histogram of Rescaled Sample Means (n=", eval(n_sample),")"), col='beige')
  abline(v=mean(clt), lwd=3, col='green')
  abline(v=theoretical_mean, lwd=3, col='red')
  legend(c("Sample", "Theoretical"),x='topright', lwd=c(3,3), col=c('green', 'red'))
}

clt_function_scaled(10,.4)
clt_function_scaled(20,.4)
clt_function_scaled(100,.4)
clt_function_scaled(1000,.4)
