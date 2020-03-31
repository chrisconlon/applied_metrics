library("cobalt")
data("lalonde", package = "cobalt") 
covs0 <- subset(lalonde, select = -c(treat, re78, nodegree, married))
tab<-bal.tab(covs0, treat = lalonde$treat)

# output
print(tab)
love.plot(tab, binary = "std", threshold = .1)
bal.plot(covs0, treat = lalonde$treat, var.name='age')
bal.plot(covs0, treat = lalonde$treat, var.name='educ')
bal.plot(covs0, treat = lalonde$treat, var.name='race')