library(ggplot2)    

# set the seed to make the results reproducible.
set.seed(3)

#### simulate some data ####
# epsilon = random error term
epsilon <- 0.25*rnorm(100)
x       <- seq(from=1, to=5, length.out=100)
y       <- 1 +sin(x*2)/3 + epsilon

# visualize the data (with a polynomial best-fit line)
ggplot(data=NULL,aes(x, y)) + geom_point() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 3,raw=TRUE),color='maroon')+
  geom_smooth(method = "lm", formula = y ~ poly(x, 5,raw=TRUE),color='navy')+
  geom_smooth(method = "lm", formula = y ~ poly(x, 17,raw=FALSE),color='darkgreen')
  
ggsave("~/Documents/applied_metrics/Lecture 5- Intro to Nonparametrics/resources/poly.pdf", device = "pdf")

summary(lm(y~poly(x,6,raw=TRUE)))
summary(lm(y~poly(x,7,raw=TRUE)))


summary(lm(y~poly(x,6,raw=FALSE)))
summary(lm(y~poly(x,7,raw=FALSE)))


# for splines

ggplot(data=NULL,aes(x, y)) + geom_point() + 
  geom_smooth(method = "lm", formula = y ~ bs(x,knots=3) ,color='maroon')+
  geom_smooth(method = "lm", formula = y ~ bs(x,knots=c(2,4)) ,color='darkgreen')+
  geom_smooth(method = "lm", formula = y ~ poly(x, 3),color='navy')

ggsave("~/Documents/applied_metrics/Lecture 5- Intro to Nonparametrics/resources/spline.pdf", device = "pdf")

library(mgcv)
ggplot(data=NULL,aes(x, y)) + geom_point() + 
  geom_smooth(method = "lm", formula = y ~ bs(x,knots=3) ,color='maroon')+
  stat_smooth(method = gam, formula = y ~ s(x),color='navy')


test.epsilon <- 0.25*rnorm(100)
test.x       <- seq(from=1, to=5, length.out=100)
test.y       <- 1 +sin(x*2)/3 + test.epsilon
test.data <- data.frame("x"=test.x,"y"=test.y )

# Make predictions
model1 <- gam(y ~ s(x))
predictions1 <- model1 %>% predict(test.data)
sqrt(sum((predictions1-test.data$y)**2))

model2 <- lm(y ~ poly(x,3))
predictions2 <- model2 %>% predict(test.data)
sqrt(sum((predictions2-test.data$y)**2))

model3 <- lm(y ~ poly(x,7))
predictions3 <- model3 %>% predict(test.data)
sqrt(sum((predictions3-test.data$y)**2))

model4 <- lm(y ~ bs(x,knots=c(2,4)))
predictions4 <- model4 %>% predict(test.data)
sqrt(sum((predictions4-test.data$y)**2))

model5 <- lm(y ~ bs(x,knots=c(3)))
predictions5 <- model5 %>% predict(test.data)
sqrt(sum((predictions5-test.data$y)**2))


