
if(!(require(ggplot2))){install.packages('ggplot2')}
ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
stat_function(fun = dnorm) +
ylab("pdf") +
ggtitle("Standard Normal PDF")

if(!(require(ggplot2))){install.packages('ggplot2')}
ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
stat_function(fun = pnorm) +
ylab("cdf") +
ggtitle("Standard Normal CDF")


if(!(require(ggplot2))){install.packages('ggplot2')}
ggplot(data.frame(x = c(-1, 4)), aes(x = x)) +
stat_function(fun = dexp) +
ylab("pdf") +
ggtitle("Exponential PDF")

if(!(require(ggplot2))){install.packages('ggplot2')}
ggplot(data.frame(x = c(-1, 4)), aes(x = x)) +
stat_function(fun = pexp) +
ylab("cdf") +
ggtitle("Exponential CDF")



sfun0  <- stepfun(0:1, c(0., .3, 1.), f = 0)
x = seq(-.1, 1.1, length.out = 100)
df = data.frame(x = x, y = sfun0(x))
ggplot(df, aes(x,y)) + geom_step() +
ylab("cdf")  +
ggtitle("Bernoulli CDF")
