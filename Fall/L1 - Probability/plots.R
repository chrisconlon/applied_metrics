
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


# Inverse-CDF sampling picture (slide: "how a computer draws a random variable")
u <- 0.8; xq <- qnorm(u)
pdf("inverse-cdf.pdf", width = 4.2, height = 3.6, pointsize = 11)
par(mar = c(4, 4, 1, 1))
curve(pnorm(x), from = -3.5, to = 3.5, lwd = 2, xlab = "x", ylab = "F(x)")
segments(-3.5, u, xq, u, col = "#bb0000", lwd = 2, lty = 2)
segments(xq, u, xq, 0, col = "#bb0000", lwd = 2, lty = 2)
points(xq, u, pch = 16, col = "#bb0000")
text(-3.3, u, expression(u == 0.8), pos = 3, col = "#bb0000")
text(xq, 0.03, expression(F^-1 * (u) == 0.84), pos = 4, col = "#bb0000")
dev.off()
