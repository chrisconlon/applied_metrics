
# two-sided test (approx normal)
p0 <- 0.02
lift <- 0.10
p1 <- p0*(1+lift)
alpha <- 0.05
power <- 0.80
z_alpha <- qnorm(1-alpha/2)
z_beta  <- qnorm(power)

n_per_arm <- ((z_alpha + z_beta)^2 * (p1*(1-p1) + p0*(1-p0))) /
  ((p1 - p0)^2)

ceiling(n_per_arm)
# Alternatively: use power.prop.test for approximation (one-sided/two-sided differences)
power.prop.test(p1 = p0, p2 = p1, power = power, sig.level = alpha,
                alternative = "two.sided")

# now make lift 5x larger
power.prop.test(p1 = p0, p2 = p0*(1+0.5), power = power, sig.level = alpha,
                alternative = "two.sided")

# Solve the continuous case
sigma <- 10.0        # estimated SD of outcome --> note we don't need for binary
delta <- 1.0         # desired absolute lift (mu1 - mu0)
alpha <- 0.05
power <- 0.80
z_alpha <- qnorm(1-alpha/2)
z_beta  <- qnorm(power)

n_per_arm <- (2 * sigma^2 * (z_alpha + z_beta)^2) / (delta^2)
ceiling(n_per_arm)

# or use built-in
power.t.test(delta = delta, sd = sigma, power = power,
             sig.level = alpha, type = "two.sample", alternative = "two.sided")


## now make a graph
p0 <- 0.02
alpha <- 0.05
power <- 0.8
z_alpha <- qnorm(1-alpha/2)
z_beta <- qnorm(power)

lift_grid <- seq(0.05, 1.0, by=0.05)
df <- data.frame(
  lift = lift_grid,
  p1 = p0 * (1 + lift_grid)
)
df$n <- ((z_alpha + z_beta)^2 * (df$p1*(1-df$p1) + p0*(1-p0))) / (df$p1 - p0)^2

ggplot(df, aes(x = lift*100, y = n)) +
  geom_line(linewidth=1.2) +
  geom_point() +
  scale_y_log10(labels = scales::comma) +
  labs(
    x = "Relative Lift (%)",
    y = "Required Sample Size per Arm (log scale)",
    title = "Sample Size vs. Lift (Baseline p0 = 2%)",
    subtitle = "α = 0.05, Power = 0.80"
  ) +
  theme_minimal(base_size = 14)

