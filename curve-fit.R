x <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150)
y <- c(0, 7, 13.5, 18, 23, 26.5, 29.5, 32, 34.5, 36.2, 38, 39, 40, 41, 41.5, 42.2)

plot(x, y)


model <- nls(y ~ K * N / ((K - N) * exp(-r * x) + N), start = c(K = 50, N = 1, r = 0.05))
coef <- summary(model)$coefficients
K <- coef[1]
N <- coef[2]
r <- coef[3]


f <- function(x) K * N / ((K - N) * exp(-r * x) + N)
curve(f, add = TRUE, col = "blue")

# derived
t <- seq(0, 140, by = 1)

lines(t, f(t) * r * (1 - f(t) / K), type = "l", col = "blue")

# plot cumsumWha
lines(t, f(0) + cumsum(f(t) * r * (1 - f(t) / K)), type = "l", col = "green")
