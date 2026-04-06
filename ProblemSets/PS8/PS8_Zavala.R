# PS8 - Emilian Zavala

# Step 4
set.seed(100)
X <- matrix(rnorm(100000 * 10), nrow = 100000, ncol = 10)
X[, 1] <- 1
eps <- rnorm(100000, mean = 0, sd = 0.5)
beta <- c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)
Y <- X %*% beta + eps

# Step 5 - OLS closed form
beta_OLS <- solve(t(X) %*% X) %*% t(X) %*% Y
print(beta_OLS)

# Step 6 - Gradient descent
beta_gd <- rep(0, 10)
lr <- 0.0000003
iterations <- 100000
for (i in 1:iterations) {
  gradient <- -2 * t(X) %*% (Y - X %*% beta_gd)
  beta_gd <- beta_gd - lr * gradient
}
print(beta_gd)

# Step 7 - nloptr
library(nloptr)
obj_fn <- function(b) sum((Y - X %*% b)^2)
grad_fn <- function(b) -2 * t(X) %*% (Y - X %*% b)
result_lbfgs <- lbfgs(x0 = rep(0,10), fn = obj_fn, gr = grad_fn)
print(result_lbfgs$par)
result_nm <- neldermead(x0 = rep(0,10), fn = obj_fn)
print(result_nm$par)

# Step 9 - lm
result_lm <- lm(Y ~ X - 1)
library(stargazer)
stargazer(result_lm, out = "PS8_Zavala.tex")
