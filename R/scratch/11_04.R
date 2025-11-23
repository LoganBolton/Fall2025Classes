################################################################################
# Linear Regression via Optimization + Bootstrap Covariance
# ------------------------------------------------------------------------------
# This script:
#  1) Defines a least-squares loss function for linear regression.
#  2) Simulates data with a sparse-ish true β.
#  3) Implements my_lm(), which estimates β using optim() and
#     uses a nonparametric bootstrap to estimate Cov(β̂).
#  4) Compares against R's lm() for reference.
#
# Important notes:
#  - We do NOT add an intercept automatically. X should include it if desired.
#  - optim() minimizes the provided loss; we pass y and X via fn=ls_loss(...).
#  - The bootstrap here resamples rows (pairs bootstrap) to approximate
#    the sampling distribution of β̂, then computes the sample covariance.
################################################################################

# ------------------------------------------------------------------------------
# 1) Least-squares loss function: Sum_i (y_i - x_i^T beta)^2
#    Arguments:
#      beta : numeric vector (length p) of coefficients
#      X    : n x p design matrix (NO intercept added automatically)
#      y    : length-n response vector
#    Returns:
#      scalar: sum of squared residuals
# ------------------------------------------------------------------------------
ls_loss <- function(beta, X, y) {
  sum((y - X %*% beta)^2)
}

# ------------------------------------------------------------------------------
# 2) Simulate data for demonstration
#    - n observations, p predictors with standard normal entries
#    - A sparse-ish true β for interpretability
#    - y = Xβ + ε, with ε ~ N(0, 1)
# ------------------------------------------------------------------------------
set.seed(1)
n <- 120
p <- 8

# Design matrix with p predictors (standard normal entries)
X <- matrix(rnorm(n * p), n, p)

# True coefficients (sparse-ish pattern)
beta <- c(2, -1.5, 0, 0, 1, rep(0, p - 5))

# Response with Gaussian noise (sd = 1)
y <- X %*% beta + rnorm(n, sd = 1)

# Optional: give columns names and make a convenient data frame for inspection
colnames(X) <- paste0("x", 1:p)
df <- as.data.frame(cbind(y, X))

# ------------------------------------------------------------------------------
# 3) Custom linear model via optimization + bootstrap covariance
#    my_lm(y, X, B)
#    - Fits β̂ by minimizing ls_loss with optim() (starting at zeros).
#    - Uses nonparametric bootstrap (resample rows of (y, X)) to get B replicates
#      of β̂^(b), then estimates Cov(β̂) as the sample covariance of those B rows.
#
#    Inputs:
#      y : numeric vector length n
#      X : numeric matrix n x p (no intercept added inside!)
#      B : number of bootstrap replicates (default 100)
#
#    Returns:
#      list with:
#        $est_beta : numeric vector of length p, the optim-based estimate β̂
#        $cov_beta : p x p bootstrap covariance matrix of β̂
# ------------------------------------------------------------------------------
my_lm <- function(y, X, B = 100) {
  
  # Derive problem dimensions from the design matrix
  n <- nrow(X)
  p <- ncol(X)
  
  # Fit β̂ on the original data using optim()
  # beta_hat$par will contain the estimated coefficients
  beta_hat <- optim(rep(0, p), fn = ls_loss, y = y, X = X)$par
  
  # Storage for bootstrap β̂^(b)
  boot_beta <- matrix(NA, B, p)
  df <- cbind(y, X)
  
  # Initialize a bootstrap loop (B replicates)
  for (b in 1:B) {
    
    # Bootstrap step (pairs bootstrap):
    #  - Sample row indices 1:n with replacement
    #  - Refit model on the resampled data (X_boot, y_boot)
    #  - Store β̂^(b) in boot_beta[b, ]
    new_data <- df[sample(1:n, n, replace = T) ,]
    boot_beta[b,] <- optim(rep(0, p), fn = ls_loss, y = new_data[, 1], X = as.matrix(new_data[, -1]))$par
    
  }
  
  # Estimate Cov(β̂) as the sample covariance of bootstrap estimates
  cov_beta <- cov(boot_beta)
  
  # Return both the point estimate and its bootstrap covariance
  return(list("est_beta" = beta_hat, "cov_beta" = cov_beta))
  
}

# ------------------------------------------------------------------------------
# 4) Fit with custom function and compare with lm()
# ------------------------------------------------------------------------------
fit <- my_lm(y, X)

# Reference fit using R's lm(); note V1 is the column name for y in df
fit_lm <- lm(V1 ~ ., data = df)
summary(fit_lm)

# Standard diagnostic plots for lm() (Residuals, QQ-plot, etc.)
plot(fit_lm)
