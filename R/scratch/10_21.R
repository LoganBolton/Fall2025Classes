############################################################
# Demo: Random interval search (golden-section inspired)
#       + Intro to functions using an L2 loss example
#
# NOTE: Code, order, variable names, and logic preserved.
#       Only formatting and comments were added.
############################################################

#-----------------------------------------------------------
# 1) DATA
#    Generate a small sample from N(mean = 2, sd = 1).
#-----------------------------------------------------------
x <- rnorm(50, mean = 2)

#-----------------------------------------------------------
# 2) INITIALIZATION
#    We want to minimize f(mu) = sum_i (x_i - mu)^2 over mu.
#    Start from a bracket [x1, x3] given by the sample min/max,
#    and an interior point x2 (chosen uniformly in the bracket).
#-----------------------------------------------------------
x1 <- min(x)     # left endpoint / initial guess
x3 <- max(x)     # right endpoint (by definition >= x1)

# Evaluate loss at endpoints (not used in loop logic, but informative)
f1 <- sum((x - x1)^2)
f3 <- sum((x - x3)^2)

# Stopping rule will be based on improvement between iterations
tol  <- 10^(-6)
diff <- tol + 1   # initialize larger than tol to enter the loop

iter  <- 1        # iteration counter
maxit <- 100      # safety cap to avoid infinite loops

# Current interior point and its loss
x2 <- runif(1, min = min(x), max = max(x))
f2 <- sum((x - x2)^2)

#-----------------------------------------------------------
# 3) MAIN LOOP
#    At each iteration:
#      - Compare subinterval lengths a = (x2 - x1) and b = (x3 - x2)
#      - Sample a new candidate x4 uniformly in the LARGER subinterval
#      - Evaluate its loss f4 and update the bracket accordingly
#      - Track improvement in objective via 'diff'
#-----------------------------------------------------------
while (diff > tol & iter <= maxit) {
  
  # Current subinterval lengths
  a <- x2 - x1
  b <- x3 - x2
  
  if(a > b) {
    # Left side [x1, x2] is larger -> explore here
    x4 <- runif(1, min = x1, max = x2)
    f4 <- sum((x - x4)^2)
    
    if(f4 > f2) {
      # New point worse -> move left endpoint up (shrink left side)
      x1 <- x4
    } else {
      # New point better -> shift bracket: old x2 becomes right end; x2 <- x4
      x3 <- x2
      x2 <- x4
    }
    
  } else {
    # Right side [x2, x3] is larger -> explore here
    x4 <- runif(1, min = x2, max = x3)
    f4 <- sum((x - x4)^2)
    
    if(f4 > f2) {
      # New point worse -> move right endpoint down (shrink right side)
      x3 <- x4
    } else {
      # New point better -> shift bracket: old x2 becomes left end; x2 <- x4
      x1 <- x2
      x2 <- x4
    }
    
  }
  
  # Track improvement in objective value (positive if improved)
  f2_old <- f2
  f2 <- sum((x - x2)^2)
  diff <- f2_old - f2
  
  # Increment iteration counter
  iter <- iter + 1
}

# Final estimate for the minimizer mu (numerical)
x2


#-----------------------------------------------------------
# 4) INTRO TO FUNCTIONS IN R
#    We'll use the same L2 loss f(mu) = sum((obs - mu)^2).
#    Three versions show different return styles.
#-----------------------------------------------------------

#-----------------------------------------------------------
# Version 1: Minimal function (implicit return)
#            The value of the last expression is the return.
#-----------------------------------------------------------
mean_loss <- function(obs, mu) {
  
  sum((obs - mu)^2)
  
}

# Evaluate at: numerical minimizer from above (x2), the sample mean, and mu = 2
mean_loss(obs = x, mu = x2)  
mean_loss(obs = x, mu = mean(x))  
mean_loss(obs = x, mu = 2)


#-----------------------------------------------------------
# Version 2: Assign to a name (still implicit return)
#            The assignment expression evaluates to the assigned value,
#            which becomes the function's return value.
#-----------------------------------------------------------
mean_loss <- function(obs, mu) {
  
  l2_loss <- sum((obs - mu)^2)
  
}

# Same evaluations as above
mean_loss(obs = x, mu = x2)  
mean_loss(obs = x, mu = mean(x))  
mean_loss(obs = x, mu = 2) 


#-----------------------------------------------------------
# Version 3: Explicit return of a list (richer output)
#            Useful when returning multiple items.
#-----------------------------------------------------------
mean_loss <- function(obs, mu) {
  
  l2_loss <- sum((obs - mu)^2)
  
  return(list("Sample" = obs, "Value" = mu, "Loss" = l2_loss))
  
}

# Same evaluations as above (now returns a list with fields Sample/Value/Loss)
mean_loss(obs = x, mu = x2)  
mean_loss(obs = x, mu = mean(x))  
mean_loss(obs = x, mu = 2) 
