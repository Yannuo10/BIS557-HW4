#' @title glm_new() function
#' @description another way to build a generalized linear model using only gradient information
#' @param X a data matrix used for the function
#' @param y a vector of response values
#' @param itr number of iteration
#' @param step learning rate for gradient descent
#' @param tolerance allowed tolerance for two residuals
#' @param mu_fun expectation function of the distribution
#' @param var_fun variance function of the distribution
#' @param method constant step size or momentum update
#' @param friction a number to control the velocity and prevents overshooting
#' @examples
#'  n <- 5000; p <- 3; beta <- c(-1, 0.2, 0.1); X <- cbind(1, matrix(rnorm(n * (p- 1)), ncol = p - 1))
#'  eta <- X %*% beta; lambda <- exp(eta); y <- rpois(n, lambda = lambda)
#'    fit_linear_model <- glm_new(X, y, mu_fun = function(eta) exp(eta), var_fun = identity, step = 0.0001, method = "constant")
#' @export

glm_new <- function(X, y, mu_fun, var_fun, itr = 50, step, method, friction, tolerance = 1e-10){

  beta <- rep(0, ncol(X))
  #constant step size
  if (method == "constant"){
  for (i in seq_len(itr)){
    beta.old <- beta
    eta <- X %*% beta
    mu <- mu_fun(eta)
    W <- as.numeric(var_fun(mu))
    grad <- t(X) %*% (y - mu)
    #update betas using the gradient information
    beta <- beta + step * grad
    #stop iterating if the difference is small enough
    if(sqrt(crossprod(beta - beta.old)) < tolerance) break
  }
    }
  #adaptive step size (Momentum)
  else if (method == "momentum") {
    v <- rep(0, ncol(X))
    for (i in seq_len(itr)){
      beta.old <- beta
      eta <- X %*% beta
      mu <- mu_fun(eta)
      W <- as.numeric(var_fun(mu))
      grad <- t(X) %*% (y - mu)
      #create a velocity to update betas
      v <- friction * v + step * grad
      beta <- beta + v
      if(sqrt(crossprod(beta - beta.old)) < tolerance) break
    }
  }
  else {stop("methods should be either constant or momentum.")}

  ret <- list(coefficients = beta)
  class(ret) <- "my_glm"
  ret
}



