#' @title multi_class() function
#' @description one way to implement a classification model generalizing logistic regression to accommodate more than two classes
#' @param X a data matrix used for the function
#' @param y a vector of response values
#' @param itr number of iteration
#' @param tolerance allowed tolerance for two residuals
#' @examples
#' data(iris); X <- cbind(1, as.matrix(iris[, -5]))
#' y <- ifelse(iris$Species==levels(iris$Species)[1], 1, ifelse(iris$Species==levels(iris$Species)[2], 2, 3))
#' fit_linear_model <- multi_class(X, y, itr = 50, tolerance = 1e-10)
#' @export



multi_class <- function(X, y, itr, tolerance){
  K <- length(unique(y))
  y <- as.factor(y)
  #generate matrices to store probabilities and betas
  prob <- matrix(0, nrow = K, ncol = nrow(X))
  beta <- matrix(0, nrow = K, ncol = ncol(X))

  for (i in 1:K){
    #convert y into binary forms
    y_new <- ifelse(y == levels(y)[i], 1, 0)
    beta.sub <- beta[i, ]
    #calculate betas using the Newton-Ralphson method
    for (j in seq_len(itr)){
      b_old <- beta.sub
      p <- 1 / (1 + exp(- X %*% beta.sub))
      W <- as.numeric(p * (1 - p))
      XtX <- crossprod(X, diag(W) %*% X)
      score <- t(X) %*% (y_new - p)
      delta <- solve(XtX, score)
      beta.sub <- beta.sub + delta
      if(sqrt(crossprod(beta.sub - b_old)) < tolerance) break
    }
    #probabilities for each class
    prob[i, ] <- p
    beta[i, ] <-beta.sub
  }
  #choose the one with the largest probability
  best.k <- apply(prob, 2, which.max)
  beta.final <- unique(beta[best.k, ])
  ret <- list(coefficients = beta.final, class = levels(y)[best.k])
  class(ret) <- "my_glm"
  ret
}
