#' @title lm_ofc() function
#' @description to build a linear regression model that reads in contiguous rows from a data frame
#' @param form a formula;
#' @param df a data frame used for the function;
#' @param contrasts a list of contrasts for factor variables
#' @examples
#' data(iris)
#' library(reticulate)
#' fit_linear_model <- lm_ofc(Sepal.Length ~ ., iris, contrasts = list(Species = "contr.sum"))
#' @export



lm_ofc <- function(form, df, contrasts = NULL){
  np <- import("numpy", as = "np", convert = FALSE)

  df_no_na <- model.frame(form, df)
  if(is.null(contrasts)){
    X<-model.matrix(form, df)}
  else(X<-model.matrix(form, df, contrasts.arg=contrasts))
  yname <- as.character(form)[2]
  y <- matrix(df_no_na[, yname], ncol = 1)

  #qr decomposition as a less computationally expensive (out-of-core) way to fit the model
  qr <- r_to_py(np$linalg$qr(X))
  q <- qr[[0]]
  r <- qr[[1]]
  #solution for beta hat
  beta <- py_to_r(np$linalg$inv(r)$dot(q$T)$dot(y))

  ret <- list(coefficients = beta)
  class(ret) <- "my_lm"
  ret
}

