library(testthat)
library(reticulate)
context("Test the output of lasso().")



test_that("You lasso() function works in an easy case.", {

  data(iris)

  form <- Sepal.Length ~ .; df <- iris

  df_no_na <- model.frame(form, df)

  X0 <- qr.Q(qr(model.matrix(form, df_no_na)))

  yname <- as.character(form)[2]

  y0 <- matrix(df_no_na[,yname],ncol = 1)

  fit_casl <- casl_lenet(X0, y0, lambda = 0.01, maxit = 1e5)

  fit_py <- lasso(Sepal.Length  ~ ., iris, lambda = 0.01)

  expect_equivalent(as.vector(fit_casl), as.vector(fit_py$coefficients),
                    tolerance = 1e-4)
})



test_that("You lasso() function works with contrasts.", {

  data(iris)

  form <- Sepal.Length ~ .; df <- iris

  df_no_na <- model.frame(form, df)

  X0 <- qr.Q(qr(model.matrix(form, df_no_na, contrasts.arg = list(Species = "contr.sum"))))

  yname <- as.character(form)[2]

  y0 <- matrix(df_no_na[,yname],ncol = 1)

  fit_casl <- casl_lenet(X0, y0, lambda = 0.01, maxit = 1e5)

  fit_py <- lasso(Sepal.Length  ~ ., iris, lambda = 0.01, contrasts = list(Species = "contr.sum"))

  expect_equivalent(as.vector(fit_casl), as.vector(fit_py$coefficients),
                    tolerance = 1e-4)
})

