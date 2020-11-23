library(testthat)
library(reticulate)
context("Test the output of ridge_py().")

test_that("You ridge() function works in an easy case.", {

  data(iris)

  fit_r <- ridge(Sepal.Length ~ .- Species, iris, lambda = 1)

  fit_py <- ridge_py(Sepal.Length  ~ .- Species, iris, lambda = 1)

  expect_equivalent(fit_r$coefficients, fit_py$coefficients,
                    tolerance = 0.01)
})

test_that("Your ridge() function works with contrasts.", {

  data(iris)

  fit_r <- ridge(Sepal.Length ~ .- Species, iris,
                 contrasts = list(Species = "contr.sum"), lambda = 0.5)

  fit_py <- ridge_py(Sepal.Length  ~ .- Species, iris, contrasts = list(Species = "contr.sum"), lambda = 0.5)

  expect_equivalent(fit_r$coefficients, fit_py$coefficients,
                    tolerance = 0.01)
})

