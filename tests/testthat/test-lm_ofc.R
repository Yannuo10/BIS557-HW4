library(testthat)
library(reticulate)
context("Test the output of lm_ofc().")


test_that("You lm_ofc() function works in an easy case.", {

  data(iris)

  fit_linear_model <- lm_ofc(Sepal.Length ~ ., iris)

  fit_lm <- lm(Sepal.Length  ~ ., iris)

  expect_equivalent(fit_lm$coefficients, fit_linear_model$coefficients,
                    tolerance = 0.5)
})

test_that("You lm_ofc() function works with contrasts.", {

  data(iris)

  fit_linear_model <- lm_ofc(Sepal.Length ~ ., iris,
                             contrasts = list(Species = "contr.sum"))

  fit_lm <- lm(Sepal.Length  ~ ., iris, contrasts = list(Species = "contr.sum"))

  expect_equivalent(fit_lm$coefficients, fit_linear_model$coefficients,
                    tolerance = 0.5)
})



