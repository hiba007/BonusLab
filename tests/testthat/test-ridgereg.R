library(testthat)
library(MASS)

test_that("ridge coefficients", {
  data(iris)

  lambda <- 1
  formula <- Petal.Length ~ Sepal.Length + Sepal.Width + Petal.Width


  my_model <- ridgereg$new(formula, iris, lambda)
  my_coef <- my_model$coef()

  mass_model <- lm.ridge(formula, data = iris, lambda = lambda)
  mass_coef <- coef(mass_model)

  expect_equal(my_coef, mass_coef, tolerance = 1e-3)

})
