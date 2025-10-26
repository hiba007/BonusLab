

#' @title Reference Class for Ridge Regression
#' @description Holds the data and methods for a ridge regression model.
#' @field formula The model formula.
#' @field beta_ridge The calculated ridge regression coefficients.
#' @field y_hat The fitted values (y_hat) for the training data.
#' @field mean_train The mean value of train dataset.
#' @field sd_train The standard deviation value of train dataset.
#' @field data_name The name of the input data frame.
#' @export ridgereg
#'
ridgereg <- setRefClass("ridgereg",
                        fields = list(
                          formula = "formula",
                          beta_ridge = "numeric",
                          y_hat = "matrix",
                          mean_train = "numeric",
                          sd_train = "numeric",
                          data_name = "character"
                        ),
                        methods = list(

                          #' @description
                          #' Initializes a `ridgereg` object by fitting a ridge regression model using
                          #' the provided formula, dataset, and regularization parameter `lambda`.
                          #' Standardization is applied to all covariates except the intercept, and the
                          #' ridge regression coefficients are computed using the closed-form solution:
                          #' \deqn{\hat{\beta}_{ridge} = (X^{T}X + \lambda I)^{-1}X^{T}y}
                          #'
                          #' @param formula A model formula of the form `y ~ x1 + x2 + ...`, specifying
                          #'   the response and predictor variables for ridge regression.
                          #' @param dataset A data frame containing the variables referenced in the formula.
                          #' @param lambda A non-negative numeric value indicating the ridge penalty.
                          #'   Larger values of `lambda` impose stronger shrinkage on the coefficients.
                          #'
                          #' @details
                          #' The method constructs the design matrix using `model.matrix()`, extracts the
                          #' response vector, standardizes the predictor variables (excluding the intercept),
                          #' and computes ridge regression coefficients. The fitted values, along with
                          #' scaling parameters, are stored in the object for later prediction.
                          #'
                          #' @return
                          #' The method does not return a value directly. Instead, it initializes the
                          #' fields of the reference class object, including:
                          #' \itemize{
                          #'   \item \code{beta_ridge}: Estimated ridge coefficients
                          #'   \item \code{y_hat}: Fitted values
                          #'   \item \code{mean_train}: Mean of each standardized predictor
                          #'   \item \code{sd_train}: Standard deviation of each standardized predictor
                          #'   \item \code{data_name}: Name of the dataset used
                          #' }
                          #'

                          initialize = function(formula, dataset, lambda)
                          {

                            X <- model.matrix(formula, dataset)[, -1, drop = FALSE]
                            y <- model.response(model.frame(formula, data = dataset))

                            data_name <<- deparse(substitute(dataset))


                            X_mean <- colMeans(X)
                            X_sd  <- apply(X, 2, sd)
                            X_norm <- scale(X, center = X_mean, scale = X_sd)

                            y_0 <- y - mean(y)

                            ident <- diag(ncol(X_norm))

                            inverseTerm <- solve((t(X_norm) %*% X_norm) + (lambda * ident))

                            beta_r <- inverseTerm %*% (t(X_norm) %*% y_0)

                            beta <- beta_r / X_sd
                            intercept <- mean(y) - sum(beta * X_mean)

                            beta_ridge <<- c(intercept, as.numeric(beta))
                            names(beta_ridge) <<- c("", colnames(X))


                            mean_train <<- X_mean
                            sd_train <<- X_sd
                            y_hat <<- X_norm %*% beta
                            formula <<- formula

                          },

                          #' @description
                          #' Displays a formatted summary of the fitted `ridgereg` model, similar to the
                          #' output of `print.lm()`. The printed output includes the model call and the
                          #' estimated ridge regression coefficients.
                          #'
                          #'
                          #' @examples
                          #' \dontrun{
                          #' data(iris)
                          #' model <- ridgereg$new(Sepal.Length ~ Sepal.Width + Petal.Length, iris, 1)
                          #' model$print()
                          #' }
                          #'
                          print = function() {

                            cat("Call:\n")
                            cat(class(.self)[1],"(formula = ",deparse(formula),", data = ",data_name,")\n",sep = "")

                            cat("\nCoefficients:\n")
                            print.default(.self$beta_ridge)
                          },

                          #' @description
                          #' Generates predictions from a fitted `ridgereg` model. If no `testdata` is
                          #' supplied, the method returns the fitted values from the training data. When
                          #' new data is provided, the function standardizes the covariates using the
                          #' training means and standard deviations before computing predictions.
                          #'
                          #' @param testdata Optional. A data frame containing the same predictor variables
                          #'   used in the model formula. If `NULL`, the fitted values from the training
                          #'   data are returned.
                          #'
                          #' @return
                          #' A numeric vector of predicted response values.
                          #'
                          #' @examples
                          #' \dontrun{
                          #' data(iris)
                          #' model <- ridgereg$new(Sepal.Length ~ Sepal.Width + Petal.Length, iris, 0.6)
                          #'
                          #' # fitted values
                          #' model$predict()
                          #'
                          #' }

                          predict = function(testdata = NULL) {

                            if(is.null(testdata))
                              return(as.numeric(y_hat))

                            data <- model.matrix(formula, testdata)

                            y_hat_test <- data %*% beta_ridge

                            return(as.numeric(y_hat_test))
                          },

                          #' @description
                          #' Extracts the ridge regression coefficients from the fitted `ridgereg` model.
                          #'
                          #' @return
                          #' A named numeric vector containing the coefficient estimates.
                          #'
                          #' @examples
                          #' \dontrun{
                          #' data(iris)
                          #' model <- ridgereg$new(Sepal.Length ~ Sepal.Width + Petal.Length, iris, 1)
                          #' model$coef()
                          #' }
                          #'
                          coef = function() {

                            return(.self$beta_ridge)
                          }
                        )

)
