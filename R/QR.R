# QR decomposition for lm

linreg1 <- function(formula, data) {
  # get the independent variables into a n*p matrix 
  X <- model.matrix(formula, data = data)
  # extract all the variable names from the formula
  vars <- all.vars(formula)
  if(is.numeric(data[vars[1]]) = FALSE){
    stop("Dependent variable is not numeric")
  }
  # get the first variables as independent
  Y <- as.matrix(data[, vars[1]])
  # use function qr to decompose X'X 
  QR <- qr(X)
  #Use the function solve 
  beta <- solve(QR, Y)
  # Get etimated y
  y_hat <- X %*% beta
  # subtract y from y_hat 
  res <- as.vector(y - y_hat)
  # Standard errors of the residuals
  se_sq <- sum(res^2)/ (nrow(X) - qr.X$rank)
  #Using cholesky inversion
  beta_var <- chol2inv(qr.X$qr) * se_sq
}

linreg1(formula = Petal.Length ~ Sepal.Length + Sepal.Width, data = iris)
