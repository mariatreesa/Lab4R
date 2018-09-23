# test suit for linreg_QR

data("iris")

test_that("class is correct", {
  linreg_mod <- linreg(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
  
  expect_s3_class(linreg_mod, "linreg")
})