test_that("Unit Testing for saeHB.TF.bteta", {
  skip_on_cran()

  #Case where data defined
  expect_true(is.list(
    betaTF(formula = y ~ X1 + X2, area="codearea",weight="w",data=dataBeta)
  ))

  #Case where data defined for non-sampled ares
  expect_true(is.list(
    betaTF(formula = y ~ X1 + X2, area="codearea",weight="w",data=dataBetaNS)
  ))

  #Case where data is undefined
  expect_error(
    betaTF(formula = y ~X1 + X2, area="codearea", weight = "w")
  )

  #Case where response variable (y) not between 0 and 1
  data_invalid_y <- dataBeta
  data_invalid_y[c(5,10,15), "y"] <- c(1.2,3,-0.5)
  expect_error(
    betaTF(formula = y ~ X1 + X2, area="codearea",weight="w",data=data_invalid_y)
  )

  #Case where response variable (y) not between 0 and 1 for nonsampled data
  data_invalid_y_NS <- dataBetaNS
  data_invalid_y_NS[10, "y"] <- 1.2
  expect_error(
    betaTF(formula = y ~ X1 + X2, area="codearea", weight="w", data=data_invalid_y_NS)
  )

  #Case where auxiliary variable (X) contain NA values
  data_invalid_x <- dataBeta
  data_invalid_x[c(5,10,15), "X2"] <- NA
  expect_error(
    betaTF(formula = y ~ X1 + X2, area="codearea",weight="w",data=data_invalid_x)
  )

  #Case where var.coef and coef defined
  var_coef <- rep(1, 3)
  coef <- rep (0.5,3)
  expect_true(is.list(
    betaTF(formula = y ~ X1 + X2, area="codearea",weight="w",data=dataBeta, coef = coef, var.coef = var_coef)
  ))

  #Case where coef defined but the length differ
  coef_invalid <- rep (0.5,2)
  expect_error(
    betaTF(formula = y ~ X1 + X2, area="codearea",weight="w",data=dataBeta, coef = coef_invalid)
  )

  #Case where var.coef defined but the length differ
  var_coef_invalid <- rep (0.5,2)
  expect_error(
    betaTF(formula = y ~ X1 + X2, area="codearea",weight="w",data=dataBeta, var.coef = var_coef_invalid)
  )

  #Case where iter.update < 3
  expect_error(
    betaTF(formula = y ~ X1 + X2, area="codearea", weight = "w", data = dataBeta, iter.update = 1)
  )

  #Case where codearea contain NA value
  data_invalid_area <- dataBeta
  data_invalid_area[10, "codearea"] <- NA
  expect_error(
    betaTF(formula = y ~ X1 + X2, area="codearea",weight="w",data=data_invalid_area)
  )

  #Case where weight contain NA value
  data_invalid_w <- dataBeta
  data_invalid_w[10, "w"] <- NA
  expect_error(
    betaTF(formula = y ~ X1 + X2, area="codearea",weight="w",data=data_invalid_w)
  )

})
