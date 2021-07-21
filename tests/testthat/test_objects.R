context("Electrical objects")
library(ltspicer)


test_that("Voltage object has correct fields", {
  voltage_obj <- Voltage$new("V1 N001 0 0 AC 1 0 Rser=0 Cpar=0")
  expect_equal(voltage_obj$name, "V1")
  expect_equal(voltage_obj$plus, "N001")
  expect_equal(voltage_obj$minus, "0")
  expect_equal(voltage_obj$dc, 0)
  expect_equal(voltage_obj$ac$amplitude, 1)
  expect_equal(voltage_obj$ac$phase, 0)
  expect_equal(voltage_obj$Rser, 0)
  expect_equal(voltage_obj$Cpar, 0)
})




