context("Reading utils")
library(ltspicer)

test_that("netlist is parsed appropriately", {
  expect_equal({
    netlist <- parse_netlist("../../spice_files/2m_cable_model.net")
    voltage <- netlist[stringr::str_detect(netlist, "^V")]
    voltage
  }, "V1 N001 0 0 AC 1 0 Rser=0 Cpar=0")
})



test_that("suffixes are parse correctly", {
  expect_equal(parse_number_suffix('45     u'), 45e-6)
  expect_equal(parse_number_suffix('45Megkfdsaf'), 45e6)
  expect_equal(parse_number_suffix('45Megkfdsaf'), 45000000)
  expect_equal(parse_number_suffix('4.623M'), parse_number_suffix('4.623    m'))
})



test_that("we need to put a value", {
  expect_error(assert_value("C1"))
  expect_error(assert_value("blabla"))
  expect_true(assert_value("45 u"))
})
