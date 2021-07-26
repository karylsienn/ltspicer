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



test_that("Capacitor has correct fields", {

  capacitor_netlist <- 'C1 N003 0 1p Rser=0 Lser=0 mfg="Murata" pn="GRM0335C1ER70BA01" type="C0G"'
  expect_warning(Capacitor$new(capacitor_netlist))

  capacitor_obj <- Capacitor$new("C1 N003 0 1p")
  expect_equal(capacitor_obj$num_impedance(20e3), 0+7957747i, 1e-7)
  expect_equal(capacitor_obj$sym_impedance(20e3), "1i/(2*pi*20000*C1)")

})



test_that("Objects produce good spice lines", {
  inductor <- Inductor$new("L1 N002 N003 4u")
  expect_invisible(inductor$set_inductance(4.5e-12))
  expect_equal(inductor$spice_line(), "L1 N002 N003 4.5e-12")

  capacitor <- Capacitor$new("C1 N002 N003 5.6u")
  expect_invisible(capacitor$set_capacitance(4.5e-12))
  expect_equal(capacitor$spice_line(), "C1 N002 N003 4.5e-12")

  resistor <- Resistor$new("R2 N004 N005 5.7m")
  expect_invisible(resistor$set_resistance(4.5e-12))
  expect_equal(resistor$spice_line(), "R2 N004 N005 4.5e-12")

})




