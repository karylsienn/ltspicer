context("Spice utilities")


test_that("impedance is correct.", {
  v1 <- Voltage$new("V1 N001 0 0")
  v2 <- Voltage$new("V4 N001 N002 0")

  expect_equal(part_impedance(v1), "-V(N001)/I(V1)")
  expect_equal(part_impedance(v2), "(V(N002)-V(N001))/I(V4)")
})
