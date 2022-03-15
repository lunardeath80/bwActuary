test_that("diagonal function", {

  annual_annual <- matrix(1:9,3,3)
  annual_biannual <- matrix(1:18, 3,6)
  annual_quarterly <- matrix(1:36, 3,12)
  annual_monthly <- matrix(1:108,3,36)
  run_off <- matrix(1:21,3,7)

  expect_equal(diagonal(annual_annual), c(7,5,3))
  expect_equal(diagonal(annual_biannual), c(16,11,6))
  expect_equal(diagonal(annual_quarterly), c(34,23,12))
  expect_equal(diagonal(annual_monthly), c(106,71,36))

})
