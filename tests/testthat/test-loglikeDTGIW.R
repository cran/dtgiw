test_that("Test for negative loglike value of DTGIW", {
  x <- rDTGIW(n=20,3.45,0.7,1.05,0)
  y <- x
  expect_equal(x,y)
})
