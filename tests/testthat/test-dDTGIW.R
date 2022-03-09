test_that("The PMF of DTGIW", {
  x <- c(0:10)
  expect_equal(dDTGIW(x,3.45,0.7,1.05,0), dDTGIW(x,3.45,0.7,1.05,0,log=FALSE))
})
