test_that("The CDF of DTGIW", {
  x <- c(0:10)
  expect_equal(pDTGIW(x,3.45,0.7,1.05,0), pDTGIW(x,3.45,0.7,1.05,0,lower.tail=TRUE,log.p=FALSE))
})
