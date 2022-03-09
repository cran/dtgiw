test_that("The quantile function of DTGIW", {
  x <- c(0:10)
  p<- pDTGIW(x,3.45,0.7,1.05,0)
  expect_equal(qDTGIW(p,3.45,0.7,1.05,0),qDTGIW(p,3.45,0.7,1.05,0,lower.tail = TRUE,log.p = FALSE))
})
