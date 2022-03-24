test_that("Test plot of DTGIW", {
  x <- c(0:10)
  fx<- dDTGIW(x,3.45,0.7,1.05,0)
  expect_equal(  plotDTGIW(x,fx,3.45,0.7,1.05,0), plotDTGIW(x,fx))
})
