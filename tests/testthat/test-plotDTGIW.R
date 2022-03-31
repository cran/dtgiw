test_that("Test plot of DTGIW", {
  x <- c(0:10)
  fx<- dDTGIW(x,3.45,0.7,1.05,0)
  alpha<-2.50;beta <-0.5; lambda <-1.05; theta <- 0
  fx2 <-dDTGIW(x,alpha,beta,lambda,theta)
  expect_equal(plotDTGIW(x,fx,3.45,0.7,1.05,0), plotDTGIW(x,fx))
  expect_equal(plotDTGIW(x,fx2,2.50,0.5,1.00,0),
        plotDTGIW(x,fx2,alpha=2.50,beta=0.5,lambda=1.00,theta=0))
})
