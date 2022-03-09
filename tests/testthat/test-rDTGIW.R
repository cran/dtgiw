test_that("The random generating function of DTGIW", {
  x <- rDTGIW(101,3.45,0.7,1.05,0)
  y <- x
  expect_equal(x, y)
})
