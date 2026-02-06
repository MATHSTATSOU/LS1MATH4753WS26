test_that("myvar function evaluates with Rs standard var", {
  v <- rnorm(30, 10, 5)
  expect_equal(myvar(v), var(v))
})
