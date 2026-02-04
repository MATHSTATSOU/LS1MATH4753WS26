test_that("myhdi creates a reasonable interval", {
  bci <- myhdi(funame = qbeta , shape1 = 5, shape2 = 7)
  expect_gt(bci$U, bci$L)
})
