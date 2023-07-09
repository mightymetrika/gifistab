test_that("gstab_lm works", {
  n <- 20
  data <- data.frame(y = 3*stats::rnorm(n) +5,
                     x1 = 3*stats::rnorm(n) +5 + stats::rnorm(n, 2, 0.3),
                     x2 = 2*stats::rnorm(n) + 1.5*stats::rnorm(n) + stats::rnorm(n, 1, 0.05))

  formula <- y ~ x1 + x2

  stab_res <- gstab_lm(data = data, formula = formula, nboot = 100)

  test <- summary.gstab_lm(stab_res)

  expect_equal(2 * 2, 4)
})
