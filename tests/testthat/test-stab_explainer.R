test_that("Test that stab_explainer.gstab_lm works", {
  n <- 20
  set.seed(376)
  data <- data.frame(y = 3*stats::rnorm(n) +5,
                     x1 = 3*stats::rnorm(n) +5 + stats::rnorm(n, 2, 0.3),
                     x2 = 2*stats::rnorm(n) + 1.5*stats::rnorm(n) + stats::rnorm(n, 1, 0.05))

  formula <- y ~ x1 + x2
  model <- stats::lm(formula, data = data)

  #Test with nboot
  stab_res <- gstab(model = model, nboot = 100)
  stab_exp <- stab_explainer.gstab_lm(stab_res)
  expect_equal(length(stab_exp), 8)
})
