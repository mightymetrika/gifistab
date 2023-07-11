test_that("Test that gstab.lm and summary.gstab_lm works", {
  n <- 20
  set.seed(376)
  data <- data.frame(y = 3*stats::rnorm(n) +5,
                     x1 = 3*stats::rnorm(n) +5 + stats::rnorm(n, 2, 0.3),
                     x2 = 2*stats::rnorm(n) + 1.5*stats::rnorm(n) + stats::rnorm(n, 1, 0.05))

  formula <- y ~ x1 + x2

  #Test with nboot
  stab_res <- gstab.lm(data = data, formula = formula, nboot = 100)
  stab_sum <- summary.gstab_lm(stab_res)
  expect_s3_class(stab_res, "gstab_lm")
  expect_named(stab_sum, c("original_summary", "replication_stability_summary",
                           "statistical_stability_summary", "data_selection_stability_summary",
                           "model_selection_stability_summary", "numerical_stability_summary",
                           "analytic_and_algebraic_stability_summary", "technique_stability_summary"))
})