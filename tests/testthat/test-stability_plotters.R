test_that("plot_replication_stability works", {
  n <- 20
  set.seed(376)
  data <- data.frame(y = 3*stats::rnorm(n) +5,
                     x1 = 3*stats::rnorm(n) +5 + stats::rnorm(n, 2, 0.3),
                     x2 = 2*stats::rnorm(n) + 1.5*stats::rnorm(n) + stats::rnorm(n, 1, 0.05))

  formula <- y ~ x1 + x2

  model <- stats::lm(formula, data = data)

  #Test with nboot
  stab_res <- gstab(model = model, nboot = 100)
  stab_sum <- summary(stab_res, conf.int = TRUE, conf.level = 0.95)

  p_conf <- plot_replication_stability(stab_sum, conf.int = TRUE)
  p_noconf <- plot_replication_stability(stab_sum, conf.int = FALSE)

  expect_s3_class(p_conf, "gg")
  expect_s3_class(p_noconf, "gg")
})
