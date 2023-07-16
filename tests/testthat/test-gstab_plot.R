test_that("plot.gstab_sum works", {
  #Test with stats::lm engine
  formula <- y ~ x1 + x2
  model <- stats::lm(formula, data = n20_seed376_lm)

  stab_res <- gstab(model = model, new_data = n20_seed500_lm, nboot = 100,
                    variable_to_remove = "x2", variable_of_interest = "x1")

  stab_sum <- summary(stab_res, conf.int = TRUE, conf.level = 0.95)

  p_conf <- plot(stab_sum, conf.int = TRUE)
  p_noconf <- plot(stab_sum, conf.int = FALSE)

  expect_s3_class(p_conf$replication_stability_plot$new, "gg")
  expect_s3_class(p_conf$replication_stability_plot$boot, "gg")
  expect_s3_class(p_conf$statistical_stability_plot, "gg")
  expect_s3_class(p_conf$data_selection_stability_plot$bootstrap, "gg")
  expect_s3_class(p_conf$data_selection_stability_plot$strata_bootstrap, "gg")
  expect_s3_class(p_conf$model_selection_stability_plot, "gg")
  expect_s3_class(p_conf$numerical_stability_plot, "gg")
  expect_s3_class(p_conf$analytic_and_algebraic_stability_plot, "gg")
  expect_s3_class(p_conf$technique_stability_plot, "gg")

  expect_s3_class(p_noconf$replication_stability_plot$new, "gg")
  expect_s3_class(p_noconf$replication_stability_plot$boot, "gg")
  expect_s3_class(p_noconf$statistical_stability_plot, "gg")
  expect_s3_class(p_noconf$data_selection_stability_plot$bootstrap, "gg")
  expect_s3_class(p_noconf$data_selection_stability_plot$strata_bootstrap, "gg")
  expect_s3_class(p_noconf$model_selection_stability_plot, "gg")
  expect_s3_class(p_noconf$numerical_stability_plot, "gg")
  expect_s3_class(p_noconf$analytic_and_algebraic_stability_plot, "gg")
  expect_s3_class(p_noconf$technique_stability_plot, "gg")

  rm(p_conf, p_noconf, stab_res, stab_sum)

  #Test with stats::glm engine
  formula <- y ~ x1 + x2
  model <- stats::glm(formula, family = poisson(link = "log"), data = n20_seed587_pois)
  stab_res <- suppressWarnings(gstab(model = model, family = poisson(link = "log"),
                                     new_data = n20_seed500_pois, nboot = 100,
                                     variable_to_remove = "x2",
                                     variable_of_interest = "x1"))

  stab_sum <- suppressWarnings(summary(stab_res, conf.int = TRUE, conf.level = 0.95))

  p_conf <- plot(stab_sum, conf.int = TRUE)
  p_noconf <- plot(stab_sum, conf.int = FALSE)

  expect_s3_class(p_conf$replication_stability_plot$new, "gg")
  expect_s3_class(p_conf$replication_stability_plot$boot, "gg")
  expect_s3_class(p_conf$statistical_stability_plot, "gg")
  expect_s3_class(p_conf$data_selection_stability_plot$bootstrap, "gg")
  expect_s3_class(p_conf$data_selection_stability_plot$strata_bootstrap, "gg")
  expect_s3_class(p_conf$model_selection_stability_plot, "gg")
  expect_s3_class(p_conf$numerical_stability_plot, "gg")
  expect_s3_class(p_conf$analytic_and_algebraic_stability_plot, "gg")
  expect_s3_class(p_conf$technique_stability_plot, "gg")

  expect_s3_class(p_noconf$replication_stability_plot$new, "gg")
  expect_s3_class(p_noconf$replication_stability_plot$boot, "gg")
  expect_s3_class(p_noconf$statistical_stability_plot, "gg")
  expect_s3_class(p_noconf$data_selection_stability_plot$bootstrap, "gg")
  expect_s3_class(p_noconf$data_selection_stability_plot$strata_bootstrap, "gg")
  expect_s3_class(p_noconf$model_selection_stability_plot, "gg")
  expect_s3_class(p_noconf$numerical_stability_plot, "gg")
  expect_s3_class(p_noconf$analytic_and_algebraic_stability_plot, "gg")
  expect_s3_class(p_noconf$technique_stability_plot, "gg")
})
