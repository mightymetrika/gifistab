test_that("plot_replication_stability works", {
  formula <- y ~ x1 + x2

  #Test with stats::lm engine
  model <- stats::lm(formula, data = n20_seed376_lm)

  #Test with nboot and new data
  stab_res <- gstab(model = model, new_data = n20_seed500_lm, nboot = 100)
  stab_sum <- suppressWarnings(summary(stab_res, conf.int = TRUE,
                                       conf.level = 0.95))

  p_conf <- plot_replication_stability(stab_sum, conf.int = TRUE)
  p_noconf <- plot_replication_stability(stab_sum, conf.int = FALSE)

  expect_s3_class(p_conf$new, "gg")
  expect_s3_class(p_conf$boot, "gg")
  expect_s3_class(p_noconf$new, "gg")
  expect_s3_class(p_noconf$boot, "gg")

  rm(p_conf, p_noconf, stab_res, stab_sum)

  # Test with nboot only
  stab_res <- gstab(model = model, nboot = 100)
  stab_sum <- suppressWarnings(summary(stab_res, conf.int = TRUE,
                                       conf.level = 0.95))

  p_conf <- plot_replication_stability(stab_sum, conf.int = TRUE)
  p_noconf <- plot_replication_stability(stab_sum, conf.int = FALSE)

  expect_null(p_conf$new, "gg")
  expect_s3_class(p_conf$boot, "gg")
  expect_null(p_noconf$new, "gg")
  expect_s3_class(p_noconf$boot, "gg")

  rm(p_conf, p_noconf, stab_res, stab_sum)

  # Test with new only
  stab_res <- gstab(model = model, new_data = n20_seed500_lm)
  stab_sum <- suppressWarnings(summary(stab_res, conf.int = TRUE,
                                       conf.level = 0.95))

  p_conf <- plot_replication_stability(stab_sum, conf.int = TRUE)
  p_noconf <- plot_replication_stability(stab_sum, conf.int = FALSE)

  expect_s3_class(p_conf$new, "gg")
  expect_null(p_conf$boot, "gg")
  expect_s3_class(p_noconf$new, "gg")
  expect_null(p_noconf$boot, "gg")

  rm(p_conf, p_noconf, stab_res, stab_sum)

  #Test with stats::glm engine
  model <- stats::glm(formula, family = poisson(link = "log"),
                      data = n20_seed587_pois)

  #Test with nboot
  stab_res <- suppressWarnings(gstab(model = model, new_data = n20_seed500_pois,
                                     nboot = 100, family = poisson))
  stab_sum <- summary(stab_res, conf.int = TRUE, conf.level = 0.95)

  p_conf <- plot_replication_stability(stab_sum, conf.int = TRUE)
  p_noconf <- plot_replication_stability(stab_sum, conf.int = FALSE)

  expect_s3_class(p_conf$new, "gg")
  expect_s3_class(p_conf$boot, "gg")
  expect_s3_class(p_noconf$new, "gg")
  expect_s3_class(p_noconf$boot, "gg")
})


test_that("plot_statistical_stability works", {
  formula <- y ~ x1 + x2

  #Test with stats::lm engine
  model <- stats::lm(formula, data = n20_seed376_lm)


  stab_res <- gstab(model = model)
  stab_sum <- suppressWarnings(summary(stab_res, conf.int = TRUE,
                                       conf.level = 0.95))

  p_conf <- plot_statistical_stability(stab_sum, conf.int = TRUE)
  p_noconf <- plot_statistical_stability(stab_sum, conf.int = FALSE)

  expect_s3_class(p_conf, "gg")
  expect_s3_class(p_noconf, "gg")

  rm(p_conf, p_noconf, stab_res, stab_sum)

  #Test with stats::glm engine
  model <- stats::glm(formula, family = poisson(link = "log"),
                      data = n20_seed587_pois)

  stab_res <- suppressWarnings(gstab(model = model, family = poisson))
  stab_sum <- summary(stab_res, conf.int = TRUE, conf.level = 0.95)

  p_conf <- plot_statistical_stability(stab_sum, conf.int = TRUE)
  p_noconf <- plot_statistical_stability(stab_sum, conf.int = FALSE)

  expect_s3_class(p_conf, "gg")
  expect_s3_class(p_noconf, "gg")
})


test_that("plot_data_selection_stability works", {
  formula <- y ~ x1 + x2

  #Test with stats::lm engine
  model <- stats::lm(formula, data = n20_seed376_lm)

  stab_res <- gstab(model = model)
  stab_sum <- suppressWarnings(summary(stab_res, conf.int = TRUE,
                                       conf.level = 0.95))

  p_conf <- plot_data_selection_stability(stab_sum, conf.int = TRUE)
  p_noconf <- plot_data_selection_stability(stab_sum, conf.int = FALSE)

  expect_s3_class(p_conf$bootstrap, "gg")
  expect_s3_class(p_conf$no_outlier, "gg")
  expect_s3_class(p_conf$strata_bootstrap, "gg")
  expect_s3_class(p_noconf$bootstrap, "gg")
  expect_s3_class(p_noconf$no_outlier, "gg")
  expect_s3_class(p_noconf$strata_bootstrap, "gg")

  rm(p_conf, p_noconf, stab_res, stab_sum)

  #Test with stats::glm engine
  model <- stats::glm(formula, family = poisson(link = "log"),
                      data = n20_seed587_pois)

  stab_res <- suppressWarnings(gstab(model = model, family = poisson))
  stab_sum <- summary(stab_res, conf.int = TRUE, conf.level = 0.95)

  p_conf <- plot_data_selection_stability(stab_sum, conf.int = TRUE)
  p_noconf <- plot_data_selection_stability(stab_sum, conf.int = FALSE)

  expect_s3_class(p_conf$bootstrap, "gg")
  expect_s3_class(p_conf$no_outlier, "gg")
  expect_s3_class(p_conf$strata_bootstrap, "gg")
  expect_s3_class(p_noconf$bootstrap, "gg")
  expect_s3_class(p_noconf$no_outlier, "gg")
  expect_s3_class(p_noconf$strata_bootstrap, "gg")
})

test_that("plot_model_selection_stability works", {
  formula <- y ~ x1 + x2

  #Test with stats::lm engine
  model <- stats::lm(formula, data = n20_seed376_lm)

  #Test with variable to remove and variable of interest
  stab_res <- gstab(model = model, variable_to_remove = "x2",
                    variable_of_interest = "x1")
  stab_sum <- suppressWarnings(summary(stab_res, conf.int = TRUE,
                                       conf.level = 0.95))

  p <- plot_model_selection_stability(stab_sum)

  expect_s3_class(p, "gg")

  rm(p, stab_res, stab_sum)

  #Test with variable to remove only
  stab_res <- gstab(model = model, variable_to_remove = "x2")
  stab_sum <- suppressWarnings(summary(stab_res, conf.int = TRUE,
                                       conf.level = 0.95))

  p <- plot_model_selection_stability(stab_sum)

  expect_s3_class(p, "gg")

  rm(p, stab_res, stab_sum)

  #Test with variable of interest only
  stab_res <- gstab(model = model, variable_of_interest = "x1")
  stab_sum <- suppressWarnings(summary(stab_res, conf.int = TRUE,
                                       conf.level = 0.95))

  p <- plot_model_selection_stability(stab_sum)

  expect_s3_class(p, "gg")

  rm(p, stab_res, stab_sum)

  #Test with stats::glm engine
  model <- stats::glm(formula, family = poisson(link = "log"),
                      data = n20_seed587_pois)

  stab_res <- suppressWarnings(gstab(model = model, variable_to_remove = "x2",
                                     variable_of_interest = "x1",
                                     family = poisson))
  stab_sum <- summary(stab_res, conf.int = TRUE, conf.level = 0.95)

  p <- plot_model_selection_stability(stab_sum)

  expect_s3_class(p, "gg")
})

test_that("plot_numerical_stability works", {
  formula <- y ~ x1 + x2

  #Test with stats::lm engine
  model <- stats::lm(formula, data = n20_seed376_lm)

  #Test with nboot and new data
  stab_res <- gstab(model = model)
  stab_sum <- suppressWarnings(summary(stab_res, conf.int = TRUE,
                                       conf.level = 0.95))

  p_conf <- plot_numerical_stability(stab_sum, conf.int = TRUE)
  p_noconf <- plot_numerical_stability(stab_sum, conf.int = FALSE)

  expect_s3_class(p_conf, "gg")
  expect_s3_class(p_noconf, "gg")

  rm(p_conf, p_noconf, stab_res, stab_sum)

  #Test with stats::glm engine
  model <- stats::glm(formula, family = poisson(link = "log"),
                      data = n20_seed587_pois)

  stab_res <- suppressWarnings(gstab(model = model, family = poisson))
  stab_sum <- summary(stab_res, conf.int = TRUE, conf.level = 0.95)

  p_conf <- plot_numerical_stability(stab_sum, conf.int = TRUE)
  p_noconf <- plot_numerical_stability(stab_sum, conf.int = FALSE)

  expect_s3_class(p_conf, "gg")
  expect_s3_class(p_noconf, "gg")
})

test_that("plot_analytic_and_algebraic_stability works", {
  formula <- y ~ x1 + x2

  #Test with stats::lm engine
  model <- stats::lm(formula, data = n20_seed376_lm)

  #Test with nboot and new data
  stab_res <- gstab(model = model)
  stab_sum <- suppressWarnings(summary(stab_res, conf.int = TRUE,
                                       conf.level = 0.95))

  p <- plot_analytic_and_algebraic_stability(stab_sum)

  expect_s3_class(p, "gg")

  rm(p, stab_res, stab_sum)

  #Test with stats::glm engine
  model <- stats::glm(formula, family = poisson(link = "log"),
                      data = n20_seed587_pois)
  stab_res <- suppressWarnings(gstab(model = model, family = poisson))
  stab_sum <- summary(stab_res, conf.int = TRUE, conf.level = 0.95)

  p <- plot_analytic_and_algebraic_stability(stab_sum)

  expect_s3_class(p, "gg")
})

test_that("plot_technique_stability works", {
  formula <- y ~ x1 + x2

  #Test with stats::lm engine
  model <- stats::lm(formula, data = n20_seed376_lm)

  #Test with nboot and new data
  stab_res <- gstab(model = model)
  stab_sum <- suppressWarnings(summary(stab_res, conf.int = TRUE,
                                       conf.level = 0.95))

  p_conf <- plot_technique_stability(stab_sum, conf.int = TRUE)
  p_noconf <- plot_technique_stability(stab_sum, conf.int = FALSE)

  expect_s3_class(p_conf, "gg")
  expect_s3_class(p_noconf, "gg")

  rm(p_conf, p_noconf, stab_res, stab_sum)

  #Test with stats::glm engine
  formula <- y ~ x1 + x2
  model <- stats::glm(formula, family = poisson(link = "log"),
                      data = n20_seed587_pois)

  stab_res <- suppressWarnings(gstab(model = model, family = poisson(link = "log")))
  stab_sum <- suppressWarnings(summary(stab_res, conf.int = TRUE, conf.level = 0.95))

  p_conf <- plot_technique_stability(stab_sum, conf.int = TRUE)
  p_noconf <- plot_technique_stability(stab_sum, conf.int = FALSE)

  expect_s3_class(p_conf, "gg")
  expect_s3_class(p_noconf, "gg")
})
