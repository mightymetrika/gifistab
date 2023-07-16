test_that("Test that gstab.lm and summary.gstab_lm works", {
  formula <- y ~ x1 + x2
  model <- stats::lm(formula, data = n20_seed376_lm)

  #Test with nboot
  stab_res <- gstab(model = model, nboot = 100)
  stab_sum <- summary(stab_res, conf.int = FALSE)
  expect_s3_class(stab_res, "gstab_lm")
  expect_named(stab_sum, c("original_summary", "replication_stability_summary",
                           "statistical_stability_summary", "data_selection_stability_summary",
                           "model_selection_stability_summary", "numerical_stability_summary",
                           "analytic_and_algebraic_stability_summary", "technique_stability_summary"))

  #Test with new data
  stab_res <- gstab(model, new_data = n20_seed500_lm)
  stab_sum <- summary(stab_res, conf.int = FALSE)
  expect_s3_class(stab_res, "gstab_lm")
  expect_named(stab_sum, c("original_summary", "replication_stability_summary",
                           "statistical_stability_summary", "data_selection_stability_summary",
                           "model_selection_stability_summary", "numerical_stability_summary",
                           "analytic_and_algebraic_stability_summary", "technique_stability_summary"))

  #Test with variable to remove
  stab_res <- gstab(model, variable_to_remove = "x2")
  stab_sum <- summary(stab_res, conf.int = FALSE)
  expect_s3_class(stab_res, "gstab_lm")
  expect_named(stab_sum, c("original_summary", "replication_stability_summary",
                           "statistical_stability_summary", "data_selection_stability_summary",
                           "model_selection_stability_summary", "numerical_stability_summary",
                           "analytic_and_algebraic_stability_summary", "technique_stability_summary"))

  #Test with variable of interest
  stab_res <- gstab(model, variable_of_interest = "x1")
  stab_sum <- summary(stab_res, conf.int = FALSE)
  expect_s3_class(stab_res, "gstab_lm")
  expect_named(stab_sum, c("original_summary", "replication_stability_summary",
                           "statistical_stability_summary", "data_selection_stability_summary",
                           "model_selection_stability_summary", "numerical_stability_summary",
                           "analytic_and_algebraic_stability_summary", "technique_stability_summary"))

  #Test with all parameters set
  stab_res <- gstab(model = model, nboot = 100, new_data = n20_seed500_lm,
                    variable_to_remove = "x2", variable_of_interest = "x1")
  stab_sum <- summary(stab_res, conf.int = FALSE)
  expect_s3_class(stab_res, "gstab_lm")
  expect_named(stab_sum, c("original_summary", "replication_stability_summary",
                           "statistical_stability_summary", "data_selection_stability_summary",
                           "model_selection_stability_summary", "numerical_stability_summary",
                           "analytic_and_algebraic_stability_summary", "technique_stability_summary"))
})

test_that("Test that gstab.glm works and summary.gstab_glm works", {
  formula <- y ~ x1 + x2

  # Fit the model
  model <- stats::glm(formula, family = binomial, data = n20_seed376_bin)

  #Test with nboot
  stab_res <- suppressWarnings(gstab(model = model, nboot = 100))
  stab_sum <- summary(stab_res, conf.int = FALSE)
  expect_s3_class(stab_res, "gstab_glm")
  expect_named(stab_sum, c("original_summary", "replication_stability_summary",
                           "statistical_stability_summary", "data_selection_stability_summary",
                           "model_selection_stability_summary", "numerical_stability_summary",
                           "analytic_and_algebraic_stability_summary", "technique_stability_summary"))

  #Test with new data
  stab_res <- suppressWarnings(gstab(model, new_data = n20_seed500_bin))
  stab_sum <- summary(stab_res, conf.int = FALSE)
  expect_s3_class(stab_res, "gstab_glm")
  expect_named(stab_sum, c("original_summary", "replication_stability_summary",
                           "statistical_stability_summary", "data_selection_stability_summary",
                           "model_selection_stability_summary", "numerical_stability_summary",
                           "analytic_and_algebraic_stability_summary", "technique_stability_summary"))

  #Test with variable to remove
  stab_res <- suppressWarnings(gstab(model, variable_to_remove = "x2"))
  stab_sum <- summary(stab_res, conf.int = FALSE)
  expect_s3_class(stab_res, "gstab_glm")
  expect_named(stab_sum, c("original_summary", "replication_stability_summary",
                           "statistical_stability_summary", "data_selection_stability_summary",
                           "model_selection_stability_summary", "numerical_stability_summary",
                           "analytic_and_algebraic_stability_summary", "technique_stability_summary"))

  #Test with variable of interest
  stab_res <- suppressWarnings(gstab(model, variable_of_interest = "x1"))
  stab_sum <- summary(stab_res, conf.int = FALSE)
  expect_s3_class(stab_res, "gstab_glm")
  expect_named(stab_sum, c("original_summary", "replication_stability_summary",
                           "statistical_stability_summary", "data_selection_stability_summary",
                           "model_selection_stability_summary", "numerical_stability_summary",
                           "analytic_and_algebraic_stability_summary", "technique_stability_summary"))


})

test_that("Test that gstab.glm and summary.gstab_glm work with family = poisson", {
  formula <- y ~ x1 + x2

  # Fit the model
  model <- stats::glm(formula, family = poisson(link = "log"),
                      data = n20_seed587_pois)

  #Test with nboot
  stab_res <- suppressWarnings(gstab(model = model, nboot = 100))
  stab_sum <- summary(stab_res, conf.int = FALSE)
  expect_s3_class(stab_res, "gstab_glm")
  expect_named(stab_sum, c("original_summary", "replication_stability_summary",
                           "statistical_stability_summary", "data_selection_stability_summary",
                           "model_selection_stability_summary", "numerical_stability_summary",
                           "analytic_and_algebraic_stability_summary", "technique_stability_summary"))

  #Test with new data
  stab_res <- suppressWarnings(gstab(model, new_data = n20_seed500_pois))
  stab_sum <- summary(stab_res, conf.int = FALSE)
  expect_s3_class(stab_res, "gstab_glm")
  expect_named(stab_sum, c("original_summary", "replication_stability_summary",
                           "statistical_stability_summary", "data_selection_stability_summary",
                           "model_selection_stability_summary", "numerical_stability_summary",
                           "analytic_and_algebraic_stability_summary", "technique_stability_summary"))

  #Test with variable to remove
  stab_res <- suppressWarnings(gstab(model, variable_to_remove = "x2"))
  stab_sum <- summary(stab_res, conf.int = FALSE)
  expect_s3_class(stab_res, "gstab_glm")
  expect_named(stab_sum, c("original_summary", "replication_stability_summary",
                           "statistical_stability_summary", "data_selection_stability_summary",
                           "model_selection_stability_summary", "numerical_stability_summary",
                           "analytic_and_algebraic_stability_summary", "technique_stability_summary"))

  #Test with variable of interest
  stab_res <- suppressWarnings(gstab(model, variable_of_interest = "x1"))
  stab_sum <- summary(stab_res, conf.int = FALSE)
  expect_s3_class(stab_res, "gstab_glm")
  expect_named(stab_sum, c("original_summary", "replication_stability_summary",
                           "statistical_stability_summary", "data_selection_stability_summary",
                           "model_selection_stability_summary", "numerical_stability_summary",
                           "analytic_and_algebraic_stability_summary", "technique_stability_summary"))
})
