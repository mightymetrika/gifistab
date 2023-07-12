test_that("Test that gstab.lm and summary.gstab_lm works", {
  n <- 20
  set.seed(376)
  data <- data.frame(y = 3*stats::rnorm(n) +5,
                     x1 = 3*stats::rnorm(n) +5 + stats::rnorm(n, 2, 0.3),
                     x2 = 2*stats::rnorm(n) + 1.5*stats::rnorm(n) + stats::rnorm(n, 1, 0.05))

  formula <- y ~ x1 + x2

  model <- stats::lm(formula, data = data)

  #Test with nboot
  stab_res <- gstab(model = model, nboot = 100)
  stab_sum <- summary(stab_res)
  expect_s3_class(stab_res, "gstab_lm")
  expect_named(stab_sum, c("original_summary", "replication_stability_summary",
                           "statistical_stability_summary", "data_selection_stability_summary",
                           "model_selection_stability_summary", "numerical_stability_summary",
                           "analytic_and_algebraic_stability_summary", "technique_stability_summary"))

  #Test with new data
  set.seed(500)
  new_data <- data.frame(y = 3*stats::rnorm(n) +5,
                     x1 = 3*stats::rnorm(n) +5 + stats::rnorm(n, 2, 0.3),
                     x2 = 2*stats::rnorm(n) + 1.5*stats::rnorm(n) + stats::rnorm(n, 1, 0.05))
  stab_res <- gstab(model, new_data = new_data)
  stab_sum <- summary(stab_res)
  expect_s3_class(stab_res, "gstab_lm")
  expect_named(stab_sum, c("original_summary", "replication_stability_summary",
                           "statistical_stability_summary", "data_selection_stability_summary",
                           "model_selection_stability_summary", "numerical_stability_summary",
                           "analytic_and_algebraic_stability_summary", "technique_stability_summary"))

  #Test with variable to remove
  stab_res <- gstab(model, variable_to_remove = "x2")
  stab_sum <- summary(stab_res)
  expect_s3_class(stab_res, "gstab_lm")
  expect_named(stab_sum, c("original_summary", "replication_stability_summary",
                           "statistical_stability_summary", "data_selection_stability_summary",
                           "model_selection_stability_summary", "numerical_stability_summary",
                           "analytic_and_algebraic_stability_summary", "technique_stability_summary"))

  #Test with variable of interest
  stab_res <- gstab(model, variable_of_interest = "x1")
  stab_sum <- summary(stab_res)
  expect_s3_class(stab_res, "gstab_lm")
  expect_named(stab_sum, c("original_summary", "replication_stability_summary",
                           "statistical_stability_summary", "data_selection_stability_summary",
                           "model_selection_stability_summary", "numerical_stability_summary",
                           "analytic_and_algebraic_stability_summary", "technique_stability_summary"))

  #Test with all parameters set
  stab_res <- gstab(model = model, nboot = 100, new_data = new_data,
                    variable_to_remove = "x2", variable_of_interest = "x1")
  stab_sum <- summary(stab_res)
  expect_s3_class(stab_res, "gstab_lm")
  expect_named(stab_sum, c("original_summary", "replication_stability_summary",
                           "statistical_stability_summary", "data_selection_stability_summary",
                           "model_selection_stability_summary", "numerical_stability_summary",
                           "analytic_and_algebraic_stability_summary", "technique_stability_summary"))
})

test_that("Test that gstab.glm works and summary.gstab_lm works", {
  # Generating example data
  n <- 20
  set.seed(376)
  data <- data.frame(y = stats::rbinom(n, 1, 0.5),
                      x1 = 3*stats::rnorm(n) +5 + stats::rnorm(n, 2, 0.3),
                      x2 = 2*stats::rnorm(n) + 1.5*stats::rnorm(n) + stats::rnorm(n, 1, 0.05))
  formula <- y ~ x1 + x2

  # Fit the model
  model <- stats::glm(formula, family = binomial, data = data)

  #Test with nboot
  stab_res <- suppressWarnings(gstab(model = model, nboot = 100))
  stab_sum <- summary(stab_res)
  expect_s3_class(stab_res, "gstab_glm")
  expect_named(stab_sum, c("original_summary", "replication_stability_summary",
                           "statistical_stability_summary", "data_selection_stability_summary",
                           "model_selection_stability_summary", "numerical_stability_summary",
                           "analytic_and_algebraic_stability_summary", "technique_stability_summary"))

  #Test with new data
  set.seed(500)
  new_data <- data.frame(y = stats::rbinom(n, 1, 0.5),
                         x1 = 3*stats::rnorm(n) +5 + stats::rnorm(n, 2, 0.3),
                         x2 = 2*stats::rnorm(n) + 1.5*stats::rnorm(n) + stats::rnorm(n, 1, 0.05))
  stab_res <- suppressWarnings(gstab(model, new_data = new_data))
  stab_sum <- summary(stab_res)
  expect_s3_class(stab_res, "gstab_glm")
  expect_named(stab_sum, c("original_summary", "replication_stability_summary",
                           "statistical_stability_summary", "data_selection_stability_summary",
                           "model_selection_stability_summary", "numerical_stability_summary",
                           "analytic_and_algebraic_stability_summary", "technique_stability_summary"))

  #Test with variable to remove
  stab_res <- suppressWarnings(gstab(model, variable_to_remove = "x2"))
  stab_sum <- summary(stab_res)
  expect_s3_class(stab_res, "gstab_glm")
  expect_named(stab_sum, c("original_summary", "replication_stability_summary",
                           "statistical_stability_summary", "data_selection_stability_summary",
                           "model_selection_stability_summary", "numerical_stability_summary",
                           "analytic_and_algebraic_stability_summary", "technique_stability_summary"))

  #Test with variable of interest
  stab_res <- suppressWarnings(gstab(model, variable_of_interest = "x1"))
  stab_sum <- summary(stab_res)
  expect_s3_class(stab_res, "gstab_glm")
  expect_named(stab_sum, c("original_summary", "replication_stability_summary",
                           "statistical_stability_summary", "data_selection_stability_summary",
                           "model_selection_stability_summary", "numerical_stability_summary",
                           "analytic_and_algebraic_stability_summary", "technique_stability_summary"))


})

test_that("Test that gstab.glm and summary.gstab_lm work with family = poisson", {
  # Generating example data
  n <- 20
  set.seed(587)
  data <- data.frame(y = stats::rpois(n, 10),
                     x1 = 3*stats::rnorm(n) +5 + stats::rnorm(n, 2, 0.3),
                     x2 = 2*stats::rnorm(n) + 1.5*stats::rnorm(n) + stats::rnorm(n, 1, 0.05))
  formula <- y ~ x1 + x2

  # Fit the model
  model <- stats::glm(formula, family = poisson(link = "log"), data = data)

  #Test with nboot
  stab_res <- suppressWarnings(gstab(model = model, nboot = 100))
  stab_sum <- summary(stab_res)
  expect_s3_class(stab_res, "gstab_glm")
  expect_named(stab_sum, c("original_summary", "replication_stability_summary",
                           "statistical_stability_summary", "data_selection_stability_summary",
                           "model_selection_stability_summary", "numerical_stability_summary",
                           "analytic_and_algebraic_stability_summary", "technique_stability_summary"))

  #Test with new data
  set.seed(500)
  new_data <- data.frame(y = stats::rpois(n, 10),
                     x1 = 3*stats::rnorm(n) +5 + stats::rnorm(n, 2, 0.3),
                     x2 = 2*stats::rnorm(n) + 1.5*stats::rnorm(n) + stats::rnorm(n, 1, 0.05))
  stab_res <- suppressWarnings(gstab(model, new_data = new_data))
  stab_sum <- summary(stab_res)
  expect_s3_class(stab_res, "gstab_glm")
  expect_named(stab_sum, c("original_summary", "replication_stability_summary",
                           "statistical_stability_summary", "data_selection_stability_summary",
                           "model_selection_stability_summary", "numerical_stability_summary",
                           "analytic_and_algebraic_stability_summary", "technique_stability_summary"))

  #Test with variable to remove
  stab_res <- suppressWarnings(gstab(model, variable_to_remove = "x2"))
  stab_sum <- summary(stab_res)
  expect_s3_class(stab_res, "gstab_glm")
  expect_named(stab_sum, c("original_summary", "replication_stability_summary",
                           "statistical_stability_summary", "data_selection_stability_summary",
                           "model_selection_stability_summary", "numerical_stability_summary",
                           "analytic_and_algebraic_stability_summary", "technique_stability_summary"))

  #Test with variable of interest
  stab_res <- suppressWarnings(gstab(model, variable_of_interest = "x1"))
  stab_sum <- summary(stab_res)
  expect_s3_class(stab_res, "gstab_glm")
  expect_named(stab_sum, c("original_summary", "replication_stability_summary",
                           "statistical_stability_summary", "data_selection_stability_summary",
                           "model_selection_stability_summary", "numerical_stability_summary",
                           "analytic_and_algebraic_stability_summary", "technique_stability_summary"))
})
