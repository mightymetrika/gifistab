test_that("stability_assessment works with the stats::lm engine", {
  n <- 20
  set.seed(376)
  data <- data.frame(y = 3*stats::rnorm(n) +5,
                     x1 = 3*stats::rnorm(n) +5 + stats::rnorm(n, 2, 0.3),
                     x2 = 2*stats::rnorm(n) + 1.5*stats::rnorm(n) + stats::rnorm(n, 1, 0.05))

  formula <- y ~ x1 + x2

  sa_res <- stability_assessment(data = data, formula = formula, engine = stats::lm)

  expect_equal(length(sa_res), 3)
})


test_that("stability_assessment works with the stats::glm engine", {
  # Generating example data
  n <- 20
  set.seed(376)
  data <- data.frame(y = rbinom(n, 1, 0.5),
                     x1 = 3*stats::rnorm(n) +5 + stats::rnorm(n, 2, 0.3),
                     x2 = 2*stats::rnorm(n) + 1.5*stats::rnorm(n) + stats::rnorm(n, 1, 0.05))
  formula <- y ~ x1 + x2

  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula, engine = stats::glm))

  expect_equal(length(sa_res), 3)
})
