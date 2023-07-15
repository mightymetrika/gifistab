test_that("stability_assessment works with the stats::lm engine", {
  n <- 20
  set.seed(376)
  data <- data.frame(y = 3*stats::rnorm(n) +5,
                     x1 = 3*stats::rnorm(n) +5 + stats::rnorm(n, 2, 0.3),
                     x2 = 2*stats::rnorm(n) + 1.5*stats::rnorm(n) + stats::rnorm(n, 1, 0.05))

  formula <- y ~ x1 + x2

  sa_res <- stability_assessment(data = data, formula = formula, engine = stats::lm)

  expect_equal(length(sa_res), 4)
})


test_that("stability_assessment works with the stats::glm engine", {
  # Generating example data
  n <- 20
  set.seed(350)
  data <- data.frame(y = rbinom(n, 1, 0.5),
                     x1 = 3*stats::rnorm(n) +5 + stats::rnorm(n, 2, 0.3),
                     x2 = 2*stats::rnorm(n) + 1.5*stats::rnorm(n) + stats::rnorm(n, 1, 0.05))
  formula <- y ~ x1 + x2

  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula, engine = stats::glm))

  expect_equal(length(sa_res), 4)
})


test_that("stability_assessment stats::lm engine works with interactions", {
  # Generating example data
  n <- 20
  set.seed(376)
  data <- data.frame(y = rnorm(n),
                     x1 = 3*stats::rnorm(n) +5 + stats::rnorm(n, 2, 0.3),
                     x2 = 2*stats::rnorm(n) + 1.5*stats::rnorm(n) + stats::rnorm(n, 1, 0.05),
                     x3 = stats::rpois(n,5)*stats::rnorm(n) + stats::rnorm(n, 1, 0.05))
  formula <- y ~ x1 + x2 + x3 + x1:x2

  # Test with no extra paramets
  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula,
                                                  engine = stats::lm))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with bootstrap and new data
  new_data <- data.frame(y = rnorm(n) + stats::rpois(n, 2),
                     x1 = 3*stats::rnorm(n),
                     x2 = 2*stats::rnorm(n) + 1.5*stats::rnorm(n) + stats::rnorm(n, 1, 0.05),
                     x3 = stats::rpois(n,5)*stats::rnorm(n) + stats::rnorm(n, 1, 0.8))
  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula,
                                                  engine = stats::lm,
                                                  new_data = new_data,
                                                  nboot = 250))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with variable to remove
  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula,
                                                  engine = stats::lm,
                                                  variable_to_remove = "x1"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)
  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula,
                                                  engine = stats::lm,
                                                  variable_to_remove = "x2"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with variable of interest
  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula,
                                                  engine = stats::lm,
                                                  variable_of_interest = "x1"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)
  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula,
                                                  engine = stats::lm,
                                                  variable_of_interest = "x2"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with both variable to remove and variable of interest
  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula,
                                                  engine = stats::lm,
                                                  variable_to_remove = "x1",
                                                  variable_of_interest = "x2"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula,
                                                  engine = stats::lm,
                                                  variable_to_remove = "x2",
                                                  variable_of_interest = "x1"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with both variable to remove interaction
  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula,
                                                  engine = stats::lm,
                                                  variable_to_remove = "x1:x2"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with both variable to remove interaction
  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula,
                                                  engine = stats::lm,
                                                  variable_of_interest = "x1:x2"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

})

test_that("stability_assessment stats::glm engine works with interactions", {
  # Generating example data
  n <- 20
  set.seed(320)
  data <- data.frame(y = rbinom(n, 1, 0.5),
                     x1 = 3*stats::rnorm(n) +5 + stats::rnorm(n, 2, 0.3),
                     x2 = 2*stats::rnorm(n) + 1.5*stats::rnorm(n) + stats::rnorm(n, 1, 0.05),
                     x3 = stats::rpois(n,5)*stats::rnorm(n) + stats::rnorm(n, 1, 0.05))
  formula <- y ~ x1 + x2 + x3 + x1:x2

  # Test with no extra paramets
  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula,
                                                  engine = stats::glm))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with bootstrap and new data
  set.seed(320)
  new_data <- data.frame(y = stats::rpois(n, 2),
                         x1 = 3*stats::rnorm(n),
                         x2 = 2*stats::rnorm(n) + 1.5*stats::rnorm(n) + stats::rnorm(n, 1, 0.05),
                         x3 = stats::rpois(n,5)*stats::rnorm(n) + stats::rnorm(n, 1, 0.8))
  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula,
                                                  engine = stats::glm,
                                                  new_data = new_data,
                                                  nboot = 250))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with variable to remove
  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula,
                                                  engine = stats::glm,
                                                  variable_to_remove = "x3"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)
  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula,
                                                  engine = stats::glm,
                                                  variable_to_remove = "x2"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with variable of interest
  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula,
                                                  engine = stats::glm,
                                                  variable_of_interest = "x3"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)
  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula,
                                                  engine = stats::glm,
                                                  variable_of_interest = "x2"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with both variable to remove and variable of interest
  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula,
                                                  engine = stats::glm,
                                                  variable_to_remove = "x2",
                                                  variable_of_interest = "x3"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula,
                                                  engine = stats::glm,
                                                  variable_to_remove = "x3",
                                                  variable_of_interest = "x2"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with both variable to remove interaction
  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula,
                                                  engine = stats::glm,
                                                  variable_to_remove = "x1:x2"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with both variable to remove interaction
  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula,
                                                  engine = stats::glm,
                                                  variable_of_interest = "x1:x2"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)
})

test_that("stability_assessment stats::lm engine works with polynomials", {
  # Generating example data
  n <- 20
  set.seed(376)
  data <- data.frame(y = rnorm(n),
                     x1 = 3*stats::rnorm(n) +5 + stats::rnorm(n, 2, 0.3),
                     x2 = 2*stats::rnorm(n) + 1.5*stats::rnorm(n) + stats::rnorm(n, 1, 0.05))
  formula <- y ~ x1 + x2 + I(x2^2)

  # Test with no extra paramets
  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula,
                                                  engine = stats::lm))
  expect_equal(length(sa_res), 4)

  rm(sa_res)

  # Test with bootstrap and new data
  new_data <- data.frame(y = rnorm(n) + stats::rpois(n, 2),
                         x1 = 3*stats::rnorm(n),
                         x2 = 2*stats::rnorm(n) + 1.5*stats::rnorm(n) + stats::rnorm(n, 1, 0.05))
  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula,
                                                  engine = stats::lm,
                                                  new_data = new_data,
                                                  nboot = 250))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with variable to remove
  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula,
                                                  engine = stats::lm,
                                                  variable_to_remove = "x1"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)
  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula,
                                                  engine = stats::lm,
                                                  variable_to_remove = "x2"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with variable of interest
  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula,
                                                  engine = stats::lm,
                                                  variable_of_interest = "x1"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)
  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula,
                                                  engine = stats::lm,
                                                  variable_of_interest = "x2"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with both variable to remove and variable of interest
  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula,
                                                  engine = stats::lm,
                                                  variable_to_remove = "x1",
                                                  variable_of_interest = "x2"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula,
                                                  engine = stats::lm,
                                                  variable_to_remove = "x2",
                                                  variable_of_interest = "x1"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  })

test_that("stability_assessment stats::glm engine works with polynomials", {
  # Generating example data
  n <- 20
  set.seed(320)
  data <- data.frame(y = rbinom(n, 1, 0.5),
                     x1 = 3*stats::rnorm(n) +5 + stats::rnorm(n, 2, 0.3),
                     x2 = 2*stats::rnorm(n) + 1.5*stats::rnorm(n) + stats::rnorm(n, 1, 0.05))
  formula <- y ~ x1 + x2 + I(x2^2)

  # Test with no extra paramets
  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula,
                                                  engine = stats::glm))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with bootstrap and new data
  set.seed(320)
  new_data <- data.frame(y = stats::rpois(n, 2),
                         x1 = 3*stats::rnorm(n),
                         x2 = 2*stats::rnorm(n) + 1.5*stats::rnorm(n) + stats::rnorm(n, 1, 0.05))
  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula,
                                                  engine = stats::lm,
                                                  new_data = new_data,
                                                  nboot = 250))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with variable to remove
  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula,
                                                  engine = stats::glm,
                                                  variable_to_remove = "x1"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)
  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula,
                                                  engine = stats::glm,
                                                  variable_to_remove = "x2"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with variable of interest
  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula,
                                                  engine = stats::glm,
                                                  variable_of_interest = "x1"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)
  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula,
                                                  engine = stats::glm,
                                                  variable_of_interest = "x2"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with both variable to remove and variable of interest
  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula,
                                                  engine = stats::glm,
                                                  variable_to_remove = "x1",
                                                  variable_of_interest = "x2"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula,
                                                  engine = stats::glm,
                                                  variable_to_remove = "x2",
                                                  variable_of_interest = "x1"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

})

test_that("stability_assessment stats::lm engine works with factors", {
  # Generating example data
  n <- 100
  set.seed(376)
  data <- data.frame(
    y = stats::rnorm(n),
    x1 = 3 * stats::rnorm(n) + 5 + stats::rnorm(n, 2, 0.3),
    x2 = factor(sample(1:4, n, replace = TRUE))
  )
  formula <- y ~ x1 + x2

  # Test with no extra paramets
  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula,
                                                  engine = stats::lm))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with bootstrap and new data
  new_data <- data.frame(
    y = stats::rnorm(n),
    x1 = 2.3 * stats::rnorm(n) + 4.7 + stats::rnorm(n, 1.7, 0.41),
    x2 = factor(sample(1:4, n, replace = TRUE))
  )
  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula,
                                                  engine = stats::lm,
                                                  new_data = new_data,
                                                  nboot = 250))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with variable to remove
  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula,
                                                  engine = stats::lm,
                                                  variable_to_remove = "x1"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)
  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula,
                                                  engine = stats::lm,
                                                  variable_to_remove = "x2"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with variable of interest
  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula,
                                                  engine = stats::lm,
                                                  variable_of_interest = "x1"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)
  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula,
                                                  engine = stats::lm,
                                                  variable_of_interest = "x2"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with both variable to remove and variable of interest
  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula,
                                                  engine = stats::lm,
                                                  variable_to_remove = "x1",
                                                  variable_of_interest = "x2"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula,
                                                  engine = stats::lm,
                                                  variable_to_remove = "x2",
                                                  variable_of_interest = "x1"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)
})

test_that("stability_assessment stats::glm engine works with factors", {
  # Generating example data
  n <- 100
  set.seed(320)
  data <- data.frame(y = rbinom(n, 1, 0.5),
                     x1 = 3*stats::rnorm(n) +5 + stats::rnorm(n, 2, 0.3),
                     x2 = factor(sample(1:4, n, replace = TRUE)))
  formula <- y ~ x1 + x2

  # Test with no extra paramets
  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula,
                                                  engine = stats::glm))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with bootstrap and new data
  set.seed(320)
  new_data <- data.frame(y = stats::rpois(n, 2),
                         x1 = 3*stats::rnorm(n),
                         x2 = factor(sample(1:4, n, replace = TRUE)))
  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula,
                                                  engine = stats::glm,
                                                  new_data = new_data,
                                                  nboot = 250))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with variable to remove
  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula,
                                                  engine = stats::glm,
                                                  variable_to_remove = "x1"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)
  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula,
                                                  engine = stats::glm,
                                                  variable_to_remove = "x2"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with variable of interest
  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula,
                                                  engine = stats::glm,
                                                  variable_of_interest = "x1"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)
  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula,
                                                  engine = stats::glm,
                                                  variable_of_interest = "x2"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with both variable to remove and variable of interest
  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula,
                                                  engine = stats::glm,
                                                  variable_to_remove = "x1",
                                                  variable_of_interest = "x2"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  sa_res <- suppressWarnings(stability_assessment(data = data, formula = formula,
                                                  engine = stats::glm,
                                                  variable_to_remove = "x2",
                                                  variable_of_interest = "x1"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)
})
