test_that("stability_assessment works with the stats::lm engine", {
  formula <- y ~ x1 + x2
  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_lm,
                                                  formula = formula,
                                                  engine = stats::lm))
  expect_equal(length(sa_res), 4)
})


test_that("stability_assessment works with the stats::glm engine", {
  formula <- y ~ x1 + x2
  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_bin,
                                                  formula = formula,
                                                  engine = stats::glm,
                                                  family = binomial))
  expect_equal(length(sa_res), 4)
})


test_that("stability_assessment stats::lm engine works with interactions", {
  formula <- y ~ x1 + x2 + x3 + x1:x2

  # Test with no extra parameters
  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_lm,
                                                  formula = formula,
                                                  engine = stats::lm))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with bootstrap and new data
  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_lm,
                                                  formula = formula,
                                                  engine = stats::lm,
                                                  new_data = n20_seed500_lm,
                                                  nboot = 250))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with variable to remove
  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_lm, formula = formula,
                                                  engine = stats::lm,
                                                  variable_to_remove = "x1"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)
  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_lm, formula = formula,
                                                  engine = stats::lm,
                                                  variable_to_remove = "x2"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with variable of interest
  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_lm, formula = formula,
                                                  engine = stats::lm,
                                                  variable_of_interest = "x1"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)
  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_lm, formula = formula,
                                                  engine = stats::lm,
                                                  variable_of_interest = "x2"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with both variable to remove and variable of interest
  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_lm, formula = formula,
                                                  engine = stats::lm,
                                                  variable_to_remove = "x1",
                                                  variable_of_interest = "x2"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_lm, formula = formula,
                                                  engine = stats::lm,
                                                  variable_to_remove = "x2",
                                                  variable_of_interest = "x1"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with both variable to remove interaction
  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_lm, formula = formula,
                                                  engine = stats::lm,
                                                  variable_to_remove = "x1:x2"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with both variable to remove interaction
  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_lm, formula = formula,
                                                  engine = stats::lm,
                                                  variable_of_interest = "x1:x2"))
  expect_equal(length(sa_res), 4)
})

test_that("stability_assessment stats::glm engine works with interactions", {
  formula <- y ~ x1 + x2 + x3 + x1:x2

  # Test with no extra paramets
  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_bin,
                                                  formula = formula,
                                                  engine = stats::glm,
                                                  family = binomial))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with bootstrap and new data
  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_bin,
                                                  formula = formula,
                                                  engine = stats::glm,
                                                  new_data = n20_seed500_bin,
                                                  nboot = 250,
                                                  family = binomial))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with variable to remove
  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_bin,
                                                  formula = formula,
                                                  engine = stats::glm,
                                                  variable_to_remove = "x3",
                                                  family = binomial))
  expect_equal(length(sa_res), 4)
  rm(sa_res)
  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_bin,
                                                  formula = formula,
                                                  engine = stats::glm,
                                                  variable_to_remove = "x2",
                                                  family = binomial))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with variable of interest
  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_bin,
                                                  formula = formula,
                                                  engine = stats::glm,
                                                  variable_of_interest = "x3",
                                                  family = binomial))
  expect_equal(length(sa_res), 4)
  rm(sa_res)
  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_bin,
                                                  formula = formula,
                                                  engine = stats::glm,
                                                  variable_of_interest = "x2",
                                                  family = binomial))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with both variable to remove and variable of interest
  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_bin,
                                                  formula = formula,
                                                  engine = stats::glm,
                                                  variable_to_remove = "x2",
                                                  variable_of_interest = "x3",
                                                  family = binomial))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_bin,
                                                  formula = formula,
                                                  engine = stats::glm,
                                                  variable_to_remove = "x3",
                                                  variable_of_interest = "x2",
                                                  family = binomial))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with both variable to remove interaction
  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_bin,
                                                  formula = formula,
                                                  engine = stats::glm,
                                                  variable_to_remove = "x1:x2",
                                                  family = binomial))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with both variable to remove interaction
  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_bin,
                                                  formula = formula,
                                                  engine = stats::glm,
                                                  variable_of_interest = "x1:x2",
                                                  family = binomial))
  expect_equal(length(sa_res), 4)
  rm(sa_res)
})

test_that("stability_assessment stats::lm engine works with polynomials", {
  formula <- y ~ x1 + x2 + I(x2^2)

  # Test with no extra paramets
  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_lm,
                                                  formula = formula,
                                                  engine = stats::lm))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with bootstrap and new data
  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_lm,
                                                  formula = formula,
                                                  engine = stats::lm,
                                                  new_data = n20_seed500_lm,
                                                  nboot = 250))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with variable to remove
  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_lm,
                                                  formula = formula,
                                                  engine = stats::lm,
                                                  variable_to_remove = "x1"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)
  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_lm,
                                                  formula = formula,
                                                  engine = stats::lm,
                                                  variable_to_remove = "x2"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with variable of interest
  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_lm,
                                                  formula = formula,
                                                  engine = stats::lm,
                                                  variable_of_interest = "x1"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)
  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_lm,
                                                  formula = formula,
                                                  engine = stats::lm,
                                                  variable_of_interest = "x2"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with both variable to remove and variable of interest
  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_lm,
                                                  formula = formula,
                                                  engine = stats::lm,
                                                  variable_to_remove = "x1",
                                                  variable_of_interest = "x2"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_lm,
                                                  formula = formula,
                                                  engine = stats::lm,
                                                  variable_to_remove = "x2",
                                                  variable_of_interest = "x1"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  })

test_that("stability_assessment stats::glm engine works with polynomials", {
  formula <- y ~ x1 + x2 + I(x2^2)

  # Test with no extra paramets
  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_bin,
                                                  formula = formula,
                                                  engine = stats::glm,
                                                  family = binomial))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with bootstrap and new data
  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_bin,
                                                  formula = formula,
                                                  engine = stats::glm,
                                                  new_data = n20_seed500_bin,
                                                  nboot = 250,
                                                  family = binomial))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with variable to remove
  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_bin,
                                                  formula = formula,
                                                  engine = stats::glm,
                                                  variable_to_remove = "x1",
                                                  family = binomial))
  expect_equal(length(sa_res), 4)
  rm(sa_res)
  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_bin,
                                                  formula = formula,
                                                  engine = stats::glm,
                                                  variable_to_remove = "x2",
                                                  family = binomial))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with variable of interest
  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_bin,
                                                  formula = formula,
                                                  engine = stats::glm,
                                                  variable_of_interest = "x1",
                                                  family = binomial))
  expect_equal(length(sa_res), 4)
  rm(sa_res)
  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_bin,
                                                  formula = formula,
                                                  engine = stats::glm,
                                                  variable_of_interest = "x2",
                                                  family = binomial))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with both variable to remove and variable of interest
  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_bin,
                                                  formula = formula,
                                                  engine = stats::glm,
                                                  variable_to_remove = "x1",
                                                  variable_of_interest = "x2",
                                                  family = binomial))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_bin,
                                                  formula = formula,
                                                  engine = stats::glm,
                                                  variable_to_remove = "x2",
                                                  variable_of_interest = "x1",
                                                  family = binomial))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

})

test_that("stability_assessment stats::lm engine works with factors", {
  formula <- y ~ x1 + x4

  # Test with no extra paramets
  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_lm,
                                                  formula = formula,
                                                  engine = stats::lm))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with bootstrap and new data
  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_lm,
                                                  formula = formula,
                                                  engine = stats::lm,
                                                  new_data = n20_seed500_lm,
                                                  nboot = 250))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with variable to remove
  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_lm,
                                                  formula = formula,
                                                  engine = stats::lm,
                                                  variable_to_remove = "x1"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)
  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_lm,
                                                  formula = formula,
                                                  engine = stats::lm,
                                                  variable_to_remove = "x4"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with variable of interest
  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_lm,
                                                  formula = formula,
                                                  engine = stats::lm,
                                                  variable_of_interest = "x1"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)
  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_lm,
                                                  formula = formula,
                                                  engine = stats::lm,
                                                  variable_of_interest = "x4"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with both variable to remove and variable of interest
  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_lm,
                                                  formula = formula,
                                                  engine = stats::lm,
                                                  variable_to_remove = "x1",
                                                  variable_of_interest = "x4"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_lm,
                                                  formula = formula,
                                                  engine = stats::lm,
                                                  variable_to_remove = "x4",
                                                  variable_of_interest = "x1"))
  expect_equal(length(sa_res), 4)
  rm(sa_res)
})

test_that("stability_assessment stats::glm engine works with factors", {
  formula <- y ~ x1 + x4

  # Test with no extra paramets
  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_bin,
                                                  formula = formula,
                                                  engine = stats::glm,
                                                  family = binomial))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with bootstrap and new data
  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_bin,
                                                  formula = formula,
                                                  engine = stats::glm,
                                                  new_data = n20_seed500_bin,
                                                  nboot = 250,
                                                  family = binomial))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with variable to remove
  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_bin,
                                                  formula = formula,
                                                  engine = stats::glm,
                                                  variable_to_remove = "x1",
                                                  family = binomial))
  expect_equal(length(sa_res), 4)
  rm(sa_res)
  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_bin,
                                                  formula = formula,
                                                  engine = stats::glm,
                                                  variable_to_remove = "x4",
                                                  family = binomial))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with variable of interest
  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_bin,
                                                  formula = formula,
                                                  engine = stats::glm,
                                                  variable_of_interest = "x1",
                                                  family = binomial))
  expect_equal(length(sa_res), 4)
  rm(sa_res)
  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_bin,
                                                  formula = formula,
                                                  engine = stats::glm,
                                                  variable_of_interest = "x4",
                                                  family = binomial))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  # Test with both variable to remove and variable of interest
  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_bin,
                                                  formula = formula,
                                                  engine = stats::glm,
                                                  variable_to_remove = "x1",
                                                  variable_of_interest = "x4",
                                                  family = binomial))
  expect_equal(length(sa_res), 4)
  rm(sa_res)

  sa_res <- suppressWarnings(stability_assessment(data = n20_seed376_bin,
                                                  formula = formula,
                                                  engine = stats::glm,
                                                  variable_to_remove = "x4",
                                                  variable_of_interest = "x1",
                                                  family = binomial))
  expect_equal(length(sa_res), 4)
  rm(sa_res)
})
