test_that("Test that stab_explainer.gstab_lm works", {

  formula <- y ~ x1 + x2
  model <- stats::lm(formula, data = n20_seed376_lm)

  #Test with nboot
  stab_res <- gstab(model = model, nboot = 100)
  stab_exp <- stab_explainer.gstab_lm(stab_res)
  expect_equal(length(stab_exp), 8)
})
