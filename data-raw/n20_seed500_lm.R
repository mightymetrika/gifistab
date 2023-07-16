## code to prepare `n20_seed500_lm` dataset goes here
n <- 20
set.seed(500)
n20_seed500_lm <- data.frame(y = 3*stats::rnorm(n) +5,
                       x1 = 3*stats::rnorm(n) +5 + stats::rnorm(n, 2, 0.3),
                       x2 = 2*stats::rnorm(n) + 1.5*stats::rnorm(n) + stats::rnorm(n, 1, 0.05),
                       x3 = stats::rpois(n,5)*stats::rnorm(n) + stats::rnorm(n, 1, 0.8),
                       x4 = factor(sample(1:4, n, replace = TRUE)))

usethis::use_data(n20_seed500_lm, overwrite = TRUE)
