## code to prepare `n20_seed500_pois` dataset goes here
n <- 20
set.seed(500)
n20_seed500_pois <- data.frame(y = stats::rpois(n, 10),
                       x1 = 3*stats::rnorm(n) +5 + stats::rnorm(n, 2, 0.3),
                       x2 = 2*stats::rnorm(n) + 1.5*stats::rnorm(n) + stats::rnorm(n, 1, 0.05))

usethis::use_data(n20_seed500_pois, overwrite = TRUE)
