# Plot function for replication stability
# plot_replication_stability <- function(obj, conf.int, ...) {
#   # Extract the estimates from the original model and the bootstrapped models
#   # original_estimates <- obj$original_summary$estimate
#   # boot_mean_estimates <- obj$replication_stability_summary$boot_models$boot_mean$estimate
#   original_estimates <- obj$original_summary
#   boot_mean_estimates <- obj$replication_stability_summary$boot_models$boot_mean
#
#   # Create a data frame for plotting
#   plot_df <- data.frame(
#     term = c(original_estimates$term, boot_mean_estimates$term),
#     estimate = c(original_estimates$estimate, boot_mean_estimates$estimate),
#     conf.low = if(conf.int) c(original_estimates$conf.low, boot_mean_estimates$conf.low) else NULL,
#     conf.high = if(conf.int) c(original_estimates$conf.high, boot_mean_estimates$conf.high) else NULL,
#     type = c(rep("Original", nrow(original_estimates)), rep("Bootstrap Mean", nrow(boot_mean_estimates)))
#   )
#
#   # Create the plot
#   p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = term, y = estimate, color = type)) +
#     ggplot2::geom_point() +
#     ggplot2::labs(x = "Term", y = "Estimate", color = "Type") +
#     ggplot2::theme_minimal()
#
#   if (conf.int){
#     p <- p +
#       ggplot2::geom_errorbar(ggplot2::aes(ymin = conf.low, ymax = conf.high), width = 0.2)
#   }
#
#   return(p)
# }
plot_replication_stability <- function(obj, conf.int, ...) {
  original_estimates <- obj$original_summary
  boot_mean_estimates <- obj$replication_stability_summary$boot_models$boot_mean

  n_terms_original <- nrow(original_estimates)
  n_terms_boot <- nrow(boot_mean_estimates)

  plot_df <- data.frame(
    term = c(original_estimates$term, boot_mean_estimates$term),
    estimate = c(original_estimates$estimate, boot_mean_estimates$estimate),
    conf.low = if(conf.int) c(original_estimates$conf.low, boot_mean_estimates$conf.low) else c(rep(NA, n_terms_original), rep(NA, n_terms_boot)),
    conf.high = if(conf.int) c(original_estimates$conf.high, boot_mean_estimates$conf.high) else c(rep(NA, n_terms_original), rep(NA, n_terms_boot)),
    type = c(rep("Original", n_terms_original), rep("Bootstrap Mean", n_terms_boot))
  )

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = term, y = estimate, color = type)) +
    ggplot2::geom_point() +
    ggplot2::labs(x = "Term", y = "Estimate", color = "Type") +
    ggplot2::theme_minimal()

  if (conf.int){
    p <- p +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = conf.low, ymax = conf.high), width = 0.2)
  }

  return(p)
}


