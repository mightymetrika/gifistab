plot_replication_stability <- function(obj, conf.int, ...) {
  original_estimates <- obj$original_summary
  new_estimates <- obj$replication_stability_summary$new_model
  boot_mean_estimates <- obj$replication_stability_summary$boot_models$boot_mean

  n_terms_original <- nrow(original_estimates)
  n_terms_new <- nrow(new_estimates)
  n_terms_boot <- nrow(boot_mean_estimates)

  p_new <- NULL
  if (n_terms_new > 0){
    plot_df_new <- data.frame(
      term = c(original_estimates$term, new_estimates$term),
      estimate = c(original_estimates$estimate, new_estimates$estimate),
      conf.low = if(conf.int) c(original_estimates$conf.low, new_estimates$conf.low) else c(rep(NA, n_terms_original), rep(NA, n_terms_new)),
      conf.high = if(conf.int) c(original_estimates$conf.high, new_estimates$conf.high) else c(rep(NA, n_terms_original), rep(NA, n_terms_new)),
      type = c(rep("Original", n_terms_original), rep("New", n_terms_new))
    )

    p_new <- ggplot2::ggplot(plot_df_new, ggplot2::aes(x = term, y = estimate, color = type)) +
      ggplot2::geom_point() +
      ggplot2::labs(x = "Term", y = "Estimate", color = "Type") +
      ggplot2::theme_minimal()

    if (conf.int){
      p_new <- p_new +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = conf.low, ymax = conf.high), width = 0.2)
    }
  }

  p_boot <- NULL
  if (!is.null(n_terms_boot)){
    plot_df_boot <- data.frame(
      term = c(original_estimates$term, boot_mean_estimates$term),
      estimate = c(original_estimates$estimate, boot_mean_estimates$estimate),
      conf.low = if(conf.int) c(original_estimates$conf.low, boot_mean_estimates$conf.low) else c(rep(NA, n_terms_original), rep(NA, n_terms_boot)),
      conf.high = if(conf.int) c(original_estimates$conf.high, boot_mean_estimates$conf.high) else c(rep(NA, n_terms_original), rep(NA, n_terms_boot)),
      type = c(rep("Original", n_terms_original), rep("Bootstrap Mean", n_terms_boot))
    )

    p_boot <- ggplot2::ggplot(plot_df_boot, ggplot2::aes(x = term, y = estimate, color = type)) +
      ggplot2::geom_point() +
      ggplot2::labs(x = "Term", y = "Estimate", color = "Type") +
      ggplot2::theme_minimal()

    if (conf.int){
      p_boot <- p_boot +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = conf.low, ymax = conf.high), width = 0.2)
    }
  }

  out <- list(new = p_new, boot = p_boot)
  return(out)
}


# Plot function for statistical stability
plot_statistical_stability <- function(obj, conf.int, ...) {
  original_estimates <- obj$original_summary
  noisy_estimates <- obj$statistical_stability_summary$noisy_model
  permuted_noisy_estimates <- obj$statistical_stability_summary$permuted_noisy_model

  n_terms_original <- nrow(original_estimates)
  n_terms_noisy <- nrow(noisy_estimates)
  n_terms_permuted_noisy <- nrow(permuted_noisy_estimates)

  # Create a data frame for plotting
  plot_df <- data.frame(
    term = c(original_estimates$term, noisy_estimates$term, permuted_noisy_estimates$term),
    estimate = c(original_estimates$estimate, noisy_estimates$estimate, permuted_noisy_estimates$estimate),
    conf.low = if(conf.int) c(original_estimates$conf.low, noisy_estimates$conf.low, permuted_noisy_estimates$conf.low) else c(rep(NA, n_terms_original), rep(NA, n_terms_noisy), rep(NA, n_terms_permuted_noisy)),
    conf.high = if(conf.int) c(original_estimates$conf.high, noisy_estimates$conf.high, permuted_noisy_estimates$conf.high) else c(rep(NA, n_terms_original), rep(NA, n_terms_noisy), rep(NA, n_terms_permuted_noisy)),
    type = c(rep("Original", n_terms_original), rep("Noisy", n_terms_noisy), rep("Permuted Noisy", n_terms_permuted_noisy))
  )

  # Create the plot
  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = term, y = estimate, color = type)) +
    ggplot2::geom_point() +
    ggplot2::labs(x = "Term", y = "Estimate", color = "Type") +
    ggplot2::theme_minimal()

  # Add error bars if confidence intervals are included
  if (conf.int){
    p <- p +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = conf.low, ymax = conf.high), width = 0.2)
  }

  return(p)
}

# Plot function for data selection stability
plot_data_selection_stability <- function(obj, conf.int, ...) {
  original_estimates <- obj$original_summary
  bootstrap_estimates <- obj$data_selection_stability_summary$bootstrap_model
  no_outlier_estimates <- obj$data_selection_stability_summary$no_outlier_model
  strata_boot_estimates1 <- obj$data_selection_stability_summary$strata_boot_model1
  strata_boot_estimates2 <- obj$data_selection_stability_summary$strata_boot_model2
  strata_boot_estimates3 <- obj$data_selection_stability_summary$strata_boot_model3


  n_terms_original <- nrow(original_estimates)
  n_terms_bootstrap <- nrow(bootstrap_estimates)
  n_terms_no_outlier <- nrow(no_outlier_estimates)
  n_terms_strata_boot1 <- nrow(strata_boot_estimates1)
  n_terms_strata_boot2 <- nrow(strata_boot_estimates2)
  n_terms_strata_boot3 <- nrow(strata_boot_estimates3)

  # Create a list to store the plots
  plot_list <- list()

  # Original vs Bootstrap
  plot_df_bootstrap <- data.frame(
    term = c(original_estimates$term, bootstrap_estimates$term),
    estimate = c(original_estimates$estimate, bootstrap_estimates$estimate),
    conf.low = if(conf.int) c(original_estimates$conf.low, bootstrap_estimates$conf.low) else c(rep(NA, n_terms_original), rep(NA, n_terms_bootstrap)),
    conf.high = if(conf.int) c(original_estimates$conf.high, bootstrap_estimates$conf.high) else c(rep(NA, n_terms_original), rep(NA, n_terms_bootstrap)),
    type = c(rep("Original", n_terms_original), rep("Bootstrap", n_terms_bootstrap))
  )

  plot_list[["bootstrap"]] <- create_plot(plot_df_bootstrap, conf.int)

  # Original vs No Outlier
  if (!is.null(no_outlier_estimates)){
    plot_df_no_outlier <- data.frame(
      term = c(original_estimates$term, no_outlier_estimates$term),
      estimate = c(original_estimates$estimate, no_outlier_estimates$estimate),
      conf.low = if(conf.int) c(original_estimates$conf.low, no_outlier_estimates$conf.low) else c(rep(NA, n_terms_original), rep(NA, n_terms_no_outlier)),
      conf.high = if(conf.int) c(original_estimates$conf.high, no_outlier_estimates$conf.high) else c(rep(NA, n_terms_original), rep(NA, n_terms_no_outlier)),
      type = c(rep("Original", n_terms_original), rep("No Outlier", n_terms_no_outlier))
    )

    plot_list[["no_outlier"]] <- create_plot(plot_df_no_outlier, conf.int)
  }

  # Original vs Stratified Bootstrap
  plot_df_strata_boot <- data.frame(
    term = c(original_estimates$term, strata_boot_estimates1$term, strata_boot_estimates2$term, strata_boot_estimates3$term),
    estimate = c(original_estimates$estimate, strata_boot_estimates1$estimate, strata_boot_estimates2$estimate, strata_boot_estimates3$estimate),
    conf.low = if(conf.int) c(original_estimates$conf.low,
                              strata_boot_estimates1$conf.low,
                              strata_boot_estimates2$conf.low,
                              strata_boot_estimates3$conf.low) else c(rep(NA, n_terms_original),
                                                                      rep(NA, n_terms_strata_boot1),
                                                                      rep(NA, n_terms_strata_boot2),
                                                                      rep(NA, n_terms_strata_boot3)),
    conf.high = if(conf.int) c(original_estimates$conf.high,
                              strata_boot_estimates1$conf.high,
                              strata_boot_estimates2$conf.high,
                              strata_boot_estimates3$conf.high) else c(rep(NA, n_terms_original),
                                                                      rep(NA, n_terms_strata_boot1),
                                                                      rep(NA, n_terms_strata_boot2),
                                                                      rep(NA, n_terms_strata_boot3)),
    #conf.high = if(conf.int) c(original_estimates$conf.high, strata_boot_estimates$conf.high) else c(rep(NA, n_terms_original), rep(NA, n_terms_strata_boot)),
    type = c(rep("Original", n_terms_original),
             rep("Strata 1 Bootstrap", n_terms_strata_boot1),
             rep("Strata 2 Bootstrap", n_terms_strata_boot2),
             rep("Strata 3 Bootstrap", n_terms_strata_boot3))
  )

  plot_list[["strata_bootstrap"]] <- create_plot(plot_df_strata_boot, conf.int)

  return(plot_list)
}

create_plot <- function(plot_df, conf.int) {
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

