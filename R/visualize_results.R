
# functions to aid visualization of results


# given powerms() output, generate reasonable plot
moe_plot <- function(p, x_axis, grouping=NULL) {
  add_sim_params(p) %>%
    dplyr::group_by({{x_axis}}, {{grouping}}, sim_id) %>%
    dplyr::summarize(avg_moe = mean(ci_r - ci_l)) %>%
    ggplot2::ggplot(ggplot2::aes(x={{x_axis}}, y=avg_moe,
                                 color={{grouping}}, group={{grouping}})) +
    ggplot2::geom_line() +
    ggplot2::geom_point()
}


if (F) {
  library(dplyr)
  foo <- powerms(
    sim_data_method = blkvar::generate_multilevel_data,
    sim_data_args = list(
      n.bar = c(25, 50),
      J = c(25, 50),
      p = 0.5,
      tau.11.star = 0.3,   # Total amount of cross-site treatment variation
      rho2.0W = 0.1,  # Explanatory power of W for control outcomes
      rho2.1W = 0.5,  # Explanatory power of W for average treatment impact
      ICC = 0.7,   # ICC; 1-sigma2.e
      gamma.00 = 0,
      gamma.10 = 0.2,
      zero.corr = F,
      variable.n = T),
    se_method = "pooled",
    est_method = run_t_test,
    # est_method = run_mlm,
    tx_var = Z,
    outcome_var = Yobs,
    site_id = sid,
    num_sims = 10
  )

  moe_plot(foo, x=n.bar, grouping=J)
}


