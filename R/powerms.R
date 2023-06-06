
# conduct power analysis for a single setting

### sim_data_method:
# input: sim_data_args (unlisted)
# output: individual-level dataset

### est_method:
# input: site-level dataset
# output: site-level dataset, with:
#   - estimated tau_j_hat
#   - lower & upper bounds of alpha% CI
powerms_single <- function(
    sim_data_method,
    sim_data_args,
    se_method = "pooled",
    est_method,
    tx_var = z,
    outcome_var = y,
    site_id = sid,
    num_sims = 100,
    ...
    ) {
  stopifnot(is.function(sim_data_method))

  res <- purrr::map_dfr(
    1:num_sims,
    function(x) {
      sim_data_method %>%
        do.call(sim_data_args) %>%
        summarize_sites(se = se_method,
                        tx_var = {{tx_var}},
                        outcome_var = {{outcome_var}},
                        site_id = {{site_id}}) %>%
        est_method() %>%
        dplyr::mutate(sim_id = x) %>%
        dplyr::select(sim_id, everything())
    },
    .progress="Simulating and analyzing datasets..."
  )

  attr(res, "sim_data_method") <- paste0(deparse(substitute(sim_data_method)), "()")
  attr(res, "est_method") <- paste0(deparse(substitute(est_method)), "()")
  attr(res, "sim_data_args") <- sim_data_args

  res
}

if (F) {
  foo <- powerms_single(
    sim_data_method = sim_b_nn,
    sim_data_args = list(
      site_sizes = rep(50, 10),
      alpha = 0,
      sig_alpha = 0.2,
      tau = 1,
      sig_tau = 0.2,
      rho = 0),
    se_method = "pooled",
    est_method = run_t_test,
    # est_method = run_mlm,
    num_sims = 10
  )

  summary_powerms_single(foo)

  # NOTE: need to load blkvar dependencies, e.g., dplyr,
  #  to make blkvar::generate_multilevel_data() work...
  library(dplyr)
  foo <- powerms_single(
    sim_data_method = blkvar::generate_multilevel_data,
    sim_data_args = list(
      n.bar = 25,
      J = 25,
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

  summary_powerms_single(foo)
}


