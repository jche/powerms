
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
    num_sims = 100) {
  stopifnot(is.function(sim_data_method))
  stopifnot(is.function(est_method))

  # hacky workaround: expand.grid() in powerms ends up nesting
  # vector parameters (e.g., site.sizes) one layer too deep in the list,
  # so we unlist them here.
  sim_data_args <- purrr::map(sim_data_args,
                              function(a) {
                                if (is.list(a)) {return(a[[1]])}
                                a
                              })

  if (exists("DEBUGGING")) { browser() }

  res <- purrr::map(
    1:num_sims,
    function(x) {
      sim_data_method %>%
        do.call(sim_data_args) %>%
        summarize_sites(se = se_method,
                        tx_var = {{tx_var}},
                        outcome_var = {{outcome_var}},
                        site_id = {{site_id}}) %>%
        est_method() %>%
        dplyr::mutate(rep_id = x) %>%
        dplyr::select(rep_id, everything())
    },
    .progress="Simulating and analyzing datasets..."
  ) %>%
    purrr::list_rbind()

  attr(res, "sim_data_method") <- paste0(deparse(substitute(sim_data_method)), "()")
  attr(res, "est_method") <- paste0(deparse(substitute(est_method)), "()")
  attr(res, "sim_data_args") <- sim_data_args

  res
}

if (F) {

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


