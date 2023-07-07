
#' conduct power analysis for a single setting
#'
#' @param sim_data_method method for simulating data, default is the
#'   sim_data() method in the package which uses fabricatr to simulate
#'   a multisite trial
#' @param formula indicating outcome, treatment, and site ID
#'   column names from simulated data, in the following form:
#' @param sim_data_args list of arguments for sim_data_method
#' @param se_method = "pooled" or "individual": how to compute
#'   standard errors for the site-aggregated estimates
#' @param est_method function for analyzing site-aggregated estimates
#'   `tau_hat` with standard errors `se`, outputs left (`ci_l`) and
#'   right (`ci_r`) endpoints of interval estimates for each site.
#'   Built-in functions are run_mlm() and run_t_test()
#' @param num_sims number of simulations to run
#' @param parallel whether to turn on parallelization (via furrr)
#'
#' @return individual-level dataset
#'
powerms_single <- function(
    sim_data_method,
    formula = NULL,
    sim_data_args,
    se_method = "pooled",
    est_method,
    num_sims = 100,
    parallel = F) {
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

  # set up parallelization
  if (parallel) {
    ncores <- future::availableCores() - 1
    future::plan(future::multisession, workers = ncores)
  } else {
    future::plan(future::sequential)
  }

  res <- furrr::future_map(
    1:num_sims,
    function(x) {
      sim_data_method %>%
        do.call(sim_data_args) %>%
        summarize_sites(
          formula = formula,
          se = se_method) %>%
        est_method() %>%
        dplyr::mutate(rep_id = x) %>%
        dplyr::select(rep_id, dplyr::everything())
    },
    # .progress=T,
    .options = furrr::furrr_options(seed = T)
  ) %>%
    purrr::list_rbind()

  attr(res, "sim_data_method") <- paste0(deparse(substitute(sim_data_method)), "()")
  attr(res, "est_method") <- paste0(deparse(substitute(est_method)), "()")
  attr(res, "sim_data_args") <- sim_data_args

  res
}


