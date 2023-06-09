


#' conduct power analyses for a user-specified grid of settings
#'
#' NOTE: to use powerms locally while developing, need to
#' devtools::install() the full package! (furrr doesn't play nicely,
#' some globals issue...), as described in
#' https://github.com/DavisVaughan/furrr/issues/95
#'
#' @param sim_data_method method for simulating data, default is the
#'   sim_data() method in the package which uses fabricatr to simulate
#'   a multisite trial
#' @param formula formula indicating outcome, treatment, and site ID
#'   column names from simulated data, in the following form:
#'   `outcome ~ treatment | site ID`. Default `Y ~ Z | sid`.
#' @param se_method = "pooled" or "individual": how to compute
#'   standard errors for the site-aggregated estimates
#' @param est_method function for analyzing site-aggregated estimates
#'   `tau_hat` with standard errors `se`, outputs left (`ci_l`) and
#'   right (`ci_r`) endpoints of interval estimates for each site.
#'   Built-in functions are run_mlm() and run_t_test()
#' @param num_sims number of simulations to run
#' @param parallel whether to turn on parallelization (via furrr)
#' @param ... additional arguments to sim_data_method. Can input
#'   multiple arguments to a parameter, e.g., n.bar = c(10, 20, 30),
#'   in which case the fully expanded grid of parameters will be
#'   simulated and assembled
#'
#' @export
powerms <- function(
    sim_data_method,
    formula = NULL,
    se_method = "pooled",
    est_method,
    num_sims = 100,
    parallel = F,
    ...) {

  stopifnot(is.function(sim_data_method))
  stopifnot(is.function(est_method))

  sim_data_args <- list(...)

  # check if user manually specified args_df
  if ("args_df" %in% names(sim_data_args)) {
    args_df <- sim_data_args$args_df
  } else {
    args_df <- expand.grid(sim_data_args, stringsAsFactors = F)
  }

  # if no arguments are supplied in sim_data_args,
  # manually generate args_df using default arguments to sim_data_method
  if (nrow(args_df) == 0) {
    args_df <- formals(sim_data_method) %>%
      as.list() %>%
      purrr::map(function(x) {
        if (is.call(x)) {
          # for vector-valued default arguments,
          # only grab first (default) element of vector as argument
          eval(x)[1]
        } else {
          x
        }
      }) %>%
      purrr::discard(is.null) %>%
      as.data.frame()
  }

  res_list <- purrr::map(
    1:nrow(args_df),
    function(i) {
      powerms_single(
        sim_data_method = sim_data_method,
        formula = formula,
        sim_data_args = as.list(args_df[i,]),
        se_method = se_method,
        est_method = est_method,
        num_sims = num_sims,
        parallel = parallel) %>%
        dplyr::mutate(sim_id = i, .before = rep_id)
    },
    .progress = paste("Running", nrow(args_df), "simulation settings..."))

  res <- purrr::list_rbind(res_list)

  attr(res, "sim_params") <- args_df %>%
    dplyr::mutate(sim_id = 1:dplyr::n(), .before=dplyr::everything())

  # class( res ) <- c( "powermsresult", class( res ) )
  class(res) <- c("powermsresult", "data.frame")

  res
}

