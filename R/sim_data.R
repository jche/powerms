
#' Simulate a dataset
#'
#' functions to simulate datasets:
#'
#'  - input: all parameters of dataset
#'  - output: individual-level dataset
#'
#'  if site_sizes or site_ps specified, tie it to site ID, in order!
#'
#' @export
sim_data <- function(
    outcome = c("continuous", "binary"),
    intercept_dist = c("normal"),
    effect_dist = c("normal", "gamma"),

    J = 30,

    # site size parameters
    site_sizes = NULL,
    nbar = 10,
    vary_site_sizes = F,
    size_ratio = 1/3,

    # site proportion treated parameters
    site_ps = NULL,
    pbar = 0.5,
    vary_site_ps = F,

    # intercept distribution parameters
    alpha = 0,
    sig_alpha = sqrt(0.2),

    # effect distribution parameters
    tau = 0.2,
    sig_tau = sqrt(0.2),
    rho = 0,
    a = NULL,
    b = NULL,

    # site size & prop. treated parameters
    cor_tau_n = 0,
    cor_tau_p = 0,

    # observation-level parameters
    ICC = 0.3) {

  outcome <- match.arg(outcome)
  intercept_dist <- match.arg(intercept_dist)
  effect_dist <- match.arg(effect_dist)

  stopifnot("cor_tau_n must be in (-1,1)" = cor_tau_n >= -1 & cor_tau_n <= 1)
  stopifnot("cor_tau_p must be in (-1,1)" = cor_tau_p >= -1 & cor_tau_p <= 1)

  # compute appropriate J value
  if (!(is.null(site_sizes) & is.null(site_ps))) {
    J <- max(length(site_sizes), length(site_ps))
  }
  if (!is.null(site_sizes) & !is.null(site_ps)) {
    stopifnot(length(site_sizes) == length(site_ps))
  }

  # generate site intercepts, effects, sizes, and/or tx probabilities,
  # as needed, with appropriate correlations
  site_params <- gen_site_params(
    J, intercept_dist, effect_dist,
    alpha=alpha, sig_alpha=sig_alpha,
    tau=tau, sig_tau=sig_tau, rho=rho,
    a=a, b=b)
  site_intercepts <- site_params$alpha_j
  site_taus <- site_params$tau_j

  if (is.null(site_sizes) & is.null(site_ps)) {
    site_sizes <- gen_site_sizes(nbar, J, size_ratio, vary=vary_site_sizes) %>%
      induce_correlation(site_taus, cor_tau_n)
    site_ps <- gen_site_ps(pbar, J, vary=vary_site_ps) %>%
      induce_correlation(site_taus, cor_tau_p)
  } else if (is.null(site_ps)) {
    site_taus <- site_taus %>%
      induce_correlation(site_sizes, cor_tau_n)
    site_ps <- gen_site_ps(pbar, J, vary=vary_site_ps) %>%
      induce_correlation(site_taus, cor_tau_p)
  } else if (is.null(site_sizes)) {
    site_taus <- site_taus %>%
      induce_correlation(site_ps, cor_tau_p)
    site_sizes <- gen_site_sizes(nbar, J, size_ratio, vary=vary_site_sizes) %>%
      induce_correlation(site_taus, cor_tau_n)
  } else {
    if (cor_tau_n != cor_tau_p) {
      stop("User assigned fixed site sizes and treatment probabilities;
           cannot induce separate cor_tau_n and cor_tau_p values!")
    }
    site_taus <- site_taus %>%
      induce_correlation(site_sizes, cor_tau_n)
  }

  fabricatr::fabricate(
    sid = fabricatr::add_level(
      N = J,
      alpha_j = site_intercepts,
      tau_j = site_taus,
      n_j = site_sizes,
      p_j = site_ps),
    i = fabricatr::add_level(
      N = n_j,
      Y0 = if (outcome == "continuous") {
        fabricatr::draw_normal_icc(
          mean = alpha_j,
          clusters = sid,
          total_sd = 1,
          ICC = ICC)
      } else {   # binary outcome
        rbinom(n_j, size=1, prob=floor_ceil(alpha_j))
      },
      Y1 = if (outcome == "continuous") {
        Y0 + tau_j
      } else {
        rbinom(n_j, size=1, prob=floor_ceil(alpha_j + tau_j))
      }
    )
  ) %>%
    assign_treatment()
}

assign_treatment <- function(d) {
  d %>%
    dplyr::mutate(
      Z = randomizr::block_ra(blocks = sid, prob_unit = p_j),
      Y = ifelse(Z, Y1, Y0))
}
