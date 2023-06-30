

#' Simulate a dataset
#'
#' functions to simulate datasets:
#'
#'  - input: all parameters of dataset
#'  - output: individual-level dataset
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

  # if we've specified either site_sizes or site_ps,
  # need to ensure that J is correct before generating the other.

  # ensure J matches with manually specified site sizes/ps
  if (!is.null(site_sizes) | !is.null(site_ps)) {
    if (!is.null(site_sizes) & !is.null(site_ps)) {
      stopifnot(length(site_sizes) == length(site_ps))
    }
    J <- max(length(site_sizes), length(site_ps))
  }

  # generate raw vector of site sizes
  if (is.null(site_sizes)) {
    if (vary_site_sizes) {
      site_sizes <- gen_site_sizes(nbar, J, size_ratio)
    } else {
      site_sizes <- rep(nbar, J)
    }
  }

  # generate raw vector of site proportions treated
  if (is.null(site_ps)) {
    if (vary_site_ps) {
      ths <- min(pbar, 1 - pbar) * 0.75
      site_ps <- runif(J, pbar - ths, pbar + ths)
    } else {
      site_ps <- rep(pbar, J)
    }
  }

  # generate dataset
  fabricatr::fabricate(
    sid = fabricatr::add_level(
      N = J,
      alpha_j = if (intercept_dist == "normal") {
        rnorm(J, mean=alpha, sd=sig_alpha)
      } else {
        stop("Non-normal intercept distributions not currently implemented")
      },

      tau_j = if (effect_dist == "normal") {
        # generate tau_j, potentially correlated with alpha_j
        mu <- tau + rho*sig_tau/sig_alpha * (alpha_j-alpha)
        var <- (1-rho^2) * sig_tau^2
        rnorm(J, mean=mu, sd=sqrt(var))
      } else if (effect_dist == "gamma") {
        stopifnot("Gamma effect distribution requires parameter a" = !is.null(a))
        stopifnot("Gamma effect distribution requires parameter b" = !is.null(b))
        if (rho != 0) {
          warning("Correlation between intercept and effect not implemented for Gamma effect distribution!")
        }
        rgamma(J, shape=a, rate=b)
      },

      n_j = {
        if (sd(site_sizes) > 0) {
          # induce correlation between tau_j and n_j
          n_j_temp <- site_sizes[order(
            cor_tau_n^2 * site_sizes + (1-cor_tau_n^2) * rnorm(J, sd=sd(site_sizes)) )]
          # magic hack: avoids sorting tau_j
          n_j_temp[order(order(sign(cor_tau_n) * tau_j))]
        } else {
          site_sizes
        }
      },
      p_j = {
        if (sd(site_ps) > 0) {
          p_j_temp <- site_ps[order(
            cor_tau_p^2 * site_ps + (1-cor_tau_p^2) * rnorm(J, sd=sd(site_ps)) )]
          p_j_temp[ order(order(sign(cor_tau_p)*tau_j)) ]
        } else {
          site_ps
        }
      }
    ),

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
      },
      Z = rbinom(n_j, size=1, p=p_j),
      Y = ifelse(Z, Y1, Y0)
    )
  ) %>%
    ensure_one_tx_co()
}


# assumes names sid, Z
ensure_one_tx_co <- function(d) {
  d %>%
    split(f = d$sid) %>%
    purrr::map(function(x) {
      stopifnot("Only one unit in site! Cannot ensure a treated and a control unit." =
                  nrow(x) > 1)
      s <- sum(x$Z)
      if (s == 0 | s == nrow(x)) {
        temp <- x$Z
        temp[1] <- as.numeric(!temp[1])
        x$Z <- temp
      }
      x
    }) %>%
    purrr::list_rbind()
}











