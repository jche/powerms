
# script to replicate case study plots

library(dplyr)

foo <- powerms_single(
  sim_data_method = sim_data,
  se_method = "pooled",
  est_method = run_t_test,
  tx_var = Z,
  outcome_var = Yobs,
  site_id = sid,
  num_sims = 10,

  sim_data_args = list(
    outcome = "binary",
    intercept_dist = "normal",
    effect_dist = "normal",

    J = 15,
    nbar = 1000,
    vary_site_sizes = F,
    pbar = 0.5,
    vary_site_ps = F,

    alpha = 0.175,
    sig_alpha = 0.01,
    tau = 0.03,
    sig_tau = 0.03,
    a = 1.5,
    b = 50,

    cor_tau_n = 0,
    cor_tau_p = 0
  )
)

res_norm <- powerms(
  sim_data_method = sim_data,
  se_method = "pooled",
  est_method = run_t_test,
  tx_var = Z,
  outcome_var = Yobs,
  site_id = sid,
  num_sims = 3,

  outcome = "binary",
  intercept_dist = "normal",
  effect_dist = "normal",

  site_sizes = list(c(551, 928, 895, 1008, 309),
                    c(551, 412, 343, 173, 464, 544, 499, 396, 197, 116)),
  pbar = 0.5,
  vary_site_ps = F,

  alpha = 0.175,
  sig_alpha = 0.01,
  tau = 0.03,
  sig_tau = c(0.01, 0.02, 0.03, 0.04, 0.05),
  rho = c(0, 0.3, 0.6),

  cor_tau_n = 0,
  cor_tau_p = 0
)

res_norm %>%
  filter()

summary_powerms_single(res_norm)

# TODO: add summary method / plotting method which groups for each site
#  - in general, more support for individual-site stuff

# CHALLENGE: paper does analysis FOR EACH SITE!
#  - since site sizes are fixed!
# TODO: do this grouping when site sizes are specified
#  (i.e., when sites are no longer exchangeable)

foo %>%
  moe_plot(x=tau.11, grouping=tau.01)

moe_plot(foo, x=n.bar, grouping=J)



# set site sample sizes
site_sizes <- c(551, 412, 343, 173, 464, 544, 499, 396, 197, 116)
site_sizes <- c(551, 928, 895, 1008, 309)

# set data-generating parameters (note: no ICC)
tau <- 0.03
b <- 50
alpha <- 0.175
sig_alpha <- 0.01
ICC <- 1





