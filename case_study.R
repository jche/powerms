
# script to replicate case study plots

# TIMING INFO
#
# 10 sims:
#  - 25 seconds without parallelization
#  - 40 seconds in parallel
#
# 100 sims:
#  - 147 seconds in parallel
#

res_norm <- powerms(
  sim_data_method = sim_data,
  se_method = "pooled",
  est_method = run_mlm,
  tx_var = "Z",
  outcome_var = "Y",
  site_id = "sid",
  num_sims = 100,
  parallel = T,

  outcome = "binary",
  intercept_dist = "normal",
  effect_dist = "normal",

  # site_sizes = list(c(551, 928, 895, 1008, 309),
  #                   c(551, 412, 343, 173, 464, 544, 499, 396, 197, 116)),
  site_sizes = list(c(551, 928, 895, 1008, 309)),
  pbar = 0.5,
  vary_site_ps = F,

  alpha = 0.175,
  sig_alpha = 0.01,
  tau = 0.03,
  sig_tau = c(0.01, 0.02, 0.03, 0.04, 0.05),
  # rho = c(0, 0.3, 0.6),
  rho = 0,

  cor_tau_n = 0,
  cor_tau_p = 0
)

# replicate Figure 3.10!
res_norm %>%
  add_sim_params() %>%
  force_sid_n_match() %>%
  dplyr::filter(rho == 0) %>%
  dplyr::group_by(sim_id, sig_tau, rho, sid) %>%
  dplyr::summarize(
    n = dplyr::first(n),
    avg_moe = mean(ci_r - ci_l) / 2) %>%
  ggplot2::ggplot(ggplot2::aes(
    x=n, y=avg_moe, group=sim_id, color=as.factor(sig_tau))) +
  ggplot2::geom_point() +
  ggplot2::geom_line()

#



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





