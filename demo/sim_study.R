
# simulation study conducted in paper, with plots
library( powerms )


fig3.6 <- powerms(
  sim_data_method = sim_data,
  formula = Y ~ Z | sid,
  se_method = "pooled",
  est_method = powerms:::run_mlm,
  num_sims = 7,
  parallel = F,

  outcome = "continuous",
  intercept_dist = "normal",
  effect_dist = "normal",

  J = c(10, 25, 100),
  nbar = seq(5, 200, by=5),
  vary_site_sizes = T,
  pbar = 0.5,
  vary_site_ps = F,

  alpha = 0,
  sig_alpha = 0.2,
  # tau = c(0.1, 0.2, 0.3),
  # sig_tau = c(0.1, 0.2, 0.3),
  # ICC = c(0.1, 0.2, 0.3),
  tau = 0.2,
  sig_tau = 0.2,
  ICC = 0.2,

  rho = 0,
  cor_tau_n = 0,
  cor_tau_p = 0
)

readr::write_rds(fig3.6, "sim_study_mlm36.rds")

# replicate Figure 3.9
fig3.6 <- readr::read_rds("sim_study_mlm36.rds")
pal <- wesanderson::wes_palette("Zissou1", 3, type="continuous")

moe_plot(fig3.6, x_axis=nbar) +
  ggplot2::labs(
    y = "Average margin of error",
    x = "Average site size")

