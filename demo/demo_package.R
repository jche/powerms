
# simulation study based on paper, but cut down for speed, with plots

library( powerms )
library( tidyverse )



fig3.6 <- powerms(
  sim_data_method = sim_data,
  se_method = "pooled",
  est_method = powerms:::run_mlm,
  tx_var = "Z",
  outcome_var = "Y",
  site_id = "sid",
  num_sims = 3,
  parallel = F,

  outcome = "continuous",
  intercept_dist = "normal",
  effect_dist = "normal",

  J = c(10, 25, 100),
  nbar = seq(5, 200, by=50),
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

class( fig3.6 )
head( fig3.6 )

readr::write_rds(fig3.6, here::here( "demo/demo_results.rds") )

# replicate Figure 3.9 (in the cut down form)
fig3.6 <- readr::read_rds( here::here( "demo/demo_results.rds" ) )


# TODO: Is this getting used?  Need to be passed?
pal <- wesanderson::wes_palette("Zissou1", 3, type="continuous")


# TODO: What happens when grouping is not set?  Should this average
# over everything?  I changed the code to do this, but fix if this is
# wrong! -Luke
moe_plot(fig3.6, x_axis=nbar) +
  ggplot2::labs(
    y = "Average margin of error",
    x = "Average site size")


moe_plot(fig3.6, x_axis=nbar, grouping = J) +
  ggplot2::labs(
    y = "Average margin of error",
    x = "Average site size")




#### Data simulation ####


# This shows the DGP code and how we can get some multisite simulated
# data.

vec <- seq(0.3,0.7,by=0.1)
dat <- sim_data(
  outcome = "continuous",
  intercept_dist = c("normal"),
  effect_dist = "normal",
  nbar = 1000,
  vary_site_sizes = TRUE,
  site_ps = vec
)
head( dat )
length( unique( dat$sid ) )

summarize_sites( dat, tx_var = Z, outcome_var = Y, site_id = sid )

