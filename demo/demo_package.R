
# simulation study based on paper, but cut down for speed, with plots

library( powerms )
library( tidyverse )

set.seed(90210)
demo_sim <- powerms(
  sim_data_method = powerms:::sim_data,
  formula = Y ~ Z | sid,
  se_method = "pooled",
  est_method = powerms:::run_mlm,
  num_sims = 3,
  parallel = F,

  outcome = "continuous",
  intercept_dist = "normal",
  effect_dist = "normal",

  J = c(10, 25),
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
readr::write_rds(demo_sim, here::here( "demo/demo_results.rds") )


##### Look at results in various ways #####

demo_sim <- readr::read_rds( here::here( "demo/demo_results.rds" ) )

print( demo_sim )

class( demo_sim )

head( demo_sim )

summary( demo_sim )

a = summary( demo_sim )
ggplot( a, aes( nbar, mean_se, col=as.factor(J) ) ) +
  geom_line()


##### replicate Figure 3.9 (in the cut down form) #####

moe_plot(demo_sim, x_axis=nbar) +
  ggplot2::labs(
    y = "Average margin of error",
    x = "Average site size")

moe_plot(demo_sim, x_axis=nbar, grouping = J) +
  ggplot2::labs(
    y = "Average margin of error",
    x = "Average site size")



# demonstrate use of individually specified sites -------------------------

set.seed(1600)
demo_sim_indiv <- powerms(
  sim_data_method = powerms:::sim_data,
  formula = Y ~ Z | sid,
  se_method = "pooled",
  est_method = powerms:::run_mlm,
  num_sims = 5,
  parallel = F,

  outcome = "continuous",
  intercept_dist = "normal",
  effect_dist = "normal",

  # site_sizes = list(seq(10,90,by=10)),
  site_sizes = list(c(10,20,20,20,30,40,70,80,90)),
  site_ps = list(seq(0.1,0.9,by=0.1)),

  alpha = 0,
  tau = 0.2,
  sig_tau = c(0.1, 0.2),
  ICC = 0.2,

  rho = 0,
  cor_tau_n = 0,
  cor_tau_p = 0
)

readr::write_rds(demo_sim_indiv, here::here( "demo/demo_results_indiv.rds") )
demo_sim_indiv <- readr::read_rds( here::here( "demo/demo_results_indiv.rds" ) )

moe_plot_indiv(demo_sim_indiv)
moe_plot_indiv(demo_sim_indiv, grouping=sig_tau)


# data simulation ---------------------------------------------------------

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

summarize_sites( dat, formula = Y ~ Z | sid )

