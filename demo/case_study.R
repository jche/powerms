
# script to replicate case study plots


#### school-level analysis ####

# Run simulation for specific scenario with specific site sizes.
res_schools <- powerms(
  sim_data_method = sim_data,
  se_method = "pooled",
  est_method = powerms:::run_mlm,
  tx_var = "Z",
  outcome_var = "Y",
  site_id = "sid",
  num_sims = 100,
  parallel = TRUE,

  outcome = "binary",
  intercept_dist = "normal",
  effect_dist = "normal",

  site_sizes = list(c(551, 412, 343, 173, 464, 544, 499, 396, 197, 116)),
  pbar = 0.5,
  vary_site_ps = FALSE,

  alpha = 0.175,
  sig_alpha = 0.01,
  tau = 0.03,
  sig_tau = c(0.01, 0.02, 0.03, 0.04, 0.05),
  rho = c(0, 0.3, 0.6),

  cor_tau_n = 0,
  cor_tau_p = 0
)

# NOTE/TODO: The above produces many convergence warnings.  Use the
# quietly() method in purrr to capture and count?


readr::write_rds(res_schools, here::here( "demo/case_study_schools.rds" ) )

# replicate Figure 3.9
res_schools <- readr::read_rds(here::here( "demo/case_study_schools.rds" ) )
pal <- wesanderson::wes_palette("Zissou1", 5, type="continuous")

# LUKE: This didn't work, but gave a promise evaluation.  Not sure if
# I broke something.
moe_plot_indiv(res_schools, grouping=sig_tau) +
  ggplot2::scale_color_manual(values = pal) +
  ggplot2::labs(
    y = "Average margin of error",
    x = "Site size",
    color = latex2exp::TeX("$\\sigma_\\tau$"))





#### state-level analysis ####

res_states <- powerms(
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

  site_sizes = list(c(551, 928, 895, 1008, 309)),
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

readr::write_rds(res_states, "case_study_states.rds")

# replicate Figure 3.10
res_states <- readr::read_rds("case_study_states.rds")
pal <- wesanderson::wes_palette("Zissou1", 5, type="continuous")

moe_plot_indiv(res_states, grouping=sig_tau) +
  ggplot2::scale_color_manual(values = pal) +
  ggplot2::labs(
    y = "Average margin of error",
    x = "Site size",
    color = latex2exp::TeX("$\\sigma_\\tau$"))




# misc --------------------------------------------------------------------

if (FALSE) {
  summary_powerms_single()
  moe_plot()
}


# TODO:
#  - challenge: want to pass sim data arguments via ...,
#    but also want to pass arguments for est_method(),
#    e.g., settings for the priors, significance level, (centered/ncp), etc...
#  - how to do this with just a single ...?
# A: force est_method to be the full thing.

# TODO:
#  - debug stan a bit, see why we're getting divergences
#     --> not so much of an issue, probably add centered parameterization option
#  - add "default" dashed line for t-test...?
#     --> nah, just run separately for now.
#  - done...? package and ship to Luke for some testing!

