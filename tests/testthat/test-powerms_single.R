test_that("function runs", {
  foo <- powerms_single(
    sim_data_method = sim_b_nn,
    sim_data_args = list(
      site_sizes = rep(50, 10),
      alpha = 0,
      sig_alpha = 0.2,
      tau = 1,
      sig_tau = 0.2,
      rho = 0),
    se_method = "pooled",
    est_method = run_t_test,
    # est_method = run_mlm,
    num_sims = 10
  )

  expect_equal(nrow(foo), 100)
  expect_type(foo %>%
                dplyr::summarize(avg_moe = mean(ci_r - ci_l)) %>%
                dplyr::pull(avg_moe),
              "double")
})
