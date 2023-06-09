test_that("powerms function runs", {
  foo <- powerms(
    sim_data_method = sim_b_nn,
    se_method = "pooled",
    est_method = run_t_test,
    num_sims = 2,
    site_sizes = list(rep(25, 10), rep(50, 10)),
    alpha = 0,
    sig_alpha = c(0.2, 0.3),
    tau = 1,
    sig_tau = 0.2,
    rho = 0
  )

  expect_equal(length(unique(foo$rep_id)), 2)
  expect_equal(length(unique(foo$sim_id)), 4)
  expect_type(foo %>%
                dplyr::summarize(avg_moe = mean(ci_r - ci_l)) %>%
                dplyr::pull(avg_moe),
              "double")
})
