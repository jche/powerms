test_that("function runs", {
  foo <- powerms_single(
    sim_data_method = sim_data,
    sim_data_args = list(),
    se_method = "pooled",
    est_method = run_t_test,
    # est_method = run_mlm,
    num_sims = 10
  )

  expect_equal(nrow(foo), 300)
  expect_type(foo %>%
                dplyr::summarize(avg_moe = mean(ci_r - ci_l)) %>%
                dplyr::pull(avg_moe),
              "double")
})

test_that("renaming important variables works", {

  temp_sim_data_method <- function() {
    fabricatr::fabricate(
      site = fabricatr::add_level(
        N = 5,
        num_students = rbinom(5, size=100, prob=0.2)
      ),
      student = fabricatr::add_level(
        N = num_students,
        tx = rbinom(N, size=1, prob=0.5),
        outcome = rnorm(N)
      )
    )
  }

  foo <- powerms_single(
    sim_data_method = temp_sim_data_method,
    sim_data_args = list(),
    se_method = "pooled",
    est_method = run_t_test,
    # est_method = run_mlm,
    num_sims = 10,

    tx_var = "tx",
    outcome_var = "outcome",
    site_id = "site"
  )

  expect_equal(nrow(foo), 5*10)
})
