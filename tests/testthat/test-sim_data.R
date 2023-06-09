
test_that("site_sizes and site_ps work", {

  # checking site_ps
  vec <- seq(0.3,0.7,by=0.1)
  dat <- sim_data(
    outcome = "continuous",
    intercept_dist = c("normal"),
    effect_dist = "normal",
    nbar = 1000,
    site_ps = vec
  )
  dat %>%
    dplyr::group_by(sid) %>%
    dplyr::summarize(p_j = dplyr::first(p_j)) %>%
    dplyr::summarize(res = dplyr::setequal(p_j, vec)) %>%
    dplyr::pull(res) %>%
    expect_true()


  # checking site_sizes
  size_vec <- c(17,32,50,43,22)
  dat <- sim_data(
    outcome = "continuous",
    intercept_dist = c("normal"),
    effect_dist = "normal",
    site_sizes = size_vec
  )
  expect_equal(nrow(dat), sum(size_vec))


  # Site summary code works?
  ss <- summarize_sites( dat )
  expect_true( all( sort( ss$n )  == sort( size_vec ) ) )


  # checking when inputs conflict
  expect_error(
    sim_data(
      outcome = "continuous",
      intercept_dist = c("normal"),
      effect_dist = "normal",
      site_sizes = 5:10,
      site_ps = seq(0.5,0.9, by=0.1)
    )
  )

})

test_that("effect_dist works as intended", {
  dat <- sim_data(
    outcome = "continuous",
    intercept_dist = "normal",
    effect_dist = "normal",
    J = 100,
    rho = 1
  )
  dat %>%
    dplyr::group_by(sid) %>%
    dplyr::summarize(alpha_j = dplyr::first(alpha_j),
                     tau_j = dplyr::first(tau_j)) %>%
    dplyr::summarize(cor = cor(alpha_j, tau_j)) %>%
    dplyr::pull(cor) %>%
    expect_equal(1)

  # check that gamma dist looks gamma-ish, i.e., skewed to the right
  skew <- function(x, na.rm = FALSE){
    if(na.rm) x <- x[!is.na(x)]
    n <- length(x)
    sum((x - mean(x))^3)/(n - 2)/var(x)^(3/2)
  }
  dat <- sim_data(
    outcome = "continuous",
    intercept_dist = "normal",
    effect_dist = "gamma",
    J = 1000,
    nbar = 50,
    a = 1.5,
    b = 50
  )
  dat %>%
    dplyr::group_by(sid) %>%
    dplyr::summarize(tau_j = dplyr::first(tau_j)) %>%
    dplyr::summarize(skew = skew(tau_j)) %>%
    dplyr::pull(skew) %>%
    expect_gt(1)
})

test_that("correlations hold", {
  dat <- sim_data(
    outcome = "continuous",
    intercept_dist = "normal",
    effect_dist = "normal",
    J = 100,
    nbar = 50,
    vary_site_sizes = T,
    pbar = 0.5,
    vary_site_ps = T,

    cor_tau_n = -1,
    cor_tau_p = 1
  )

  res <- dat %>%
    dplyr::group_by(sid) %>%
    dplyr::summarize(tau_j = dplyr::first(tau_j),
                     n_j = dplyr::first(n_j),
                     p_j = dplyr::first(p_j)) %>%
    dplyr::summarize(cor1 = cor(tau_j, n_j),
                     cor2 = cor(tau_j, p_j))

  expect_lt(res$cor1, -0.9)
  expect_gt(res$cor2, 0.9)
})

test_that("manually specified sizes and ps stick together", {
  dat <- sim_data(
    outcome = "continuous",
    intercept_dist = "normal",
    effect_dist = "normal",
    site_sizes = 5:10,
    site_ps = (5:10)/15)

  temp <- dat %>%
    dplyr::group_by(sid) %>%
    dplyr::summarise(n_j = dplyr::first(n_j),
                     p_j = dplyr::first(p_j))
  expect_equal(temp$n_j, 5:10)
  expect_equal(temp$p_j, (5:10)/15)
})

test_that("correlations hold when site_ps specified", {
  dat <- sim_data(
    outcome = "continuous",
    intercept_dist = "normal",
    effect_dist = "normal",
    J = 100,
    nbar = 50,
    vary_site_sizes = T,
    site_ps = (26:125)/150,

    cor_tau_n = -1,
    cor_tau_p = 1
  )

  res <- dat %>%
    dplyr::group_by(sid) %>%
    dplyr::summarize(tau_j = dplyr::first(tau_j),
                     n_j = dplyr::first(n_j),
                     p_j = dplyr::first(p_j)) %>%
    dplyr::summarize(cor1 = cor(tau_j, n_j),
                     cor2 = cor(tau_j, p_j))

  expect_lt(res$cor1, -0.9)
  expect_gt(res$cor2, 0.9)
})

test_that("correlations hold when site_sizes specified", {
  dat <- sim_data(
    outcome = "continuous",
    intercept_dist = "normal",
    effect_dist = "normal",
    J = 100,
    site_sizes = 51:150,
    pbar = 0.5,
    vary_site_ps = T,

    cor_tau_n = -1,
    cor_tau_p = 1
  )

  res <- dat %>%
    dplyr::group_by(sid) %>%
    dplyr::summarize(tau_j = dplyr::first(tau_j),
                     n_j = dplyr::first(n_j),
                     p_j = dplyr::first(p_j)) %>%
    dplyr::summarize(cor1 = cor(tau_j, n_j),
                     cor2 = cor(tau_j, p_j))

  expect_lt(res$cor1, -0.9)
  expect_gt(res$cor2, 0.9)
})


test_that("control sd equals 1", {
  dat <- sim_data()

  dat %>%
    dplyr::summarize(sd = sd(Y0)) %>%
    dplyr::pull(sd) %>%
    expect_equal(1)
})





