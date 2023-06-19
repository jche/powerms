
// simple normal mlm, centered parameterization (for informative data)

data{
    int<lower=0> J;             // number of sites
    vector[J] tau_j_hat;        // observed site-specific mean
    vector<lower=0>[J] se_j;    // observed site-specific sd
}

parameters{
    real tau;            // grand mean
    real<lower=0> sig_tau;   // grand sd

    vector[J] tau_j;
}

model{
    // some weakly informative priors
    tau ~ normal(0, 0.1);
    sig_tau ~ normal(0, 0.1);

    tau_j ~ normal(tau, sig_tau);
    tau_j_hat ~ normal(tau_j, se_j);
}

generated quantities{
    // posterior predictive draw of true site impact
    real y_site_pred = normal_rng(tau, sig_tau);
}
