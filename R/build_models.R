source("generate_data.R")
int_data <- generate_data(m=20, int_t=0.5, target_n=300, beta_m=1.75, beta_s=0.3, v_df=10, v_sc=0.4, setup_r_a=10, setup_r_b=1)[[1]]

bprior <- c(prior(normal(1.75, 0.3), class = "Intercept"),
            prior(student_t(10, 0, 0.4), class = "sd"))

fit_empty  <- brm(y | rate(t) ~ 1 + (1 | c), data = int_data, family = poisson(),
                  prior = bprior, 
                  chains = 0, silent = 2,
                  file = "bayes_model_1")