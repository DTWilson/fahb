#setwd("C:/Users/meddwilb/OneDrive - University of Leeds/Documents/Research/Projects/fahb/R")
library(brms)
library(posterior)
library(extraDistr)

source("generate_data.R")
source("PC_analysis.R")
source("Bayes_analysis.R")

input_arg <- as.numeric(commandArgs(TRUE))

task_id <- input_arg[1]; N <- input_arg[2]
#task_id <- 1; N <- 10

#scenarios <- matrix(c(1,1,20,0.5,300,1.75,0.3,10,0.4,10,1,
#                      1,2,20,0.5,300,1.75,0.3,10,0.4,10,1,
#                      2,1,20,1,300,1.75,0.3,10,0.4,10,1,
#                      2,2,20,1,300,1.75,0.3,10,0.4,10,1), nrow=4, byrow = TRUE)
#saveRDS(scenarios, "scenarios.rds")

scenarios <- read.csv("scenarios.csv")
sce <- as.numeric(scenarios[task_id,])

scenario_id <- sce[1]; part <- sce[2]

m <- sce[3]; int_t <- sce[4]; target_n <- sce[5]
beta_m <- sce[6]; beta_s <- sce[7]
v_sh <- sce[8]; v_r <- sce[9]
setup_r_a <- sce[10]; setup_r_b <- sce[11]
int_t <- int_t*sce[12]

int_data <- generate_data(m=20, int_t=3, target_n=300, beta_m=1.75, beta_s=0.3, v_sh=5, v_r=100, setup_r_a=10, setup_r_b=1)[[1]]

stanvars <- stanvar(beta_m, name='beta_m') + stanvar(beta_s, name='beta_s') + stanvar(v_sh, name='v_sh') + stanvar(v_r, name='v_r')

bprior <- c(prior(normal(beta_m, beta_s), class = "Intercept"),
            prior(gamma(v_sh, v_r), class = "sd"))

bayes_model <- brm(y | rate(t) ~ 1 + (1 | c), data = int_data, family = poisson(),
                  prior = bprior, 
                  stanvars = stanvars,
                  chains = 0, silent = 2)

res <- NULL
for(i in 1:N){
  x <- generate_data(m, int_t, target_n, beta_m, beta_s, v_sh, v_r, setup_r_a, setup_r_b)
  
  int_data <- x[[1]]
  rec_time <- x[[2]]
  
  PC_stats <- PC_analysis(int_data)
  Bayes_stats <- Bayes_analysis(int_data, m, bayes_model, int_t, target_n, setup_r_a, setup_r_b)
  
  res <- rbind(res, c(rec_time, PC_stats, Bayes_stats))
}

write.csv(res, file = paste0("./results/res_", scenario_id, "_", part, ".csv"), row.names = FALSE)
