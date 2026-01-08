source("generate_data.R")
source("PC_analysis.R")
source("Bayes_analysis.R")

input_arg <- as.numeric(commandArgs(TRUE))

task_id <- input_arg[1]; N <- input_arg[2]

scenario_id <- sce[1]; part <- sce[2]

m <- sce[3]; int_t <- sce[4]; target_n <- sce[5]
beta_m <- sce[6]; beta_s <- sce[7]
v_df <- sce[8]; v_sc <- sce[9]
setup_r_a <- sce[10]; setup_r_b <- sce[11]

res <- NULL
for(i in 1:N){
  x <- generate_data(m, int_t, target_n, beta_m, beta_s, v_df, v_sc, setup_r_a, setup_r_b)
  
  int_data <- x[[1]]
  rec_time <- x[[2]]
  
  PC_stats <- PC_analysis(int_data)
  Bayes_stats <- Bayes_analysis(int_data, m, bayes_model, int_t, target_n)
  
  res <- rbind(res, c(rec_time, PC_stats, Bayes_stats))
}

saveRDS(res, file = paste0("./results/res_", scenario_id, "_", part, ".rds"))