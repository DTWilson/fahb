scenarios <- 1:2
parts <- 1:2

for(i in scenarios){
  sce_data <- NULL
  for(j in parts){
    df <- read.csv(paste0("./data/res_", i, "_", j, ".csv"))
    sce_data <- rbind(sce_data, df)
  }
  sce_data <- as.data.frame(sce_data)
  names(sce_data) <- c("rec_t", "m_p", "n_p", "r_p", "pred_t")
  saveRDS(sce_data, file = paste0("./data/res_", i, "_full.rds"))
}
