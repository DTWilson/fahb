sces <- 1:128
parts <- 1:10

scenarios <- read.csv("./R/scenarios.csv")

# Check we have the right number of data sets
counts <- rep(0, 128)
sce_sub <- NULL
for(i in sces){
  for(j in parts){
    counts[i] <- counts[i] + file.exists(paste0("./data/raw/res_", i, "_", j, ".csv"))
    if(!file.exists(paste0("./data/raw/res_", i, "_", j, ".csv"))){
      print(c(i,j))
      sce_sub <- rbind(sce_sub, scenarios[10*(i-1) + j,])
    }
  }
}

#write.csv(sce_sub, "./R/sce_sub.csv", row.names = F)

for(i in sces){
  sce_data <- NULL
  for(j in parts){
    if(file.exists(paste0("./data/raw/res_", i, "_", j, ".csv"))){
      df <- read.csv(paste0("./data/raw/res_", i, "_", j, ".csv"))
      sce_data <- rbind(sce_data, df)
    }
  }
  sce_data <- as.data.frame(sce_data)
  names(sce_data) <- c("rec_t", "m_p", "n_p", "r_p", "pred_t")
  saveRDS(sce_data, file = paste0("./data/res_", i, "_full.rds"))
}
