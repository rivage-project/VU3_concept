rm(list=ls())
library("tidyverse")

# 1. Data ----
# load bird and mammal checklists per island
birds <- readRDS("data/derived-data/12_bird_ckl_islands_clean.RDS")

# load traits
tr_birds <- readRDS("data/derived-data/12_Bird_traits.RDS")

# 2. Trait normalization ----
# trait columns
tracol <- c("nb_hab", "Hand-Wing.Index", "Mass", "Range.Size", "GenLength",
            "nb_diet")

# Min-max normalization
tr_birds_minmax <- apply(tr_birds[, tracol], 2, function(x){
  (x - min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
})
colnames(tr_birds_minmax) <- paste0(colnames(tr_birds_minmax), "_minmax")

# Log minmax normalization
tr_birds_logminmax <- apply(tr_birds[, tracol], 2, function(x){
  (log(x + 1) - min(log(x + 1), na.rm = TRUE))/(max(log(x + 1), na.rm = TRUE) - min(log(x + 1), na.rm = TRUE))
})
colnames(tr_birds_logminmax) <- paste0(colnames(tr_birds_logminmax), "_logminmax")

# Rank normalization
tr_birds_rankminmax <- apply(tr_birds[, tracol], 2, function(x){
  (dense_rank(x) - min(dense_rank(x), na.rm = TRUE))/(max(dense_rank(x), na.rm = TRUE) - min(dense_rank(x), na.rm = TRUE))
})
colnames(tr_birds_rankminmax) <- paste0(colnames(tr_birds_rank), "_rankminmax")

# Binding all normalizations to the original trait dataset
tr_birds <- cbind(tr_birds, tr_birds_minmax)
tr_birds <- cbind(tr_birds, tr_birds_logminmax)
tr_birds <- cbind(tr_birds, tr_birds_rankminmax)
