rm(list=ls())
library("tidyverse")

# 1. Data ----
# load bird and mammal checklists per island
birds <- readRDS("data/derived-data/12_bird_ckl_islands_clean.RDS")

# load traits
tr_birds <- readRDS("data/derived-data/12_Bird_traits.RDS")

# 2. Trait normalization ----
sensitivity_FUN <- function(trait_df,
                            trait_columns,
                            norm){
  
  if(!(norm %in% c("minmax", "logminmax", "rankminmax"))){
    stop("norm should be one of these three options: 'minmax',
         'logminmax' or 'rankminmax'.")
  }
  
  if(norm == "minmax"){
    # Min-max normalization
    trait_df_minmax <- apply(trait_df[, tracol], 2, function(x){
      (x - min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
    })
    colnames(trait_df_minmax) <- paste0(colnames(trait_df_minmax), "_minmax")
    
    # Binding normalization to the original trait dataset
    trait_df <- cbind(trait_df, trait_df_minmax)

  } else if(norm == "logminmax"){
    # Log minmax normalization
    trait_df_logminmax <- apply(trait_df[, tracol], 2, function(x){
      (log(x + 1) - min(log(x + 1), na.rm = TRUE))/(max(log(x + 1), na.rm = TRUE) - min(log(x + 1), na.rm = TRUE))
    })
    colnames(trait_df_logminmax) <- paste0(colnames(trait_df_logminmax), "_logminmax")
    
    # Binding normalization to the original trait dataset
    trait_df <- cbind(trait_df, trait_df_logminmax)
    
  } else if(norm == "rankminmax"){
    # Rank normalization
    trait_df_rankminmax <- apply(trait_df[, tracol], 2, function(x){
      (dense_rank(x) - min(dense_rank(x), na.rm = TRUE))/(max(dense_rank(x), na.rm = TRUE) - min(dense_rank(x), na.rm = TRUE))
    })
    colnames(trait_df_rankminmax) <- paste0(colnames(trait_df_rank), "_rankminmax")
    
    # Binding normalization to the original trait dataset
    trait_df <- cbind(trait_df, trait_df_rankminmax)
  }
  
  return(trait_df)
}

# 3. Running the function ----
# trait columns
tracol <- c("nb_hab", "Hand-Wing.Index", "Mass", "Range.Size", "GenLength",
            "nb_diet")

# test <- sensitivity_FUN(trait_df = tr_birds,
#                         trait_columns = tracol,
#                         norm = "minmax")

