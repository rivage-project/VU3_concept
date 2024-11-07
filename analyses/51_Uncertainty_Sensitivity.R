rm(list=ls())
library("tidyverse")

# 1. Data ----
# load bird and mammal checklists per island
birds <- readRDS("data/derived-data/12_bird_ckl_islands_clean.RDS")

# load traits
tr_birds <- readRDS("data/derived-data/12_Bird_traits.RDS")

# 2. Trait normalization ----
sensitivityFUN <- function(trait_df,
                           trait_columns,
                           norm){
  
  if(!(norm %in% c("minmax", "logminmax", "rankminmax"))){
    stop("norm should be one of these three options: 'minmax',
         'logminmax' or 'rankminmax'.")
  }
  
  # Normalize each marker
  if(norm == "minmax"){
    # Min-max normalization
    trait_df_minmax <- apply(trait_df[, tracol], 2, function(x){
      (x - min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
    })
    colnames(trait_df_minmax) <- paste0(colnames(trait_df_minmax), "_minmax")
    
    trait_df_minmax <- as.data.frame(trait_df_minmax)
    
    # Calculating the Sensitivity Index
    trait_df_minmax$sensitivity_minmax <-
      apply(trait_df_minmax, 1, function(x) sum(x, na.rm = TRUE))
    
    # Normalizing the Sensitivity Index
    trait_df_minmax$sensitivity_minmax_norm <-
      (trait_df_minmax$sensitivity_minmax -
         min(trait_df_minmax$sensitivity_minmax, na.rm = TRUE))/
      (max(trait_df_minmax$sensitivity_minmax, na.rm = TRUE) -
         min(trait_df_minmax$sensitivity_minmax, na.rm = TRUE))
    
    # Binding normalization to the original trait dataset
    trait_df <- cbind(trait_df, trait_df_minmax)
    
  } else if(norm == "logminmax"){
    # Log minmax normalization
    trait_df_logminmax <- apply(trait_df[, tracol], 2, function(x){
      (log(x + 1) - min(log(x + 1), na.rm = TRUE))/(max(log(x + 1), na.rm = TRUE) - min(log(x + 1), na.rm = TRUE))
    })
    colnames(trait_df_logminmax) <- paste0(colnames(trait_df_logminmax), "_logminmax")
    
    trait_df_logminmax <- as.data.frame(trait_df_logminmax)
    
    # Calculating the Sensitivity Index
    trait_df_logminmax$sensitivity_logminmax <-
      apply(trait_df_logminmax, 1, function(x) sum(x, na.rm = TRUE))
    
    # Normalizing the Sensitivity Index
    trait_df_logminmax$sensitivity_logminmax_norm <-
      (trait_df_logminmax$sensitivity_logminmax -
         min(trait_df_logminmax$sensitivity_logminmax, na.rm = TRUE))/
      (max(trait_df_logminmax$sensitivity_logminmax, na.rm = TRUE) -
         min(trait_df_logminmax$sensitivity_logminmax, na.rm = TRUE))
    
    # Binding normalization to the original trait dataset
    trait_df <- cbind(trait_df, trait_df_logminmax)
    
  } else if(norm == "rankminmax"){
    # Rank normalization
    trait_df_rankminmax <- apply(trait_df[, tracol], 2, function(x){
      (dense_rank(x) - min(dense_rank(x), na.rm = TRUE))/(max(dense_rank(x), na.rm = TRUE) - min(dense_rank(x), na.rm = TRUE))
    })
    colnames(trait_df_rankminmax) <- paste0(colnames(trait_df_rankminmax), "_rankminmax")
    
    trait_df_rankminmax <- as.data.frame(trait_df_rankminmax)
    
    # Calculating the Sensitivity Index
    trait_df_rankminmax$sensitivity_rankminmax <-
      apply(trait_df_rankminmax, 1, function(x) sum(x, na.rm = TRUE))
    
    # Normalizing the Sensitivity Index
    trait_df_rankminmax$sensitivity_rankminmax_norm <-
      (trait_df_rankminmax$sensitivity_rankminmax -
         min(trait_df_rankminmax$sensitivity_rankminmax, na.rm = TRUE))/
      (max(trait_df_rankminmax$sensitivity_rankminmax, na.rm = TRUE) -
         min(trait_df_rankminmax$sensitivity_rankminmax, na.rm = TRUE))
    
    # Binding normalization to the original trait dataset
    trait_df <- cbind(trait_df, trait_df_rankminmax)
  }
  
  
  return(trait_df)
}

# 3. Running the function ----
# trait columns
tracol <- c("Range.Size", "nb_diet", "nb_hab", "GenLength")
# "Hand-Wing.Index", "Mass" => adaptive capacity traits

test <- sensitivityFUN(trait_df = tr_birds,
                        trait_columns = tracol,
                        norm = "minmax")
test2 <- sensitivityFUN(trait_df = tr_birds,
                       trait_columns = tracol,
                       norm = "logminmax")
test3 <- sensitivityFUN(trait_df = tr_birds,
                         trait_columns = tracol,
                         norm = "rankminmax")
