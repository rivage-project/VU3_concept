
rm(list=ls())
library("tidyverse")

# 1. Data ----
# load bird and mammal checklists per island
birds <- readRDS("data/derived-data/12_bird_ckl_islands_clean.RDS")

# load traits
tr_birds <- readRDS("data/derived-data/12_Bird_traits.RDS")

# 2. Trait normalization ----
normaliseFUN <- function(norm_method, df) {
  # browser()
  # Original dataset saved
  df_or <- df
  
  if(any(colnames(df)=='sci_name_IUCN')){
    df <- df %>% 
      select(-sci_name_IUCN)
  }
  
  if(any(colnames(df)=='ULM_ID')){
    df <- df %>% 
      select(-ULM_ID)
  }
  
  if(!(norm_method %in% c("minmax", "logminmax", "rankminmax"))){
    stop("norm_method should be one of these three options: 'minmax',
         'logminmax' or 'rankminmax'.")
  }
  
  if(norm_method == "minmax"){
    # Min-max normalization
    df <- apply(df, 2, function(x){
      (x - min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
    })
  }
  
  if(norm_method == "logminmax"){
    # Log minmax normalization
    df <- apply(df, 2, function(x){
      (log(x + 1) - min(log(x + 1), na.rm = TRUE))/(max(log(x + 1), na.rm = TRUE) - min(log(x + 1), na.rm = TRUE))
    })
  }
  
  if(norm_method == "rankminmax"){
    # Rank normalization
    df <- apply(df, 2, function(x){
      (dense_rank(x) - min(dense_rank(x), na.rm = TRUE))/(max(dense_rank(x), na.rm = TRUE) - min(dense_rank(x), na.rm = TRUE))
    })
  }
  
  colnames(df) <- paste0(colnames(df), '_', norm_method)
  
  # Binding normalization to the original dataset
  df <- as.data.frame(df)
  df <- cbind(df_or, df)
  return(df)    
}

sensitivityFUN <- function(trait_df,
                           trait_columns,
                           norm_method){
  
  if(!(norm_method %in% c("minmax", "logminmax", "rankminmax"))){
    stop("norm should be one of these three options: 'minmax',
         'logminmax' or 'rankminmax'.")
  }
  # browser()
  trait_df_norm <- normaliseFUN(norm_method = norm_method,
                                df = trait_df[, trait_columns])
  
  # calculate sensitivity
  ColsSp <- grep(x = colnames(trait_df_norm),  pattern = norm_method)
  ColsSp <- colnames(trait_df_norm)[ColsSp]
  trait_df_norm$sensitivity <- apply(trait_df_norm[, ColsSp], 1,
                                     function(x) sum(x, na.rm = TRUE))
  
  # normalise sensitivity with mix max only
  outDF <- normaliseFUN(norm_method = 'minmax',
                        df = trait_df_norm[, 'sensitivity', drop = FALSE])
  outDF <- cbind(trait_df_norm, outDF[, 'sensitivity_minmax', drop = FALSE])
  
  return(outDF)
}

# 3. Running the function ----
# trait columns
tracol <- c("Range.Size", "nb_diet", "nb_hab", "GenLength")
# "Hand-Wing.Index", "Mass" => adaptive capacity traits

test <- sensitivityFUN(trait_df = tr_birds,
                       trait_columns = tracol,
                       norm_method = "minmax")
test2 <- sensitivityFUN(trait_df = tr_birds,
                        trait_columns = tracol,
                        norm_method = "logminmax")
test3 <- sensitivityFUN(trait_df = tr_birds,
                        trait_columns = tracol,
                        norm_method = "rankminmax")
# Plots
library("ggplot2")
sensitivity_Medians <- test %>% 
  summarise(Median = median(sensitivity_minmax), na.rm = TRUE)

ggplot(test, aes(sensitivity_minmax)) +
  geom_histogram(color = "grey30", fill = "grey70") +
  geom_segment(data = sensitivity_Medians, 
               aes(x = Median, xend = Median, y = 0, yend = 22),
               col = "dodgerblue", linewidth = 1) +
  labs(x = "Normalized sensitivity", y = "Count") +
  theme_bw()
