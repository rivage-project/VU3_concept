
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

    trait_df_norm <- normaliseFUN(norm_method = norm_method,
                                df = trait_df[, trait_columns])
  
  # calculate sensitivity
  ColsSp <- grep(x = colnames(trait_df_norm),  pattern = norm_method)
  ColsSp <- colnames(trait_df_norm)[ColsSp]
  trait_df_norm$sensitivity <- apply(trait_df_norm[, ColsSp], 1,
                                     function(x) sum(x, na.rm = TRUE))
  trait_df_norm$sci_name_IUCN <- trait_df$sci_name_IUCN
  
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

sensitivity_results <- sensitivityFUN(trait_df = tr_birds,
                       trait_columns = tracol,
                       norm_method = "minmax")
# test2 <- sensitivityFUN(trait_df = tr_birds,
#                         trait_columns = tracol,
#                         norm_method = "logminmax")
# test3 <- sensitivityFUN(trait_df = tr_birds,
#                         trait_columns = tracol,
#                         norm_method = "rankminmax")
# Plots

## joining with other relevant datasets
Data_Archi <- read.csv('data/derived-data/01_selected_islands.csv')
Data_Archi <- Data_Archi %>% 
  select(ID, Archip, Island_name)
colnames(Data_Archi)[1] <- 'ULM_ID'

Data <- readRDS('data_birds_all.rds') %>% 
  select(ULM_ID, Island_name, sci_name_IUCN)

sensitivity_results <- left_join(sensitivity_results, Data)
sensitivity_results <- left_join(sensitivity_results, Data_Archi)

library("ggplot2")
sensitivity_Medians <- sensitivity_results %>%
  group_by(Island_name) %>%
  summarise(Median = median(sensitivity_minmax, na.rm = TRUE))

sensitivity_Means <- sensitivity_results %>% 
  group_by(Island_name) %>%
  summarise(Mean=mean(sensitivity_minmax, na.rm=TRUE))


## histograms
ggplot(sensitivity_results, aes(sensitivity_minmax)) +
  geom_histogram(aes(fill=Archip, col=Archip)) +
  facet_wrap(~Island_name) +  
  geom_segment(data=sensitivity_Medians, 
               aes(x = Median, xend=Median, y=0, yend=10), col='black') +
  geom_segment(data=sensitivity_Means, 
               aes(x = Mean, xend=Mean, y=0, yend=10), col='blue') +
  theme_bw()


## plotting medians and CI from bootstrapping
my_boot <- function(x, times=5000) {
  
  # Get column name from input object
  var = deparse(substitute(x))
  var = gsub("^\\.\\$","", var)
  
  # Bootstrap 95% CI
  Sample <- sample(x, replace=TRUE, size = times)
  cis = quantile(Sample, probs=c(0.025,0.975))
  
  # Return data frame of results
  data.frame(lower.ci=cis[1], upper.ci=cis[2])
}

sensitivity_CIs <-
  sensitivity_results %>% 
  group_by(Island_name) %>%
  do(my_boot(.$sensitivity_minmax))


sensitivity_CIs <- left_join(sensitivity_results, sensitivity_CIs)
sensitivity_CIs <- left_join(sensitivity_CIs, sensitivity_Medians)
sensitivityOrder <- sensitivity_CIs[order(sensitivity_CIs$Median),]
sensitivity_CIs$Island_name <- factor(sensitivity_CIs$Island_name, 
                                            levels=unique(sensitivityOrder$Island_name))


library(RColorBrewer)

ggplot(sensitivity_CIs, aes(x=Median,
                                  xmin=lower.ci,
                                  xmax=upper.ci,
                                  y=Island_name,
                                  fill=Archip, 
                                  col=Archip)) +
  geom_point() +
  geom_errorbar() +
  theme_bw() +
  ggtitle('Sensitivity') +
  xlab('median \u00B1 95% bootstrap quantiles') +
  scale_color_manual("Archipelago",
                     values = c("Azores" = brewer.pal(n = 5, name = "Set1")[5],
                                "Canary Islands" = brewer.pal(n = 5, name = "Set1")[2],
                                "Galapagos Islands" = brewer.pal(n = 5, name = "Set1")[3],
                                "Hawaii" = brewer.pal(n = 5, name = "Set1")[1],
                                "Mascarene Islands" = brewer.pal(n = 5, name = "Set1")[4])) +
  scale_fill_manual("Archipelago",
                    values = c("Azores" = brewer.pal(n = 5, name = "Set1")[5],
                               "Canary Islands" = brewer.pal(n = 5, name = "Set1")[2],
                               "Galapagos Islands" = brewer.pal(n = 5, name = "Set1")[3],
                               "Hawaii" = brewer.pal(n = 5, name = "Set1")[1],
                               "Mascarene Islands" = brewer.pal(n = 5, name = "Set1")[4])) +
  theme(legend.position = "bottom") +
  ylab('Island name')


