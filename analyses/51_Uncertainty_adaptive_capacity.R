## function for adaptive capacity 
library(dplyr)

normaliseFUN <- function(norm_method, df) {
  
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

adaptive_capacityFUN <- function(df,
                                 traits, 
                                 island_markers,
                                 norm_method){
  
  
  
  ## traits are HWI and body mass (body mass JUST FOR MAMMALS)
  ## exposure markers are island elevation, topographic heterogeneity, % protected area
  
  #### steps
  ## normalise the markers
  ## add the markers to obtain AC
  ## nornalise the AC
  
  df_species <- unique(df[, c('ULM_ID', 'sci_name_IUCN', traits)])
  df_islands <- unique(df[, c('ULM_ID', island_markers)])
  
  # Normalize each trait
  df_species_norm <- normaliseFUN(norm_method = norm_method, df = df_species)
  df_islands_norm <- normaliseFUN(norm_method = norm_method, df = df_islands)
  
  # bind data together
  ColsSp <- grep(x = colnames(df_species_norm),  pattern = norm_method)
  ColsSp <- colnames(df_species_norm)[ColsSp]
  ColsIsl <- grep(x = colnames(df_islands_norm),  pattern = norm_method)
  ColsIsl <- colnames(df_islands_norm)[ColsIsl]
  
  df <- dplyr::full_join(df_species_norm[, c('ULM_ID', 'sci_name_IUCN', ColsSp)], 
                         df_islands_norm[, c('ULM_ID', ColsIsl)],
                         by='ULM_ID')
  
  # calculate adapative capacity
  df$adaptive_capacity <-
    apply(df[, c(ColsSp, ColsIsl)], 1, function(x) sum(x, na.rm = TRUE))
  
  # normalise adaptive capacity with mix max only
  outDF <- normaliseFUN(norm_method = 'minmax', df = df[, 'adaptive_capacity', drop=FALSE])
  outDF <- cbind(df, outDF[, 'adaptive_capacity_minmax', drop=FALSE])
  
  return(outDF)
}

## load data 
Data <- readRDS('data_birds_all.rds')
colnames(Data)
Adaptive_capacity <- adaptive_capacityFUN(df = Data, 
                                          traits = c('Hand-Wing.Index'), 
                                          island_markers = c('mean_elev', 'mean_tri', 'PA_prop'), 
                                          norm_method = 'minmax') 

library(ggplot2)

Data_Archi <- read.csv('data/derived-data/01_selected_islands.csv')
Data_Archi <- Data_Archi %>% 
  select(ID, Archip, Island_name)
colnames(Data_Archi)[1] <- 'ULM_ID'

Adaptive_capacity <- left_join(Adaptive_capacity, Data_Archi)

Adaptive_capacity_Means <- Adaptive_capacity %>% 
  group_by(Island_name) %>%
  summarise(Mean=mean(adaptive_capacity_minmax, na.rm=TRUE))

Adaptive_capacity_Medians <- Adaptive_capacity %>% 
  group_by(Island_name) %>%
  summarise(Median=median(adaptive_capacity_minmax, , na.rm=TRUE))

## calculating CI from bootstrapping
library(boot)

ggplot(Adaptive_capacity, aes(adaptive_capacity_minmax)) +
  geom_histogram(aes(fill=Archip, col=Archip)) +
  facet_wrap(~Island_name) +  
  geom_segment(data=Adaptive_capacity_Means, 
               aes(x = Mean, xend=Mean, y=0, yend=10), col='black') +
  geom_segment(data=Adaptive_capacity_Medians, 
               aes(x = Median, xend=Median, y=0, yend=10), col='blue') +
  theme_bw()


## plotting medians and CI from bootstrapping
my_boot <- function(x, times=5000) {
  
  # Get column name from input object
  var = deparse(substitute(x))
  var = gsub("^\\.\\$","", var)
  
  # Bootstrap 95% CI
  Sample <- sample(x, replace=TRUE, size = times)
  cis = quantile(Sample, probs=c(0.025,0.975))
  browser()
  cis = sd(Sample)
  
  
  # Return data frame of results
  data.frame(var, lower.ci=cis[1], upper.ci=cis[2])
}

Adaptive_capacity_CIs <-
  Adaptive_capacity %>% 
  group_by(Island_name) %>%
  do(my_boot(.$adaptive_capacity_minmax))


Adaptive_capacity_CIs <- left_join(Adaptive_capacity, Adaptive_capacity_CIs)
Adaptive_capacityOrder <- Adaptive_capacity_CIs[order(Adaptive_capacity_CIs$Median),]
Adaptive_capacity_CIs$Island_name <- factor(Adaptive_capacity_CIs$Island_name, 
                                            levels=unique(Adaptive_capacityOrder$Island_name))


library(RColorBrewer)

ggplot(Adaptive_capacity_CIs, aes(x=Median,
                                  xmin=lower.ci,
                                  xmax=upper.ci,
                                  y=Island_name,
                                  fill=Archip, 
                                  col=Archip)) +
  geom_point() +
  geom_errorbar() +
  theme_bw() +
  ggtitle('Adaptive capacity') +
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



