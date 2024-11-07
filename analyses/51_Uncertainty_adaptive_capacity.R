## function for adaptive capacity 


normaliseFUN <- function(norm_method, df) {
  
  df_or <- df
  
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
  
  if(norm == "logminmax"){
    # Log minmax normalization
    df <- apply(df, 2, function(x){
      (log(x + 1) - min(log(x + 1), na.rm = TRUE))/(max(log(x + 1), na.rm = TRUE) - min(log(x + 1), na.rm = TRUE))
    })
  }
  
  if(norm == "rankminmax"){
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
  
  
  
  ## traits are HWI and body mass
  ## exposure markers are island elevation, topographic heterogeneity, % protected area
  
  #### steps
  ## normalise the markers
  ## add the markers to obtain AC
  ## nornalise the AC
  
  df_species <- unique(df[, c('sci_name_IUCN', traits)])
  df_islands <- unique(df[, c('ULM_ID', island_markers)])
  
  # Normalize each trait
  df_species_norm <- normaliseFUN(norm_method = norm_method, df = df_species)
  df_islands_norm <- normaliseFUN(norm_method = norm_method, df = df_islands)
  
  # bind data together
  ColsSp <- grep(x = colnames(df_species_norm),  pattern = norm_method)
  ColsIsl <- grep(x = colnames(df_islands_norm),  pattern = norm_method)

  df <- dplyr::full_join(df_species[, c('ULM_ID', 'sci_name_IUCN', ColsSp)], 
                         df_islands_norm[, c('ULM_ID', ColsIsl)],
                         by='ULM_ID')
  
  # calculate adapative capacity
  df$adaptive_capacity <-
    apply(df[, c(ColsSp, ColsIsl)], 1, function(x) sum(x, na.rm = TRUE))
  
  # normalise adaptive capacity with mix max only
  outDF <- normaliseFUN(norm_method = 'minmax', df = df[, 'adaptive_capacity', drop=FALSE])
  outDF <- cbind(df, outDF[, 'adaptive_capacity_norm', drop=FALSE])
  
  return(df)
}

## load data 
Data <- readRDS('data_birds_all.rds')
colnames(Data)
Adaptive_capacity <- adaptive_capacityFUN(df = Data, 
                                          traits = c('Hand-Wing.Index', 'Mass'), 
                                          island_markers = c('mean_elev', 'mean_tpi', ''), 
                                          norm_method = 'minmax')

