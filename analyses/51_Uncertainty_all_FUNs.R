min_maxFUN <- function(x){
  
  maxcol <- apply(x, 2, max, na.rm=T)
  mincol <- apply(x, 2, min, na.rm=T)
  
  for (i in 1:length(maxcol)){
    x[,i] <- (x[,i]-mincol[i])/(maxcol[i]-mincol[i])
  }
  
  x
  
}

logFUN <- function(x){
  
  maxlog <- apply(x, 2, function(x){max(log(x+1), na.rm = T)})
  minlog <- apply(x, 2, function(x){min(log(x+1), na.rm = T)})
  
  for (i in 1:length(maxlog)){
    x[,i] <- ((log(x[,i]+1)-minlog[i]))/(maxlog[i]-minlog[i])
  }
  
  x
  
}

rankFUN <- function(x){
  
  x_rank <- x %>%
    dplyr::mutate_all(dense_rank)
  maxrank <- apply(x_rank, 2, max, na.rm=T)
  minrank <- apply(x_rank, 2, min, na.rm=T)
  
  for (i in 1:length(maxrank)){
    x[,i] <- (x_rank[,i]-minrank[i])/(maxrank[i]-minrank[i])
  }
  
  x
  
}

exposureFUN <-
  function(exposure_df, 
           norm){
    
    if(norm == "minmax") normFUN <- min_maxFUN
    if(norm == "logminmax") normFUN <- logFUN
    if(norm == "rankminmax") normFUN <- rankFUN
    
    # exposure_df <- exposure_df1
    # exposure_df <- exposure_df2
    
    exposure_df <-
      exposure_df[,c("ULM_ID",
                     "mean_HM_change", 
                     "mean_HM_static_2017", 
                     "rdens_osm", 
                     "sed_tot_med", 
                     "nb_alien_bird", 
                     "prop_alien_bird", 
                     "alien_bird_cover")] %>% 
      unique
    
    # HM change contains negative values
    # add the minimal value to all values 
    # so relationship between islands is conserved
    exposure_df$mean_HM_change <- exposure_df$mean_HM_change + abs(min(exposure_df$mean_HM_change))
    
    row.names(exposure_df) <- NULL
    
    exposure_df <-
      exposure_df %>% 
      column_to_rownames("ULM_ID")
    
    # Normalize variables
    
    th <- normFUN(exposure_df)
    
    #### Sum components to get final exposure
    
    y <- th %>%
      mutate(lu = mean_HM_change+mean_HM_static_2017+rdens_osm,
             cc = sed_tot_med,
             ias = nb_alien_bird + prop_alien_bird + alien_bird_cover) %>%
      select(lu, cc, ias)
    
    y_norm <- min_maxFUN(y)
    
    z <-
      y_norm %>% rownames_to_column("ULM_ID") %>% 
      mutate(ULM_ID = as.integer(ULM_ID)) %>%
      mutate(expo = lu + 
               cc + 
               ias)
    
    return(z)
    
  }

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
    trait_df_minmax <- apply(trait_df[, trait_columns], 2, function(x){
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
    trait_df_logminmax <- apply(trait_df[, trait_columns], 2, function(x){
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

## function for adaptive capacity 
# library(dplyr)

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
  
  
  
  ## traits are HWI and body mass
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

vulnerabilityFUN <-
  function(df, 
           norm = "minmax",
           trait_columns = c("Range.Size", "nb_diet", "nb_hab", "GenLength"),
           traits = c('Hand-Wing.Index', 'Mass'),
           island_markers = c('mean_elev', 'mean_tri', 'PA_prop'),
           summary_method = "sum",
           n_samples = 1,
           prop_samples = 1){
    
    bootstrap_df <-
      lapply(1:n_samples, function(i){
        
        set.seed(i)
        bootstrap_samples <-
          sample(1:nrow(df), prop_samples*nrow(df))
        
        E <- exposureFUN(df[bootstrap_samples,], norm)
        S <- sensitivityFUN(df[bootstrap_samples,], trait_columns, norm)
        AC <- adaptive_capacityFUN(df[bootstrap_samples,], 
                                   traits = traits,
                                   island_markers = island_markers,
                                   norm_method = norm)
        
        compo <- left_join(E, left_join(S, AC)) 
        
        # 3 different methods for calculating VU: topsis, sum, product
        
        # define positive and negative ideal solution for TOPSIS method
        sol <- data.frame(
          Exposure = c(0, 1),
          Sensitivity = c(0, 1),
          AdaptCapacity = c(1, 0))
        rownames(sol) <- c("pos", "neg")
        
        compo <- left_join(E, left_join(S, AC)) 
        
        names(compo)[names(compo) == "sensitivity_minmax"] <- "sens"
        names(compo)[names(compo) == "adaptive_capacity"] <- "AC"
        
        compo <- compo %>%
          # normalize final components
          mutate(Exposure01 = (expo-min(compo$expo, na.rm = T))/
                   (max(compo$expo, na.rm = T)-min(compo$expo, na.rm = T)),
                 Sensitivity01 = (sens-min(compo$sens, na.rm = T))/
                   (max(compo$sens, na.rm = T)-min(compo$sens, na.rm = T)),
                 AdaptCapacity01 = (AC-min(compo$AC, na.rm = T))/
                   (max(compo$AC, na.rm = T)-min(compo$AC, na.rm = T))) %>%
          # Calculate VU = E+S-AC and rank isl 
          mutate(Vu_sum = Exposure01 + Sensitivity01 - AdaptCapacity01) %>%
          mutate(Vu_rank_sum = dense_rank(Vu_sum)) %>%
          # Calculate TOPSIS VU and rank isl 
          mutate(
            pos_sol = ((Exposure01-sol[1,1])^2 + (Sensitivity01-sol[1,2])^2 + (AdaptCapacity01-sol[1,3])^2)^0.5,
            neg_sol = ((Exposure01-sol[2,1])^2 + (Sensitivity01-sol[2,2])^2 + (AdaptCapacity01-sol[2,3])^2)^0.5,
          ) %>%
          mutate(Vu_TOPSIS = pos_sol / (pos_sol + neg_sol)) %>%
          mutate(Vu_rank_TOPSIS = dense_rank(Vu_TOPSIS)) %>%
          # calculate VU = E*S/(1+AC) (Butt et al 2022)
          mutate(Vu_prod = Exposure01*Sensitivity01/(AdaptCapacity01+1)) %>%
          mutate(Vu_rank_prod = dense_rank(Vu_prod))
        
        if(summary_method == "sum") out <- compo$Vu_sum 
        if(summary_method == "rank_sum") out <- compo$Vu_rank_sum 
        if(summary_method == "TOPSIS") out <- compo$Vu_TOPSIS
        if(summary_method == "rank_TOPSIS") out <- compo$Vu_rank_TOPSIS
        if(summary_method == "prod") out <- compo$Vu_prod
        if(summary_method == "rank_prod") out <- compo$Vu_rank_prod
        
        data.frame(Island_name = df[bootstrap_samples,]$Island_name, 
                   ULM_ID = df[bootstrap_samples,]$ULM_ID, 
                   sci_name_IUCN = df[bootstrap_samples,]$sci_name_IUCN,
                   ARCHIP = df[bootstrap_samples,]$ARCHIP,
                   vulnerability = out)
        
      })
    
    out <-
    Reduce(function(dtf1,dtf2) left_join(dtf1,dtf2,by=c("Island_name", 
                                                        "ULM_ID", 
                                                        "sci_name_IUCN", 
                                                        "ARCHIP")), 
           bootstrap_df)
    
    names(out)[-c(1:4)] <- paste0("vulnerability", 1:(ncol(out)-4))
    
    return(out)
    
  }
