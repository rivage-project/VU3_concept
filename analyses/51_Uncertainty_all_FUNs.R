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
  
  outDF$sci_name_IUCN <-
    trait_df$sci_name_IUCN
  
  return(outDF)
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

# norm = "minmax"
# trait_columns = c("Range.Size", "nb_diet", "nb_hab", "GenLength")
# traits = c('Hand-Wing.Index', 'Mass')
# island_markers = c('mean_elev', 'mean_tri', 'PA_prop')
# summary_method = "sum"
# n_samples = 1
# prop_samples = 1

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
        AC_df <- adaptive_capacityFUN(df[bootstrap_samples,], 
                                      traits = traits,
                                      island_markers = island_markers,
                                      norm_method = norm)
        
        E$expo <- (E$expo-min(E$expo, na.rm = T))/
          (max(E$expo, na.rm = T)-min(E$expo, na.rm = T))
        
        E <- E[!is.na(E$lu),]
        E <- E[!is.na(E$cc),]
        E <- E[!is.na(E$ias),]
        
        S <- S[c("sensitivity_minmax", "sci_name_IUCN")] %>% unique()
        
        # 3 different methods for calculating VU: topsis, sum, product
        
        # define positive and negative ideal solution for TOPSIS method
        sol <- data.frame(
          Exposure = c(0, 1),
          Sensitivity = c(0, 1),
          AdaptCapacity = c(1, 0))
        rownames(sol) <- c("pos", "neg")
        
        isl_info <- df[c(1, 2, 28)] %>% unique
        
        compo <- left_join(E, left_join(S, AC_df)) %>% 
          left_join(isl_info)
       
        compo <- compo %>%
          mutate(Exposure01 = expo,
                 Sensitivity01 = sensitivity_minmax,
                 AdaptCapacity01 = adaptive_capacity_minmax) %>%
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
        
        results <-
          data.frame(Island_name = compo$Island_name,
                     ARCHIP = compo$ARCHIP, 
                     ULM_ID = compo$ULM_ID, 
                     sci_name_IUCN = compo$sci_name_IUCN,
                     VU = out)
        
        results_agg <-
          dplyr::left_join(results,
                           E, 
                           by = "ULM_ID") %>% 
          dplyr::left_join(unique(S[c("sci_name_IUCN", "sensitivity_minmax")]), 
                           by = "sci_name_IUCN") %>% 
          dplyr::left_join(AC_df[c("ULM_ID", "sci_name_IUCN", "adaptive_capacity_minmax")], 
                           by = c("ULM_ID", "sci_name_IUCN"))
        
        names(results_agg)[c(9:10)] <- c("E", "S", "AC")
        
        return(results_agg)
        
      })
    
    out <-
      Reduce(function(dtf1,dtf2) left_join(dtf1,dtf2,by=c("ULM_ID", 
                                                          "sci_name_IUCN")), 
             bootstrap_df)
    
    names(out)[-c(1:4)] <- paste0(c("VU_", "lu_", "cc_", "ias_", "E_", "S_", "AC_"), rep(1:n_samples, each = 7))
    
    return(out)
    
  }
