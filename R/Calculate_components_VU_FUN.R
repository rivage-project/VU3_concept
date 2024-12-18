# Functions for calculating VU


#' Normalize components
#' 
#' @description
#' This function prints a simple message. This is a demo function to show good
#' practices in writing and documenting R function. If you delete this function
#' do not forget to delete the corresponding test file 
#' `tests/testthat/test-demo.R` if you used `new_package(test = TRUE)`.
#' 
#' @param x a character of length 1. Default is `'Hello world'`.
#'
#' @return The value of `x` (only if the result is assigned to a variable).
#' 
#' @export
#'
#' @examples
#' print_msg()
#' print_msg("Bonjour le monde")
#' 
#' x <- print_msg("Bonjour le monde")
#' x

normaliseFUN <- function(norm, df) {
  # browser()
  # Original dataset saved
  df_or <- df
  
  if(any(colnames(df)=='sci_name')){
    df <- df %>% 
      select(-sci_name_IUCN)
  }
  
  if(any(colnames(df)=='ID')){
    df <- df %>% 
      select(-ULM_ID)
  }
  
  if(!(norm %in% c("minmax", "logminmax", "rankminmax"))){
    stop("norm should be one of these three options: 'minmax',
         'logminmax' or 'rankminmax'.")
  }
  
  if(norm == "minmax"){
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
  
  # colnames(df) <- paste0(colnames(df), '_', norm)
  
  # Binding normalization to the original dataset
  df <- as.data.frame(df)
  # df <- cbind(df_or, df)
  return(df)    
}


#' Calculate exposure
#' 
#' @description
#' This function prints a simple message. This is a demo function to show good
#' practices in writing and documenting R function. If you delete this function
#' do not forget to delete the corresponding test file 
#' `tests/testthat/test-demo.R` if you used `new_package(test = TRUE)`.
#' 
#' @param x a character of length 1. Default is `'Hello world'`.
#'
#' @return The value of `x` (only if the result is assigned to a variable).
#' 
#' @export
#'
#' @examples
#' print_msg()
#' print_msg("Bonjour le monde")
#' 
#' x <- print_msg("Bonjour le monde")
#' x

exposureFUN <-
  function(expo_df, 
           lu_cols = c("mean_HM_change","mean_HM_static_2017", "rdens_osm"),
           cc_cols = "sed_tot_med",
           ias_cols = c("nb_alien_bird", "prop_alien_bird", "alien_bird_cover"),
           inv_variables = NULL,
           norm = "minmax"){
    
    if(!(norm %in% c("minmax", "logminmax", "rankminmax"))){
      stop("norm should be one of these three options: 'minmax',
         'logminmax' or 'rankminmax'.")
    }
    
    expo_df_unq <- 
      expo_df[, c("ULM_ID", 
                  lu_cols,
                  cc_cols,
                  ias_cols)] %>% 
      unique()
    
    exposure_df_norm <- 
      normaliseFUN(norm = norm,
                   df = expo_df_unq[!names(expo_df_unq) %in% c("ULM_ID")])
    
    if(any(inv_variables %in% names(exposure_df_norm))){
      
      this_invs <- inv_variables[inv_variables %in% names(exposure_df_norm)]
      
      for(inv in this_invs){
        
        exposure_df_norm[,inv] <- 1 - exposure_df_norm[,inv]
        
      }
      
    }
    
    lu <- apply(exposure_df_norm[lu_cols], 1,
                function(x) sum(x, na.rm = TRUE))
    cc <- apply(exposure_df_norm[cc_cols], 1,
                function(x) sum(x, na.rm = TRUE))
    ias <- apply(exposure_df_norm[ias_cols], 1,
                 function(x) sum(x, na.rm = TRUE))
    
    exposure_df_norm$lu <- 
      normaliseFUN(norm = norm,
                   df = data.frame(lu))
    
    exposure_df_norm$cc <- 
      normaliseFUN(norm = norm,
                   df = data.frame(cc))
    
    exposure_df_norm$ias <- 
      normaliseFUN(norm = norm,
                   df = data.frame(ias))
    
    exposure <- exposure_df_norm$lu + exposure_df_norm$cc + exposure_df_norm$ias
    
    exposure_df_norm$exposure <- 
      normaliseFUN(norm = norm,
                   df = data.frame(exposure)) %>% 
      unlist()
    
    exposure_df_norm$ULM_ID <-
      expo_df_unq$ULM_ID
    
    return(exposure_df_norm)
    
  }



#' Calculate sensitivity
#' 
#' @description
#' This function prints a simple message. This is a demo function to show good
#' practices in writing and documenting R function. If you delete this function
#' do not forget to delete the corresponding test file 
#' `tests/testthat/test-demo.R` if you used `new_package(test = TRUE)`.
#' 
#' @param x a character of length 1. Default is `'Hello world'`.
#'
#' @return The value of `x` (only if the result is assigned to a variable).
#' 
#' @export
#'
#' @examples
#' print_msg()
#' print_msg("Bonjour le monde")
#' 
#' x <- print_msg("Bonjour le monde")
#' x

sensitivityFUN <- function(sens_df,
                           sens_cols = c("Range.Size", "nb_diet", "nb_hab", "GenLength"),
                           inv_variables = c("Range.Size", "nb_hab", "nb_diet"),
                           norm = "minmax"){
  
  if(!(norm %in% c("minmax", "logminmax", "rankminmax"))){
    stop("norm should be one of these three options: 'minmax',
         'logminmax' or 'rankminmax'.")
  }
  
  sens_df_unq <- 
    sens_df[, c("sci_name_IUCN", 
                sens_cols)] %>% 
    unique()
  
  sens_df_norm <- normaliseFUN(norm = norm,
                               df = sens_df_unq[!names(sens_df_unq) %in% c("sci_name_IUCN")])
  
  
  if(any(inv_variables %in% names(sens_df_norm))){
    
    this_invs <- inv_variables[inv_variables %in% names(sens_df_norm)]
    
    for(inv in this_invs){
      
      sens_df_norm[,inv] <- 1 - sens_df_norm[,inv]
      
    }
    
  }
  
  
  
  # calculate sensitivity
  sensitivity <- apply(sens_df_norm, 1,
                       function(x) sum(x, na.rm = TRUE))
  
  # normalise sensitivity with mix max only
  sens_df_norm$sensitivity <- 
    normaliseFUN(norm = 'minmax',
                 df = data.frame(sensitivity)) %>% 
    unlist()
  
  sens_df_norm$sci_name_IUCN <-
    sens_df_unq$sci_name_IUCN
  
  return(sens_df_norm)
}

adaptive_capacityFUN <- function(ac_df,
                                 ac_sp_cols = c('Hand-Wing.Index', 'Mass'),
                                 ac_isl_cols = c('mean_elev', 'mean_tri', 'PA_prop'),
                                 inv_variables = NULL,
                                 norm = "minmax"){
  
  if(!(norm %in% c("minmax", "logminmax", "rankminmax"))){
    stop("norm should be one of these three options: 'minmax',
         'logminmax' or 'rankminmax'.")
  }
  
  df_species <- unique(ac_df[, c('ULM_ID', 'sci_name_IUCN', ac_sp_cols)])
  df_islands <- unique(ac_df[, c('ULM_ID', ac_isl_cols)])
  
  # Normalize each trait
  df_species_norm <- normaliseFUN(norm = norm, df = df_species)
  df_islands_norm <- normaliseFUN(norm = norm, df = df_islands)
  
  if(any(inv_variables %in% names(df_species_norm))){
    
    this_invs <- inv_variables[inv_variables %in% names(df_species_norm)]
    
    for(inv in this_invs){
      
      df_species_norm[,inv] <- 1 - df_species_norm[,inv]
      
    }
    
  }
  
  if(any(inv_variables %in% names(df_islands_norm))){
    
    this_invs <- inv_variables[inv_variables %in% names(df_islands_norm)]
    
    for(inv in this_invs){
      
      df_islands_norm[,inv] <- 1 - df_islands_norm[,inv]
      
    }
    
  }
  
  df_species_norm$ULM_ID <- df_species$ULM_ID
  df_species_norm$sci_name_IUCN <- df_species$sci_name_IUCN
  df_islands_norm$ULM_ID <- df_islands$ULM_ID
  df_islands_norm$ARCHIP <- df_islands$ARCHIP
  
  # bind data together
  out_df <- dplyr::full_join(df_species_norm[, c('ULM_ID', 'sci_name_IUCN', ac_sp_cols)], 
                             df_islands_norm[, c('ULM_ID', ac_isl_cols)],
                             by='ULM_ID')
  
  # calculate adapative capacity
  adaptive_capacity <-
    apply(out_df[!names(out_df) %in% c("ULM_IF", "sci_name_IUCN", "ARCHIP")], 1, function(x) sum(x, na.rm = TRUE))
  
  # normalise adaptive capacity with mix max only
  out_df$adaptive_capacity <- 
    normaliseFUN(norm = 'minmax', 
                 df = data.frame(adaptive_capacity)) %>% 
    unlist()
  
  return(out_df)
}

vulnerabilityFUN <-
  function(df, 
           norm = "minmax",
           sens_cols = c("Range.Size", "nb_diet", "nb_hab", "GenLength"),
           ac_sp_cols = c('Hand-Wing.Index', 'Mass'),
           ac_isl_cols = c('mean_elev', 'mean_tri', 'PA_prop'),
           lu_cols = c("mean_HM_change","mean_HM_static_2017", "rdens_osm"),
           cc_cols = "sed_tot_med",
           ias_cols = c("nb_alien_bird", "prop_alien_bird", "alien_bird_cover"),
           summary_method = "sum",
           n_samples = 1,
           prop_samples = 1,
           by_island = FALSE,
           inv_variables = c("Range.Size", "nb_hab", "nb_diet")){
    
    bootstrap_df <-
      pbapply::pblapply(1:n_samples, function(i){
        
        # set.seed(i)
        bootstrap_samples <-
          sample(1:nrow(df), prop_samples*nrow(df))
        
        E_df <- 
          exposureFUN(expo_df = df[bootstrap_samples,], 
                      lu_cols = lu_cols, 
                      cc_cols = cc_cols, 
                      ias_cols = ias_cols, 
                      inv_variables = inv_variables,
                      norm)
        
        S_df <- 
          sensitivityFUN(sens_df = df[bootstrap_samples,], 
                         sens_cols = sens_cols, 
                         inv_variables = inv_variables,
                         norm = norm)
        
        AC_df <- 
          adaptive_capacityFUN(ac_df = df[bootstrap_samples,], 
                               ac_sp_cols = ac_sp_cols,
                               ac_isl_cols = ac_isl_cols,
                               inv_variables = inv_variables,
                               norm = norm)
        
        # 3 different methods for calculating VU: topsis, sum, product
        
        # define positive and negative ideal solution for TOPSIS method
        sol <- data.frame(
          Exposure = c(0, 1),
          Sensitivity = c(0, 1),
          AdaptCapacity = c(1, 0))
        rownames(sol) <- c("pos", "neg")
        
        isl_info <- df[c(1, 2, 15)] %>% unique
        
        compo <- 
          dplyr::left_join(AC_df[c("adaptive_capacity", "sci_name_IUCN", "ULM_ID")],
                           E_df[c("exposure","ULM_ID")], by = join_by(ULM_ID)) %>% 
          dplyr::left_join(S_df[c("sensitivity", "sci_name_IUCN")], by = join_by(sci_name_IUCN)) %>% 
          dplyr::left_join(isl_info, by = join_by(ULM_ID))
        
        if(by_island == TRUE){
          
          compo <-
            compo %>% 
            dplyr::group_by(ULM_ID) %>% 
            dplyr::summarise(Island_name = unique(Island_name),
                             ARCHIP = unique(ARCHIP),
                             exposure = median(exposure),
                             sensitivity = median(sensitivity),
                             adaptive_capacity = median(adaptive_capacity))
          
        }
        
        compo <- compo %>%
          mutate(E = exposure,
                 S = sensitivity,
                 AC = adaptive_capacity) %>%
          # Calculate VU = E+S-AC and rank isl 
          mutate(Vu_sum = E + S - AC) %>%
          mutate(Vu_rank_sum = dense_rank(Vu_sum)) %>%
          # Calculate TOPSIS VU and rank isl 
          mutate(
            pos_sol = ((E-sol[1,1])^2 + (S-sol[1,2])^2 + (AC-sol[1,3])^2)^0.5,
            neg_sol = ((E-sol[2,1])^2 + (S-sol[2,2])^2 + (AC-sol[2,3])^2)^0.5,
          ) %>%
          mutate(Vu_TOPSIS = pos_sol / (pos_sol + neg_sol)) %>%
          mutate(Vu_rank_TOPSIS = dense_rank(Vu_TOPSIS)) %>%
          # calculate VU = E*S/(1+AC) (Butt et al 2022)
          mutate(Vu_prod = E*S/(AC+1)) %>%
          mutate(Vu_rank_prod = dense_rank(Vu_prod))
        
        if(summary_method == "sum") out <- compo$Vu_sum 
        if(summary_method == "rank_sum") out <- compo$Vu_rank_sum 
        if(summary_method == "TOPSIS") out <- compo$Vu_TOPSIS
        if(summary_method == "rank_TOPSIS") out <- compo$Vu_rank_TOPSIS
        if(summary_method == "prod") out <- compo$Vu_prod
        if(summary_method == "rank_prod") out <- compo$Vu_rank_prod
        
        if(by_island == TRUE){
          
          results <-
            data.frame(Island_name = compo$Island_name,
                       ARCHIP = compo$ARCHIP, 
                       ULM_ID = compo$ULM_ID, 
                       VU = unlist(out))
          
          results_agg <-
            dplyr::left_join(results,
                             AC_df[,c("ULM_ID", "sci_name_IUCN", "adaptive_capacity")], 
                             by = "ULM_ID", "sci_name_IUCN") %>% 
            dplyr::left_join(S_df[,c("sci_name_IUCN", "sensitivity")], 
                             by = "sci_name_IUCN") %>% 
            dplyr::left_join(E_df[,c("ULM_ID", "exposure")], 
                             by = c("ULM_ID"))
          
          results_agg <-
            results_agg %>% 
            dplyr::group_by(ULM_ID) %>% 
            dplyr::summarise(Island_name = unique(Island_name),
                             ARCHIP = unique(ARCHIP),
                             VU = unique(VU),
                             exposure = median(exposure),
                             sensitivity = median(sensitivity),
                             adaptive_capacity = median(adaptive_capacity))
          
          names(results_agg)[c(5:7)] <- c("E", "S", "AC")
          
        } else {
          
          results <-
            data.frame(Island_name = compo$Island_name,
                       ARCHIP = compo$ARCHIP, 
                       ULM_ID = compo$ULM_ID, 
                       sci_name_IUCN = compo$sci_name_IUCN,
                       VU = unlist(out))
          
          results_agg <-
            dplyr::left_join(results,
                             E_df[,c("ULM_ID", "exposure")], 
                             by = "ULM_ID") %>% 
            dplyr::left_join(S_df[,c("sci_name_IUCN", "sensitivity")], 
                             by = "sci_name_IUCN") %>% 
            dplyr::left_join(AC_df[,c("ULM_ID", "sci_name_IUCN", "adaptive_capacity")], 
                             by = c("ULM_ID", "sci_name_IUCN"))
          
          names(results_agg)[c(6:8)] <- c("E", "S", "AC")
          
        }
        
        return(results_agg)
        
      })
    
    if(by_island == TRUE){
      
      out <-
        Reduce(function(dtf1,dtf2) left_join(dtf1,dtf2,by=c("Island_name",
                                                            "ARCHIP",
                                                            "ULM_ID")), 
               bootstrap_df)
      
      if(n_samples > 1){
        names(out)[-c(1:3)]<- 
          paste0(c("VU_","E_", "S_", "AC_"), 
                 rep(1:n_samples, each = 4))
      }
      
    } else {
      
      out <-
        Reduce(function(dtf1,dtf2) left_join(dtf1,dtf2,by=c("Island_name",
                                                            "ARCHIP",
                                                            "ULM_ID", 
                                                            "sci_name_IUCN")), 
               bootstrap_df)
      
      if(n_samples > 1){
        names(out)[-c(1:4)]<- 
          paste0(c("VU_","E_", "S_", "AC_"), 
                 rep(1:n_samples, each = 4))
      }
      
    }
    
    return(out)
    
  }

summary_bootstrap <-
  function(df){
    
    # get column names for the replicates of each component
    VU_cols <- names(df)[grep("^VU_", names(df))]
    E_cols <- names(df)[grep("^E_", names(df))]
    S_cols <- names(df)[grep("^S_", names(df))]
    AC_cols <- names(df)[grep("^AC_", names(df))]
    
    # calculate median and CIs for each set of components
    df_summary <-
      lapply(1:nrow(df), function(j){
        
        row <- df[j,]
        
        VU_values <- row[,names(row) %in% VU_cols] %>% unlist()
        E_values <- row[,names(row) %in% E_cols] %>% unlist()
        S_values <- row[,names(row) %in% S_cols] %>% unlist()
        AC_values <- row[,names(row) %in% AC_cols] %>% unlist()
        
        data.frame(
          ULM_ID = row$ULM_ID,    
          Island_name = row$Island_name,
          ARCHIP = row$ARCHIP,
          median_VU = median(VU_values, na.rm = TRUE),
          sd_VU = sd(VU_values, na.rm = TRUE),
          lci_VU = quantile(VU_values, 0.025, na.rm = TRUE),
          uci_VU = quantile(VU_values, 0.975, na.rm = TRUE),
          median_E = median(E_values, na.rm = TRUE),
          sd_E = sd(E_values, na.rm = TRUE),
          lci_E = quantile(E_values, 0.025, na.rm = TRUE),
          uci_E = quantile(E_values, 0.975, na.rm = TRUE),
          median_S = median(S_values, na.rm = TRUE),
          sd_S = sd(S_values, na.rm = TRUE),
          lci_S = quantile(S_values, 0.025, na.rm = TRUE),
          uci_S = quantile(S_values, 0.975, na.rm = TRUE),
          median_AC = median(AC_values, na.rm = TRUE),
          sd_AC = sd(AC_values, na.rm = TRUE),
          lci_AC = quantile(AC_values, 0.025, na.rm = TRUE),
          uci_AC = quantile(AC_values, 0.975, na.rm = TRUE))
        
      }) %>% 
      do.call(rbind, .)
    
    df_summary
    
  }


