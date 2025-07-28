# Functions for calculating VU
# (c) Clara Marino, Gabriel Caetano, Adrienne Etard, Pierre Denelle

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
    df <- df |> 
      dplyr::select(-sci_name)
  }
  
  if(any(colnames(df)=='ID')){
    df <- df |> 
      dplyr::select(-ID)
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
      (dplyr::dense_rank(x) - min(dplyr::dense_rank(x), na.rm = TRUE))/
        (max(dplyr::dense_rank(x), na.rm = TRUE) - min(dplyr::dense_rank(x), na.rm = TRUE))
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
           lu_cols = c("med_HM_change", "rdens_osm"),
           cc_cols = "sed_tot_med",
           ias_cols = c("nb_alien", "alien_vert_cover"),
           isl_col_name = "ID",
           inv_variables = NULL,
           norm = "minmax",
           weight_e = c(1,1,1)){
    
    if(!(norm %in% c("minmax", "logminmax", "rankminmax"))){
      stop("norm should be one of these three options: 'minmax',
         'logminmax' or 'rankminmax'.")
    }
    
    expo_df_unq <- 
      expo_df[, c(isl_col_name, 
                  lu_cols,
                  cc_cols,
                  ias_cols)] |> 
      unique()
    
    exposure_df_norm <- 
      normaliseFUN(norm = norm,
                   df = expo_df_unq[!names(expo_df_unq) %in% isl_col_name])
    
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
                   df = data.frame(lu))$lu
    
    exposure_df_norm$cc <- 
      normaliseFUN(norm = norm,
                   df = data.frame(cc))$cc
    
    exposure_df_norm$ias <- 
      normaliseFUN(norm = norm,
                   df = data.frame(ias))$ias
    
    exposure <- weight_e[2]*exposure_df_norm$lu + weight_e[1]*exposure_df_norm$cc + weight_e[3]*exposure_df_norm$ias
    
    # normalize final exposure with minmax only
    exposure_df_norm$exposure <- 
      normaliseFUN(norm = "minmax",
                   df = data.frame(exposure)) |> 
      unlist()
    
    exposure_df_norm$ID <- expo_df_unq[,isl_col_name]
    
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
                           sens_cols = c("aoh_km2", "nb_diet", "nb_hab", "gen_length_y"),
                           inv_variables = c("aoh_km2", "nb_hab", "nb_diet"),
                           sp_col_name = "sci_name",
                           isl_col_name = "ID",
                           norm = "minmax"){
  
  if(!(norm %in% c("minmax", "logminmax", "rankminmax"))){
    stop("norm should be one of these three options: 'minmax',
         'logminmax' or 'rankminmax'.")
  }
  
  sens_df_unq <- 
    sens_df[, c(sp_col_name, 
                sens_cols)] |> 
    unique()
  
  sens_df_norm <- normaliseFUN(norm = norm,
                               df = sens_df_unq[!names(sens_df_unq) %in% sp_col_name])
  
  
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
                 df = data.frame(sensitivity)) |> 
    unlist()
  
  sens_df_norm$sci_name <- sens_df_unq[,sp_col_name]
  
  return(sens_df_norm)
}

#' Calculate adaptive capacity
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

adaptive_capacityFUN <- function(ac_df,
                                 ac_biotic_cols = c('dispersal', 'fred'),
                                 ac_abiotic_cols = c('Area', 'max_elev', 'med_tri', 'PA_prop'),
                                 ac_sp_cols = c('dispersal'),
                                 ac_isl_cols = c('Area', 'max_elev', 'med_tri', 'PA_prop', 'fred'),
                                 sp_col_name = "sci_name",
                                 isl_col_name = "ID",
                                 inv_variables = NULL,
                                 norm = "minmax",
                                 weight_ac = c(1,1)){
  
  if(!(norm %in% c("minmax", "logminmax", "rankminmax"))){
    stop("norm should be one of these three options: 'minmax',
         'logminmax' or 'rankminmax'.")
  }
  
  df_species <- unique(ac_df[, c(sp_col_name, isl_col_name, ac_sp_cols)])
  df_islands <- unique(ac_df[, c(isl_col_name, ac_isl_cols)])
  
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
  
  df_species_norm$ID <- df_species[,isl_col_name]
  df_species_norm$sci_name <- df_species[,sp_col_name]
  df_islands_norm$ID <- df_islands[,isl_col_name]
  
  # bind data together
  ac_df_norm <- dplyr::full_join(df_species_norm[, c('ID', 'sci_name', ac_sp_cols)], 
                             df_islands_norm[, c('ID', ac_isl_cols)],
                             by='ID')
  
  ac_abiotic <- apply(ac_df_norm[ac_abiotic_cols], 1,
              function(x) sum(x, na.rm = TRUE))
  ac_biotic <- apply(ac_df_norm[ac_biotic_cols], 1,
              function(x) sum(x, na.rm = TRUE))
  
  ac_df_norm$ac_abiotic <- 
    normaliseFUN(norm = norm,
                 df = data.frame(ac_abiotic))$ac_abiotic
  
  ac_df_norm$ac_biotic <- 
    normaliseFUN(norm = norm,
                 df = data.frame(ac_biotic))$ac_biotic

  adaptive_capacity <- weight_ac[2]*ac_df_norm$ac_biotic + weight_ac[1]*ac_df_norm$ac_abiotic
  
  # normalize final AC with minmax only
  ac_df_norm$adaptive_capacity <- 
    normaliseFUN(norm = "minmax",
                 df = data.frame(adaptive_capacity)) |> 
    unlist()
  
  return(ac_df_norm)
}


#' Calculate Vulnerability
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

vulnerabilityFUN <-
  function(df, 
           norm = "minmax",
           sp_col_name = "sci_name",
           isl_col_name = "ID",
           sens_cols = c("aoh_km2", "nb_diet", "nb_hab", "gen_length_y"),
           ac_biotic_cols = c('dispersal', 'fred'),
           ac_abiotic_cols = c('Area', 'max_elev', 'med_tri', 'PA_prop'),
           ac_sp_cols = c('dispersal'),
           ac_isl_cols = c('Area', 'max_elev', 'med_tri', 'PA_prop', 'fred'),
           lu_cols = c("med_HM_change", "rdens_osm"),
           cc_cols = "sed_tot_med",
           ias_cols = c("nb_alien", "alien_vert_cover"),
           summary_method = "sum",
           n_samples = 1,
           prop_samples = 1,
           by_island = FALSE,
           inv_variables = c("aoh_km2", "nb_hab", "nb_diet"),
           weight_out = c(1,1,1),
           weight_e = c(1,1,1),
           weight_ac = c(1,1)){
    
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
                      isl_col_name = isl_col_name,
                      inv_variables = inv_variables,
                      norm = norm,
                      weight_e = weight_e)
        
        S_df <- 
          sensitivityFUN(sens_df = df[bootstrap_samples,], 
                         sens_cols = sens_cols, 
                         isl_col_name = isl_col_name,
                         sp_col_name = sp_col_name,
                         inv_variables = inv_variables,
                         norm = norm)
        
        AC_df <- 
          adaptive_capacityFUN(ac_df = df[bootstrap_samples,], 
                               ac_biotic_cols = ac_biotic_cols,
                               ac_abiotic_cols = ac_abiotic_cols,
                               ac_sp_cols = ac_sp_cols,
                               ac_isl_cols = ac_isl_cols,
                               isl_col_name = isl_col_name,
                               sp_col_name = sp_col_name,
                               inv_variables = inv_variables,
                               norm = norm,
                               weight_ac = weight_ac)
        
        # 3 different methods for calculating VU: topsis, sum, product
        
        # define positive and negative ideal solution for TOPSIS method
        sol <- data.frame(
          Exposure = c(0, 1),
          Sensitivity = c(0, 1),
          AdaptCapacity = c(1, 0))
        rownames(sol) <- c("pos", "neg")
        
        # initiate database
        
        compo <- 
          dplyr::left_join(AC_df[c("adaptive_capacity", isl_col_name, sp_col_name)],
                           E_df[c("exposure", isl_col_name)], by = isl_col_name) |> 
          dplyr::left_join(S_df[c("sensitivity", sp_col_name)], by = sp_col_name)
        
        
        if(by_island == TRUE){
          
          compo <-
            compo |> 
            dplyr::group_by(eval(as.name(isl_col_name))) |> 
            dplyr::summarise(exposure = median(exposure),
                             sensitivity = median(sensitivity),
                             adaptive_capacity = median(adaptive_capacity))
          colnames(compo)[1] <- isl_col_name
        }
        
        
        compo <- compo |>
          dplyr::mutate(E = weight_out[1]*exposure,
                 S = weight_out[2]*sensitivity,
                 AC = weight_out[3]*adaptive_capacity) |>
          # Calculate VU = E+S-AC and rank isl 
          dplyr::mutate(Vu_sum = E + S - AC) |>
          dplyr::mutate(Vu_rank_sum = dplyr::dense_rank(dplyr::desc(Vu_sum))) |>
          # Calculate TOPSIS VU and rank isl 
          dplyr::mutate(
            pos_sol = ((E-sol[1,1])^2 + (S-sol[1,2])^2 + (AC-sol[1,3])^2)^0.5,
            neg_sol = ((E-sol[2,1])^2 + (S-sol[2,2])^2 + (AC-sol[2,3])^2)^0.5,
          ) |>
          dplyr::mutate(Vu_TOPSIS = pos_sol / (pos_sol + neg_sol)) |>
          dplyr::mutate(Vu_rank_TOPSIS = dplyr::dense_rank(Vu_TOPSIS)) |>
          # calculate VU = E*S/(1+AC) (Butt et al 2022)
          dplyr::mutate(Vu_prod = E*S/(AC+1)) |>
          dplyr::mutate(Vu_rank_prod = dplyr::dense_rank(dplyr::desc(Vu_prod)))
        
        if(summary_method == "sum"){ 
          out <- compo$Vu_sum
          out_rank <- compo$Vu_rank_sum}
        if(summary_method == "TOPSIS"){ 
          out <- compo$Vu_TOPSIS
          out_rank <- compo$Vu_rank_TOPSIS}
        if(summary_method == "prod") {
          out <- compo$Vu_prod
          out_rank <- compo$Vu_rank_prod}
        
        if(by_island == TRUE){
          
          results <-
            data.frame(ID = compo[, isl_col_name],
                       VU = unlist(out),
                       VU_rank = unlist(out_rank))
          colnames(results)[1]<- isl_col_name
          
          results_agg <-
            dplyr::left_join(results,
                             compo[, c(isl_col_name, "E", "S", "AC")],
                             by = isl_col_name)
          
        } else {
          
          results <-
            data.frame(ID = compo[, isl_col_name],
                       sp = compo[, sp_col_name],
                       VU = unlist(out),
                       VU_rank = unlist(out_rank))
          colnames(results)[1:2]<- c(isl_col_name, sp_col_name)
          
          results_agg <-
            dplyr::left_join(results,
                             E_df[,c(isl_col_name, "exposure")], 
                             by = isl_col_name) |> 
            dplyr::left_join(S_df[,c(sp_col_name, "sensitivity")], 
                             by = sp_col_name) |> 
            dplyr::left_join(AC_df[,c(isl_col_name, sp_col_name, "adaptive_capacity")], 
                             by = c(isl_col_name, sp_col_name)) |>
            dplyr::rename(E = exposure, S = sensitivity, AC = adaptive_capacity)
          
        }
        
        return(results_agg)
        
      })
    
    if(by_island == TRUE){
      
      out <-
        Reduce(function(dtf1,dtf2) dplyr::left_join(dtf1,dtf2,by=isl_col_name), 
               bootstrap_df)
      
      if(n_samples > 1){
        names(out)[-1]<- 
          paste0(c("VU_", "VUrank_" ,"E_", "S_", "AC_"), 
                 rep(1:n_samples, each = 5))
      }
      
    } else {
      
      out <-
        Reduce(function(dtf1,dtf2) dplyr::left_join(dtf1,dtf2,by=c(isl_col_name, sp_col_name)), 
               bootstrap_df)
      
      if(n_samples > 1){
        names(out)[-c(1,2)]<- 
          paste0(c("VU_","VUrank_","E_", "S_", "AC_"), 
                 rep(1:n_samples, each = 5))
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
    df_summary <- lapply(as.list(1:nrow(df)), function(j){
        
        row <- df[j,]
        
        VU_values <- row[,names(row) %in% VU_cols] |> unlist()
        E_values <- row[,names(row) %in% E_cols] |> unlist()
        S_values <- row[,names(row) %in% S_cols] |> unlist()
        AC_values <- row[,names(row) %in% AC_cols] |> unlist()
        
        data.frame(
          ID = row$ID,
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
        
      }) |> dplyr::bind_rows()
    
    df_summary
    
  }


