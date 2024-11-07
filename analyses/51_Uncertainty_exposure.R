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
           norm_method){
    
    if(norm_method == "max_min") normFUN <- min_maxFUN
    if(norm_method == "log") normFUN <- logFUN
    if(norm_method == "rank") normFUN <- rankFUN
    
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

# # calculate expo for each normalization method
# expo_max_min <- exposureFUN(threats, norm_method = "max_min")
# expo_log <- exposureFUN(threats, norm_method = "log")
# expo_rank <- exposureFUN(threats, norm_method = "rank")
# 
# # histograms
# hist(expo_max_min$expo)
# hist(expo_log$expo)
# hist(expo_rank$expo)
# 
# # scatter plots
# 
# plot(expo_max_min$expo, expo_log$expo)
# plot(expo_max_min$expo, expo_rank$expo)
# plot(expo_rank$expo, expo_log$expo)
