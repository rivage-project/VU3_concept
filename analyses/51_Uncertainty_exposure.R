# create function

exposureFUN <-
  function(exposure_df, norm_method){
    
    # HM change contains negative values
    # add the minimal value to all values 
    # so relationship between islands is conserved
    threats$mean_HM_change <- threats$mean_HM_change + abs(min(threats$mean_HM_change))
    
    # Normalize variables
    
    # select major or all islands
    th_max_min  <- th_log <- th_rank <- x <- threats 
    
    #------ initialization
    # max min linear
    maxcol <- apply(x, 2, max, na.rm=T)
    mincol <- apply(x, 2, min, na.rm=T)
    # log transformed
    maxlog <- apply(x, 2, function(x){max(log(x+1), na.rm = T)})
    minlog <- apply(x, 2, function(x){min(log(x+1), na.rm = T)})
    # ranks
    x_rank <- x %>%
      dplyr::mutate_all(dense_rank)
    maxrank <- apply(x_rank, 2, max, na.rm=T)
    minrank <- apply(x_rank, 2, min, na.rm=T)
    
    #------ normalization
    for (i in 1:length(maxcol)){
      # max min
      th_max_min[,i] <- (x[,i]-mincol[i])/(maxcol[i]-mincol[i])
      # log-transformed
      th_log[,i] <- ((log(x[,i]+1)-minlog[i]))/
        (maxlog[i]-minlog[i])
      # ranks
      th_rank[,i] <- (x_rank[,i]-minrank[i])/(maxrank[i]-minrank[i])
    }
    
    th_norm = list(
      th_max_min = th_max_min,
      th_log=th_log,
      th_rank = th_rank
    )
    
    #### Sum components to get final exposure
    
    th_agg <- lapply(th_norm, function(x){
      y <- x %>%
        mutate(lu = mean_HM_change+mean_HM_static_2017+rdens_osm,
               cc = sed_tot_med,
               ias = nb_alien_bird + prop_alien_bird + alien_bird_cover) %>%
        select(lu, cc, ias)
      
      # normalize lu, ias, and cc to sum for final exposure
      maxcol <- apply(y, 2, max, na.rm=T)
      mincol <- apply(y, 2, min, na.rm=T)
      y_norm <- y
      for (i in 1:length(maxcol)){
        y_norm[,i] <- (y[,i]-mincol[i])/(maxcol[i]-mincol[i])
      }
      # get final ias = ias plant + ias mam + ias b?
      y_norm_ias <- y_norm %>% 
        mutate(ias = ias)
      a=y_norm_ias
      maxa = max(a$ias, na.rm = T)
      mina = min(a$ias, na.rm = T)
      y_norm_ias$ias = (a$ias-mina)/(maxa-mina)
      
      z <-
        y_norm_ias %>% rownames_to_column("ID") %>% 
          mutate(ID = as.integer(ID)) %>%
          mutate(expo = lu + cc + ias)
      
      return(z)
    })
    
    if(norm_method == "max_min") return(th_agg$th_max_min)
    if(norm_method == "log") return(th_agg$th_log)
    if(norm_method == "rank") return(th_agg$th_rank)
    
  }

# load exposure to each threat

ias <- readRDS("data/derived-data/22_IAS_exposure_39_isl.rds")
cc <- readRDS("data/derived-data/21_CC_SED_exposure_55_isl.rds")
lu <- readRDS("data/derived-data/20_LU_exposure_55_isl.rds")

# join all threats
threats <- left_join(
  lu %>% select(ID, mean_HM_static_2017, mean_HM_change, rdens_osm) %>%
    mutate(ID = as.integer(ID)),
  left_join(
    cc %>% select(ID, sed_tot_med), 
    ias %>% select(ULM_ID,
                   nb_alien_bird, 
                   prop_alien_bird, 
                   alien_bird_cover) %>%
      rename(ID=ULM_ID)
  )
)

# calculate expo for each normalization method
expo_max_min <- exposureFUN(threats, norm_method = "max_min")
expo_log <- exposureFUN(threats, norm_method = "log")
expo_rank <- exposureFUN(threats, norm_method = "rank")

# histograms
hist(expo_max_min$expo)
hist(expo_log$expo)
hist(expo_rank$expo)

# scatter plots

plot(expo_max_min$expo, expo_log$expo)
plot(expo_max_min$expo, expo_rank$expo)
plot(expo_rank$expo, expo_log$expo)
