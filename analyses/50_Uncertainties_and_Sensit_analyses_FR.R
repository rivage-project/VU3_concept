# Explore Uncertainties and sensitivity to methodological choices

rm(list = ls())



library(tidyverse)


####### Exposure #########

# load threat data (normalized)
th_norm <- readRDS("23_Exposure_components_norm.rds")
isl_select <- read.csv("01_selected_islands.csv")


minor_islands <- c(
  "Isla Graciosa","Isla de Alegranza","Lobos","Isla de Montana Clara", # Canary
  "Lisianski Island","Sand Island","Lehua","Laysan Island", "Ford Island", # Hawaii
  "Ile d' Ambre", # Mascarene
  "Isla Bartolome", "Isla Seymour", "Isla Darwin"# Galapagos
)


isl_select_2 <- dplyr::filter(isl_select,  !Island_name%in%minor_islands)

# Compute exposure
expo <- lapply(th_norm, function(x){
  y <- x %>%
    # select the components you want in each global threat
    mutate(lu = mean_HM_change+mean_HM_static_2017+rdens_osm,
           cc = sed_tot_med,
           ias_bird = nb_alien_bird + prop_alien_bird + alien_bird_cover,
           ias_mam = nb_alien_mam + prop_alien_mam + alien_mam_cover,
           ias_plant = nb_alien_plant + prop_alien_plant) %>%
    select(lu, cc, ias_bird, ias_mam, ias_plant)
  
  # normalize lu, ias, and cc to sum for final exposure
  maxcol <- apply(y, 2, max, na.rm=T)
  mincol <- apply(y, 2, min, na.rm=T)
  y_norm <- y
  for (i in 1:length(maxcol)){
    y_norm[,i] <- (y[,i]-mincol[i])/(maxcol[i]-mincol[i])
  }
  # get final ias = ias plant + ias mam + ias b?
  y_norm_ias <- y_norm %>% 
    mutate(ias = ias_bird+ias_mam+ias_plant)
  a=y_norm_ias
  maxa = max(a$ias, na.rm = T)
  mina = min(a$ias, na.rm = T)
  y_norm_ias$ias = (a$ias-mina)/(maxa-mina)
  
  z <- left_join(
    y_norm_ias %>% rownames_to_column("ID") %>% 
      mutate(ID = as.integer(ID)) %>%
      mutate(expo_b =  lu + cc + ias_bird,
             expo_m = lu + cc + ias_mam,
             expo_p = lu + cc + ias_plant,
             expo = lu + cc + ias), 
    isl_select %>% select(ID, Island_name, Archip, Area, Dist, Elev, SLMP, Lat))
  return(z)
})

####### Sensitivity ########

# load sensitivity components for birds
sb_norm <- readRDS("13_Sensitivity_bird_major_isl.rds")

# compute species sensitivity
sens <- lapply(sb_norm, function(x){
  x %>%
    select(-sens) %>%
    # select the traits you want in final sensitivity
    # what is the effect of trait removal?
    mutate(sens = restricted_range + diet_specialism + hab_specialism + GenLength)
})

#################################################################
#################################################################

# testing contribution to sens #

# the idea is to test which of the markers contributed the most to the final sensitivy variable.
# Then, we keep the sensitivy based on the four markers and correlate it with 
# sensitivy based on all combination of one, two and three markers and check how far markers or groups of markers contribute to the sens

var_sens <- sb_norm$sb_max_min[,c("restricted_range", "diet_specialism", "hab_specialism", "GenLength")]

combins <- c(combn(4, 2, simplify=F), combn(4, 3, simplify=F))

corr_sens_vars <- NA

names_vars_involved <- NA

for (i in 1:length(combins))
{
  select <- var_sens[,combins[[i]]]
  
  sens_vars <- apply(select, 1, sum)
  
  corr_sens_vars[i] <- cor(sens$sb_max_min$sens, sens_vars)
  
  names_vars_involved[i] <- paste(names(select), collapse=' ')
}

data.frame(cor = c(cor(sb_norm$sb_max_min$sens, var_sens), corr_sens_vars), comb=c(names(var_sens), names_vars_involved))

#################################################################
#################################################################


####### Adaptive capacity #######

# load adaptive capacity components for birds
ac_norm <- readRDS("32_AC_tot_BMP_major_isl.rds")



# compute species AC
ac <- lapply(ac_norm, function(x){
  x %>%
    select(-c(contains("AC"), mass, max_height)) %>%
    # select the components you want in final AC
    mutate(AC = mean_tri + mean_elev + PA_prop + mean_hwi)
})

####### Final Vulnerability #######

# 3 different methods for calculating VU: topsis, sum, product

# define positive and negative ideal solution for TOPSIS method
sol <- data.frame(
  Exposure = c(0, 1),
  Sensitivity = c(0, 1),
  AdaptCapacity = c(1, 0))
rownames(sol) <- c("pos", "neg")


# define a function to compute vulnerability
# input: the normalization method 
# output: a dataframe with island classic biogeo var + VU values + VU ranks

calc.vu <- function(norm.method){
  
  # normalization method must be max_min, rank, or log
  if(!norm.method %in% c("log", "rank", "max_min")) stop("Normalization method is rank, log, or max_min")
  norma <- norm.method
  
  # select data accordingly
  E <- expo[[paste0("th_", norma)]] %>%
    select(ID, expo:Lat)
  S <- sens[[paste0("sb_", norma)]] %>%
    select(ID, sens, SR_bird)
  AC <- ac[[paste0("ac_", norma)]] %>% 
    select(ID, AC)
  
  compo <- left_join(E, left_join(S, AC)) 
  
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
  
  return(compo)
  
}

# test calc.vu 
vu_log <- calc.vu(norm.method = "log")



# function to save the output of calc.vu
save.vu <- function(norm.method, taxon){
  compo <- calc.vu(norm.method, taxon)
  saveRDS(compo, paste0("outputs/50_Uncert_VU_birds_", norm.method, ".rds"))
}

# save VU for classic method
save.vu(norm.method = "log")
save.vu(norm.method = "max_min")
save.vu(norm.method = "rank")



####### Explore differences between methods #######












##########################################################

# Explore data gaps


# load raw data (not normalized)
# because seleciton of major islands for normalization

# define also major/minor islands
# => lower data gaps on major islands?
# sure for sensitivity...


# on all the islands from Weigelt et al 
# 55 polygons


# how many with exposure data?
# with bird data checklist?
# with mammal data checklist?

# trait gaps for vertebrates:
# none?
# include imputed traits for mammal generation length
# and for bird generation length


# trait gaps for plants
# spatial pattern of missing values


