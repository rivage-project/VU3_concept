# Total AC = Extrinsic AC + Intrinsic AC (for each taxon)

rm(list=ls())

library(tidyverse)

# load islands
isl_select <- read.csv("data/derived-data/01_selected_islands.csv")
minor_islands <- c(
  "Isla Graciosa","Isla de Alegranza","Lobos","Isla de Montana Clara", # Canary
  "Lisianski Island","Sand Island","Lehua","Laysan Island", "Ford Island", # Hawaii
  "Ile d' Ambre", # Mascarene
  "Isla Bartolome", "Isla Seymour", "Isla Darwin"# Galapagos
)


# load AC extrinsic
top <- readRDS("data/derived-data/30_extrinsic_AC_elevation.rds")
pa <- readRDS("data/derived-data/30_extrinsic_AC_prop_PA.rds")

colnames(top)
colnames(pa)

# plot(top$mean_elev, top$mean_tri)
# plot(top$sd_elev, top$sd_tri)
# plot(top$mean_elev, top$sd_elev)
# plot(top$mean_tri, top$sd_tri)


ext <- left_join(
  top %>% select(ULM_ID, mean_tri, mean_elev), 
  pa %>% sf::st_drop_geometry() %>% select(ULM_ID, PA_prop) %>%
    mutate(PA_prop = as.numeric(PA_prop)))

ext[is.na(ext)] <- 0

summary(ext)

# load intrinsic
intr <- readRDS("data/derived-data/31_AC_intrinsic_BMP.rds")

# bind intrinsic and extrinsic
ac <- full_join(ext, intr)

# select major or all islands
ac_major <- ac %>%
  filter(!ULM_ID %in% isl_select$ID[isl_select$Island_name %in% minor_islands])
summary(ac_major)

#### normalize the components

ac_max_min  <- ac_log <- ac_rank <- x <- ac_major %>% column_to_rownames("ULM_ID") 

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
  ac_max_min[,i] <- (x[,i]-mincol[i])/(maxcol[i]-mincol[i])
  # log-transformed
  ac_log[,i] <- ((log(x[,i]+1)-minlog[i]))/
    (maxlog[i]-minlog[i])
  # ranks
  ac_rank[,i] <- (x_rank[,i]-minrank[i])/(maxrank[i]-minrank[i])
}

summary(ac_max_min)
summary(ac_log)
summary(ac_rank)

ac_norm = list(
  ac_max_min = ac_max_min,
  ac_log = ac_log,
  ac_rank = ac_rank
)

colnames(ac_major)
ac_agg <- lapply(ac_norm, function(x){
  x %>% rownames_to_column("ID") %>%
    mutate(ID = as.numeric(ID)) %>%
    mutate(AC_plant = mean_tri+mean_elev+PA_prop+max_height,
           AC_bird = mean_tri+mean_elev+PA_prop+mean_hwi,
           AC_mam = mean_tri+mean_elev+PA_prop+mass)
})

# save AC total 
saveRDS(ac_agg, "data/derived-data/32_AC_tot_BMP_major_isl.rds")





