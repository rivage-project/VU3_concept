# Exposure to global threats
rm(list = ls())

library(tidyverse)

# load exposure to each threat
ias <- readRDS("data/derived-data/")
cc <- readRDS("data/derived-data/21_CC_SED_exposure_45_isl.rds")
lu <- readRDS("data/derived-data/20_LU_exposure_45_isl.rds")


# select variables and create a list with all threats
colnames(ias)
colnames(lu)
colnames(cc)

threats <- left_join(
  lu %>% select(ID, mean_HM_change, rdens_osm) %>%
    mutate(ID = as.integer(ID)),
  left_join(
    cc %>% select(ID, sed_tas_isl_med, sed_pr_isl_med, sed_tot_med), 
    ias %>% select(ULM_ID, nb_alien_plant, prop_alien_plant,
                   nb_alien_bird, prop_alien_bird, alien_bird_cover,
                   nb_alien_mam, prop_alien_mam, alien_mam_cover) %>%
      rename(ID=ULM_ID)
    )
) %>% column_to_rownames("ID")

summary(threats)

# HM change contains negative values
# add the minimal value to all values 
# so relationship between islands is conserved
threats$mean_HM_change <- threats$mean_HM_change + abs(min(threats$mean_HM_change))
summary(threats)

# Select
isl <- readRDS("data/derived-data/01_shp_45_major_isl.rds")
unique(isl$Archip)


threats_major <- threats %>%
  rownames_to_column("ID") %>%
  filter(!ID %in% isl_select$ID[isl_select$Island_name %in% minor_islands]) %>%
  column_to_rownames("ID")

# Normalize variables

# select major or all islands
th_max_min  <- th_log <- th_rank <- x <- threats_major 

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

summary(th_max_min)
summary(th_log)
summary(th_rank)

th_norm = list(
  th_max_min = th_max_min,
  th_log=th_log,
  th_rank = th_rank
)

# save for uncertainties & sensitivity script
saveRDS(th_norm, "data/derived-data/23_Exposure_components_norm.rds")

#### Sum components to get final exposure

th_agg <- lapply(th_norm, function(x){
  y <- x %>%
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


saveRDS(th_agg, "data/derived-data/23_Exposure_major_isl.rds")



################

th_agg <- readRDS("data/derived-data/23_Exposure_major_isl.rds")

expo <- th_agg[["th_max_min"]] # select log, rank, max_min



#take the same colors as first fig
archip_col <-  c(
  "Galapagos Islands" = "#4DAF4A",
  "Canary Islands" = "#377EB8",
  "Azores"="#FF7F00", 
  "Mascarene Islands" = "#984EA3",
  "Hawaii" = "#E41A1C")

# lu and cc
lc <- ggplot(expo)+
  geom_hline(yintercept = .5, lty = 2, color = "grey")+
  geom_vline(xintercept = .5, lty = 2, color = "grey")+
  geom_point(aes(x=lu, y = cc, size = Area, color = Archip), 
             position = 'jitter', alpha = .6)+
  xlab("Land-use change")+ylab("Climate change")+
  scale_color_manual(values = archip_col)+
  theme_classic()

# lu and ias
li <- ggplot(expo)+
  geom_hline(yintercept = .5, lty = 2, color = "grey")+
  geom_vline(xintercept = .5, lty = 2, color = "grey")+
  geom_point(aes(x=lu, y = ias, size = Area, color = Archip), 
             position = 'jitter', alpha = .5)+
  xlab("Land-use change")+ylab("Biological invasions")+
  scale_color_manual(values = archip_col)+
  theme_classic()

# cc and ias
ci <- ggplot(expo)+
  geom_hline(yintercept = .5, lty = 2, color = "grey")+
  geom_vline(xintercept = .5, lty = 2, color = "grey")+
  geom_point(aes(x=ias, y = cc, size = Area, color = Archip), 
             position = 'jitter', alpha = .5)+
  xlab("Biological invasions")+ylab("Climate change")+
  scale_color_manual(values = archip_col)+
  theme_classic()

ggpubr::ggarrange(lc, li, ci, nrow=1, ncol = 3, common.legend = T, legend = "bottom")

pdf("figures/23_Exposure_components.pdf", 6, 6)
ggpubr::ggarrange(lc, li, ci, nrow=2, ncol = 2, legend = "none")
dev.off()
pdf("figures/23_Exposure_components_legend.pdf", 6, 8)
ggpubr::ggarrange(lc, li, ci, nrow=3, ncol = 1,
                  common.legend=T, legend = "right")
dev.off()


cor.test(expo$ias, expo$lu)
cor.test(expo$cc, expo$ias)
cor.test(expo$cc, expo$lu)


