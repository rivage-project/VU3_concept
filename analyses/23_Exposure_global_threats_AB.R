# Exposure to global threats
rm(list = ls())

library(tidyverse)

# load exposure to each threat
ias <- readRDS("data/derived-data/22_IAS_exposure_39_isl.rds")
cc <- readRDS("data/derived-data/21_CC_SED_exposure_55_isl.rds")
lu <- readRDS("data/derived-data/20_LU_exposure_55_isl.rds")


# select variables and create a list with all threats
colnames(ias)
colnames(lu)
colnames(cc)

threats <- left_join(
  lu %>% select(ID, mean_HM_static_2017, mean_HM_change, rdens_osm) %>%
    mutate(ID = as.integer(ID)),
  left_join(
    cc %>% select(ID, sed_tot_med), 
    ias %>% select(ULM_ID,
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

# Keep only major islands before normalization?
# could change for the ranks

isl_select <- read.csv("data/derived-data/01_selected_islands.csv")
isl_select$Island_name[order(isl_select$Area)]
table(isl_select$Archip)

minor_islands <- c(
  "Isla Graciosa","Isla de Alegranza","Lobos","Isla de Montana Clara", # Canary
  "Lisianski Island","Sand Island","Lehua","Laysan Island", "Ford Island", # Hawaii
  "Ile d' Ambre", # Mascarene
  "Isla Bartolome", "Isla Seymour", "Isla Darwin"# Galapagos
  )

threats_major <- threats %>%
  rownames_to_column("ID") %>%
  filter(!ID %in% isl_select$ID[isl_select$Island_name %in% minor_islands]) %>%
  column_to_rownames("ID")
threats_minor <- threats %>%
  rownames_to_column("ID") %>%
  filter(ID %in% isl_select$ID[isl_select$Island_name %in% minor_islands])

# Normalize variables

# select major or all islands
threats_major <- threats_major[complete.cases(threats_major$prop_alien_bird), ]
threats_major <- threats_major %>% select(-c(ARCHIP, ISLAND))
th_max_min  <- th_log <- th_rank <- x <- threats_major 


#------ initialization
# max min linear
maxcol <- apply(x, 2, max, na.rm=T)
mincol <- apply(x, 2, min, na.rm=T)
# log transformed
# maxlog <- apply(x, 2, function(x){max(log(x+1), na.rm = T)})
# minlog <- apply(x, 2, function(x){min(log(x+1), na.rm = T)})
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
  # th_log[,i] <- ((log(x[,i]+1)-minlog[i]))/
  #   (maxlog[i]-minlog[i])
  # # ranks
  th_rank[,i] <- (x_rank[,i]-minrank[i])/(maxrank[i]-minrank[i])
}

summary(th_max_min)
# summary(th_log)
summary(th_rank)

th_norm = list(
  th_max_min = th_max_min,
  # th_log=th_log,
  th_rank = th_rank
)

# save for uncertainties & sensitivity script
# saveRDS(th_norm, "data/derived-data/23_Exposure_components_norm.rds")
saveRDS(th_norm, "data/derived-data/23_Exposure_components_norm_AB.rds")

#### Sum components to get final exposure

th_agg <- lapply(th_norm, function(x){
  y <- x %>%
    mutate(lu = mean_HM_change+mean_HM_static_2017+rdens_osm,
           cc = sed_tot_med,
           ias_bird = nb_alien_bird + prop_alien_bird + alien_bird_cover,
           ias_mam = nb_alien_mam + prop_alien_mam + alien_mam_cover) %>%
    select(lu, cc, ias_bird, ias_mam)
  
  # normalize lu, ias, and cc to sum for final exposure
  maxcol <- apply(y, 2, max, na.rm=T)
  mincol <- apply(y, 2, min, na.rm=T)
  y_norm <- y
  for (i in 1:length(maxcol)){
    y_norm[,i] <- (y[,i]-mincol[i])/(maxcol[i]-mincol[i])
  }
  # get final ias = ias plant + ias mam + ias b?
  y_norm_ias <- y_norm %>% 
    mutate(ias = ias_bird+ias_mam)
  a=y_norm_ias
  maxa = max(a$ias, na.rm = T)
  mina = min(a$ias, na.rm = T)
  y_norm_ias$ias = (a$ias-mina)/(maxa-mina)
  
  z <- left_join(
    y_norm_ias %>% rownames_to_column("ID") %>% 
      mutate(ID = as.integer(ID)) %>%
      mutate(expo_b =  lu + cc + ias_bird,
             expo_m = lu + cc + ias_mam,
             expo = lu + cc + ias), 
    isl_select %>% select(ID, Island_name, Archip, Area, Dist, Elev, SLMP, Lat))
  return(z)
})


saveRDS(th_agg, "data/derived-data/23_Exposure_major_isl.rds")



################

th_agg <- readRDS("data/derived-data/23_Exposure_major_isl.rds")

expo <- th_agg[["th_log"]]
expo <- th_agg[["th_max_min"]] # select log, rank, max_min

# check if differences between mam, plants and birds
plot(expo$expo_b, expo$expo_m)
# plot(expo$expo_b, expo$expo_p)
# plot(expo$expo_m, expo$expo_p)
# # correlated but notable differences


hist(expo$expo_b)
hist(expo$expo_m)
hist(expo$expo_p)

# spatial variation of exposure

# birds
ggplot(expo)+
  geom_boxplot(aes(x=Archip, y = expo_b))+
  geom_point(aes(x=Archip, y = expo_b, size = Area, color = Archip), position = 'jitter')+
  theme_classic()

# mam
ggplot(expo)+
  geom_boxplot(aes(x=Archip, y = expo_m))+
  geom_point(aes(x=Archip, y = expo_m, size = Area, color = Archip), position = 'jitter')+
  theme_classic()

# plants
ggplot(expo)+
  geom_boxplot(aes(x=Archip, y = expo_p))+
  geom_point(aes(x=Archip, y = expo_p, size = Area, color = Archip), 
             position = 'jitter', alpha = .5)+
  theme_classic()


# test all group together
expo_lg <- expo %>%
  pivot_longer(cols = starts_with("expo"),
               names_to = "group",
               names_prefix = "expo_",
               values_to = "expo") 
ggplot(expo_lg)+
  geom_boxplot(aes(x=Archip, y = expo, fill = group) )+
  #geom_point(aes(x=Archip, y = expo, size = Area, color = group), position = 'jitter')+
  theme_classic()


# consider expo to all IAS
ggplot(expo)+
  geom_boxplot(aes(x=Archip, y = expo))+
  geom_point(aes(x=Archip, y = expo, size = Area, color = Archip), 
             position = 'jitter', alpha = .5)+
  theme_classic()


# how are the threats related?

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

## all in same plot
ggplot(expo)+
  geom_point(aes(x=ias, y = cc, size = lu, color = Archip), 
             position = 'jitter', alpha = .5)+
  theme_classic()

cor.test(expo$cc, expo$ias)
cor.test(expo$lu, expo$ias)
cor.test(expo$cc, expo$lu)

# ias and dist
ggplot(expo)+
  geom_point(aes(x=Dist, y = ias, size = Area, color = Archip), 
             position = 'jitter', alpha = .5)+
  geom_smooth(aes(x=Dist, y = ias), method = "lm")+
  theme_classic()

a=lm(ias~Dist, data=expo)
plot(a)
summary(a)

