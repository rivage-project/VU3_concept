# Exposure to global threats
rm(list = ls())


# load exposure to each threat
ias <- readRDS("data/derived-data/22_IAS_exposure_45_isl.rds")
cc <- readRDS("data/derived-data/21_CC_SED_exposure_45_isl.rds")
lu <- readRDS("data/derived-data/20_LU_exposure_45_isl.rds")|>
  dplyr::mutate(ID = as.integer(ID))


# select variables and create a list with all threats
colnames(ias)
colnames(lu)
colnames(cc)

threats <- dplyr::left_join(
  lu |> dplyr::select(ID, mean_HM_change, rdens_osm),
  dplyr::left_join(
    cc |> dplyr::select(ID, sed_tas_isl_med, sed_pr_isl_med, sed_tot_med), 
    ias |> dplyr::select(ID, nb_alien, alien_vert_cover)
    )
) |> textshape::column_to_rownames("ID")

summary(threats)

# HM change contains negative values
# add the minimal value to all values 
# so relationship between islands is conserved
threats$mean_HM_change <- threats$mean_HM_change + abs(min(threats$mean_HM_change))
summary(threats)

# Get islands
isl <- readRDS("data/derived-data/01_shp_45_major_isl.rds")
unique(isl$Archip)
isl_tbl <- dplyr::left_join(
  isl |> sf::st_as_sf() |> sf::st_drop_geometry() |> 
    dplyr::select(ID, Island, Archip), 
  lu |> dplyr::select(ID, Area_km2))
  

# Normalize variables

th_max_min  <- th_log <- th_rank <- x <- threats 

#------ initialization
# max min linear
maxcol <- apply(x, 2, max, na.rm=T)
mincol <- apply(x, 2, min, na.rm=T)
# log transformed
maxlog <- apply(x, 2, function(x){max(log(x+1), na.rm = T)})
minlog <- apply(x, 2, function(x){min(log(x+1), na.rm = T)})
# ranks
x_rank <- x |>
  dplyr::mutate_all(dplyr::dense_rank)
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
saveRDS(th_norm, "data/derived-data/23_Exposure_components_norm_45_isl.rds")

#### Sum components to get final exposure

th_agg <- lapply(th_norm, function(x){
  y <- x |>
    dplyr::mutate(lu = mean_HM_change+rdens_osm,
           cc = sed_tot_med,
           ias = alien_vert_cover + nb_alien) |>
    dplyr::select(lu, cc, ias)
  
  # normalize lu, ias, and cc to sum for final exposure
  maxcol <- apply(y, 2, max, na.rm=T)
  mincol <- apply(y, 2, min, na.rm=T)
  y_norm <- y
  for (i in 1:length(maxcol)){
    y_norm[,i] <- (y[,i]-mincol[i])/(maxcol[i]-mincol[i])
  }
  
  z <- dplyr::left_join(
    y_norm |> tibble::rownames_to_column("ID") |> 
      dplyr::mutate(ID = as.integer(ID)) |>
      dplyr::mutate(expo = lu + cc + ias), 
    isl_tbl)
  return(z)
})


saveRDS(th_agg, "data/derived-data/23_Exposure_45_isl.rds")



################

th_agg <- readRDS("data/derived-data/23_Exposure_45_isl.rds")

expo <- th_agg[["th_max_min"]] # select log, rank, max_min



source("R/Calculate_components_VU_FUN.R")

data_all <- readRDS("outputs/30_all_VU_components_45_isl_266_BM_sp.rds")

# get exposure
E_birds <- exposureFUN(data_all)


################
# compare with functions from Gabriel => same result!
th_agg <- readRDS("data/derived-data/23_Exposure_45_isl.rds")
expo <- th_agg[["th_max_min"]] # select log, rank, max_min
expo$ID==E_birds$ID

colnames(expo) <- paste0(colnames(expo), "_prev")
comp <- dplyr::left_join(expo |> dplyr::rename(ID = ID_prev), E_birds)
comp$expo_new <- (comp$expo_prev - min(comp$expo_prev))/(max(comp$expo_prev)-min(comp$expo_prev))
hist(comp$expo_prev)
hist(comp$expo_new)
hist(comp$exposure)

plot(comp$expo_new, comp$exposure)
round(comp$expo_new,3)==round(comp$exposure,3)
################

RColorBrewer::brewer.pal(12, "Paired")

#take the same colors as first fig
archip_col <-  c(
  "Galapagos Islands" = "#4DAF4A",
  "Canary Islands" = "#377EB8",
  "Azores"="#FF7F00", 
  "Mascarene Islands" = "#984EA3",
  "Hawaii" = "#E41A1C",
  "Tristan da Cunha Islands" = "#B15928")

library(ggplot2)

# lu and cc
lc <- ggplot(expo)+
  geom_hline(yintercept = .5, lty = 2, color = "grey")+
  geom_vline(xintercept = .5, lty = 2, color = "grey")+
  geom_point(aes(x=lu, y = cc, size = Area_km2, color = Archip), 
             position = 'jitter', alpha = .6)+
  xlab("Land-use change")+ylab("Climate change")+
  scale_color_manual(values = archip_col)+
  theme_classic()

# lu and ias
li <- ggplot(expo)+
  geom_hline(yintercept = .5, lty = 2, color = "grey")+
  geom_vline(xintercept = .5, lty = 2, color = "grey")+
  geom_point(aes(x=lu, y = ias, size = Area_km2, color = Archip), 
             position = 'jitter', alpha = .5)+
  xlab("Land-use change")+ylab("Biological invasions")+
  scale_color_manual(values = archip_col)+
  theme_classic()

# cc and ias
ci <- ggplot(expo)+
  geom_hline(yintercept = .5, lty = 2, color = "grey")+
  geom_vline(xintercept = .5, lty = 2, color = "grey")+
  geom_point(aes(x=ias, y = cc, size = Area_km2, color = Archip), 
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


