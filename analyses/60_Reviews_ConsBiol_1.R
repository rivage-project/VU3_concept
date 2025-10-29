# review round #1

# 1. SInAS workflow: combine multiple sources
# as we want bird and mammals, lets overlap the 45 islands with DAMA and GAVIA

# load island information and polygons
isl <- readRDS("data/derived-data/01_shp_45_major_isl_clean.rds")

# path to alien data
path <- "Z:/THESE/5_Data/Distribution_spatiale/Alien_species/"


#### For birds -----

# open GAVIA and intersect with isl

b_shps <- list.files(paste0(path, "GAVIA_rangemaps/"), ".shp$")

# 362 shapefiles à ouvrir et combiner
gavia_isl_inter <- vector("list", length(b_shps))

for (i in 1:length(b_shps)){
  
  #open range map for each species
  sp <- sf::st_read(paste0(path, "GAVIA_rangemaps/", b_shps[i]))
  
  # ensure geometries are valid
  sp  <- sf::st_make_valid(sp)
  
  # same CRS projection
  sp <- sf::st_transform(sp, crs = sf::st_crs(isl))
  
  gavia_isl_inter[[i]] <- sf::st_intersects(isl, sp)
}

i=1
View(gavia_isl_inter[[200]])

# 2 sp with geom issues
pbm <- c(245,325)
# turn off spherical geometry
sf::sf_use_s2(FALSE)
for (i in pbm){
  sp <- sf::st_read(paste0(path, "GAVIA_rangemaps/", b_shps[i]))
  sp  <- sf::st_make_valid(sp)
  # same CRS projection
  sp <- sf::st_transform(sp, crs = sf::st_crs(isl))
  gavia_isl_inter[[i]] <- sf::st_intersects(isl, sp)
}
sf::sf_use_s2(TRUE)
#



inter_clean <- gavia_isl_inter
for(i in 1:length(gavia_isl_inter)){
  if(nrow(as.data.frame(gavia_isl_inter[[i]]))>0){
    inter_clean[[i]] <- data.frame(
      sp = b_shps[i],
      row_isl = as.data.frame(gavia_isl_inter[[i]])$row.id
    )
  }
  
}

inter_clean_df <- do.call(rbind, inter_clean) |>
  dplyr::left_join(sf::st_drop_geometry(isl) |> dplyr::mutate(row_isl = 1:45), by = "row_isl")

saveRDS(inter_clean_df, "data/derived-data/alien_occ/60_inter_isl_GAVIA.rds")

inter_clean_df <- readRDS("data/derived-data/alien_occ/60_inter_isl_GAVIA.rds")
sr_ias_b <- inter_clean_df |>
  dplyr::select(-row_isl) |>
  dplyr::group_by(ID)|>
  dplyr::summarise(sr_alien_b = dplyr::n())



##### same for mammals ------


m_shps <- list.files(paste0(path, "DAMA/DataS1/"), ".shp$")

# 230 shapefiles à ouvrir et combiner
dama_isl_inter <- vector("list", length(m_shps))

for (i in 1:length(m_shps)){
  
  #open range map for each species
  sp <- sf::st_read(paste0(path, "DAMA/DataS1/", m_shps[i]))
  
  # ensure geometries are valid
  sp  <- sf::st_make_valid(sp)
  
  # same CRS projection
  sp <- sf::st_transform(sp, crs = sf::st_crs(isl))
  
  dama_isl_inter[[i]] <- sf::st_intersects(isl, sp)
}

inter_clean_m <- dama_isl_inter
for(i in 1:length(dama_isl_inter)){
  if(nrow(as.data.frame(dama_isl_inter[[i]]))>0){
    inter_clean_m[[i]] <- data.frame(
      sp = m_shps[i],
      row_isl = as.data.frame(dama_isl_inter[[i]])$row.id
    )
  }
  
}
inter_clean_df_m <- do.call(rbind, inter_clean_m) |>
  dplyr::left_join(sf::st_drop_geometry(isl) |> dplyr::mutate(row_isl = 1:45), by = "row_isl")

saveRDS(inter_clean_df_m, "data/derived-data/alien_occ/60_inter_isl_DAMA.rds")


inter_clean_df_m <- readRDS("data/derived-data/alien_occ/60_inter_isl_DAMA.rds")
sr_ias_m <- inter_clean_df_m |>
  dplyr::select(-row_isl) |>
  dplyr::group_by(ID)|>
  dplyr::summarise(sr_alien_m = dplyr::n())


##### Combine birds and mammals and compare with GRIIS ----

sr_all <- dplyr::left_join(sr_ias_b, sr_ias_m, by ="ID")
sr_all[is.na(sr_all)] <- 0
sr_all$sr_alien <- sr_all$sr_alien_b + sr_all$sr_alien_m

# compare with exposure from GRIIS & GBIF

expo_ias <- readRDS("data/derived-data/22_IAS_exposure_45_isl.rds")

compa <- expo_ias |>
  dplyr::left_join(sr_all, by = "ID") |>
  dplyr::left_join(sf::st_drop_geometry(isl)|> dplyr::select(ID, Archip))
compa[is.na(compa)] <- 0

library(ggplot2)
colnames(compa)
archip_col1 <-  c(
  "Galapagos" = "#66A61E",
  "Canaries" = "#7570B3",
  "Azores"="#1B9E77", 
  "Mascarenes" = "#D95F02",
  "Hawai‘i" = "#E7298A",
  "Tristan da Cunha" = "#A6761D")

ggplot(compa)+
  geom_abline(slope = 1, intercept = 0, lty = 2, col = "grey20") +
  geom_smooth(aes(x=nb_alien, y = sr_alien), method = "lm", col = "gold", fill = "gold3", alpha = .2)+
  geom_point(aes(x=nb_alien, y = sr_alien, color = Archip, pch =  Archip))+
  xlab("NNS richness from GRIIS and GBIF") + ylab("NNS richness from DAMA and GAVIA")+
  scale_color_manual(values = archip_col1)+
  labs(color = "Archipelago", pch = "Archipelago")+
  theme_bw()


cor.test(compa$nb_alien, compa$sr_alien)
cor.test(compa$nb_alien, compa$sr_alien, method = "spearman")



# 2. bootstrap for VU ranking

###### load data and functions --------

source("R/Calculate_components_VU_FUN.R")

# load data in final format
data_all <- readRDS("outputs/30_all_VU_components_45_isl_266_BM_sp.rds")
# load island information and polygons
isl <- readRDS("data/derived-data/01_shp_45_major_isl_clean.rds")



###### Species-island combination bootstrapping --------
# per species and islands

VU_boot <-
  vulnerabilityFUN(data_all,
                   n_samples = 100,
                   prop_samples = 0.9)

VU_boot_summary <-
  summary_bootstrap_sp_isl(VU_boot)

head(VU_boot_summary)


VU_boot_sp_to_plot <- dplyr::left_join(
  VU_isl_info |> dplyr::select(ID, Archip, Island),
  VU_boot_summary)|>
  dplyr::mutate(Archip_clean = dplyr::case_when(
    Archip == "Azores" ~ "Azores", 
    Archip == "Canary Islands" ~ "Canaries",
    Archip == "Galapagos Islands" ~ "Galapagos",
    Archip == "Hawaii" ~ "Hawai‘i",
    Archip == "Mascarene Islands" ~ "Mascarenes",
    Archip == "Tristan da Cunha Islands" ~ "Tristan da Cunha" 
  ))

# order islands
isl_order <- VU_boot_sp_to_plot$Island[match(sort(VU_boot_sp_to_plot$median_VU), VU_boot_sp_to_plot$median_VU)]
VU_boot_sp_to_plot$Island_order <- factor(VU_boot_sp_to_plot$Island, levels = isl_order)

fig_boot_sp <- ggplot() +
  geom_point(data = VU_boot_sp_to_plot, 
             aes(x = median_VU, y = Island_order, col=Archip_clean, pch = Archip_clean))+
  geom_errorbar(data = as.data.frame(VU_boot_sp_to_plot), 
                aes(x=median_VU, xmin=lci_VU, xmax=uci_VU, y=Island_order, col=Archip_clean)) +
  theme_bw() + labs(col = "Archipelago", pch = "Archipelago") +
  xlab("median \u00B1 95% CI") +
  scale_color_manual("Archipelago", values = archip_col1) +
  theme(legend.position = "right") +
  ylab('Island name')

fig_boot_sp

ggsave(here::here("figures","Suppl_fig_bootstrap_sp_isl.tiff"), fig_boot_sp,
       width = 5, height = 6, dpi = 300, compression = "lzw")

###### Traits and markers bootstrapping --------


all_markers <- c(
  #exposure
  "med_HM_change", "rdens_osm", "sed_tot_med","nb_alien", "alien_vert_cover",
  #sensitivity
  "aoh_km2", "nb_diet", "nb_hab", "gen_length_y",
  #adaptive capacity
  'dispersal', 'fred','Area', 'max_elev', 'med_tri', 'PA_prop')

colnames(data_all)
fixed_cols <- c("ID", "Archip", "Island", "sci_name","Class")


boot1<- list()
for(i in 1:length(all_markers)){
  boot1[[i]] <- data_all[,c(fixed_cols, all_markers[-i])]
}

head(boot1[[5]])

# Calculate VU per species per island for each resample
boottest <- boot1
boottest[[10]] <- NULL

VU_sp_isl <- lapply(boottest, vulnerabilityFUN)


# for each new table, clean the data

VU_sp_isl_info_list <- lapply(VU_sp_isl, function(x){
  # join VU data with island information
  VU_sp_isl_info <- dplyr::left_join(
    isl |> sf::st_drop_geometry() |> 
      dplyr::select(ID, Archip, Island) |>
      dplyr::mutate(Archip_clean = dplyr::case_when(
        Archip == "Azores" ~ "Azores", 
        Archip == "Canary Islands" ~ "Canaries",
        Archip == "Galapagos Islands" ~ "Galapagos",
        Archip == "Hawaii" ~ "Hawai‘i",
        Archip == "Mascarene Islands" ~ "Mascarenes",
        Archip == "Tristan da Cunha Islands" ~ "Tristan da Cunha" 
      )),
    x
  )
  
  # create a ranking to make sure islands of each archipelago are together in the graph
  arch_isl <- paste0(isl$Archip, isl$Island)
  archip_order <- isl$Island[match(sort(arch_isl), arch_isl)] |> unique() 
  VU_sp_isl_info$Island <- factor(VU_sp_isl_info$Island, levels = archip_order)
  
  return(VU_sp_isl_info)
})


VU_median_mean_list <- lapply(VU_sp_isl_info_list, function(x){
  # calculate mean, median and sd value per island
  VU_median_mean <- 
    x |> 
    dplyr::group_by(Archip, Island, Archip_clean) |> 
    dplyr::summarise(
      Median = median(VU),
      Mean = mean(VU),
      SD = sd(VU)) |> dplyr::ungroup() |>
    dplyr::mutate(rank_vu = rank(Mean))
  isl_order <- VU_median_mean$Island[match(sort(VU_median_mean$Mean), VU_median_mean$Mean)]
  VU_median_mean$Island_order <- factor(VU_median_mean$Island, levels = isl_order)
  
  return(VU_median_mean)
  
})


all_vu_isl <- dplyr::bind_rows(VU_median_mean_list) |>
  dplyr::filter(!is.na(Mean))



# obtain island order from the observed values

# Calculate VU per species per island

VU_sp_isl <- vulnerabilityFUN(data_all)

# join VU data with island information
VU_sp_isl_info <- dplyr::left_join(
  isl |> sf::st_drop_geometry() |> 
    dplyr::select(ID, Archip, Island) |>
    dplyr::mutate(Archip_clean = dplyr::case_when(
      Archip == "Azores" ~ "Azores", 
      Archip == "Canary Islands" ~ "Canaries",
      Archip == "Galapagos Islands" ~ "Galapagos",
      Archip == "Hawaii" ~ "Hawai‘i",
      Archip == "Mascarene Islands" ~ "Mascarenes",
      Archip == "Tristan da Cunha Islands" ~ "Tristan da Cunha" 
    )),
  VU_sp_isl
)

# calculate mean, median and sd value per island
VU_median_mean <- 
  VU_sp_isl_info |> 
  dplyr::group_by(Archip, Island, Archip_clean) |> 
  dplyr::summarise(
    Median = median(VU),
    Mean = mean(VU),
    SD = sd(VU)) |> dplyr::ungroup()
isl_order <- VU_median_mean$Island[match(sort(VU_median_mean$Mean), VU_median_mean$Mean)]

# attribute good island order:
all_vu_isl$Island_order <- factor(all_vu_isl$Island, levels = isl_order)

boot_plot <- ggplot(all_vu_isl)+
  geom_point(aes(x = Island_order, y = rank_vu, color = Archip_clean), alpha = .3, size = 1)+ #, col = "grey50"
  geom_boxplot(aes(x = Island_order, y = rank_vu, color = Archip_clean), 
               width = .5, fill = NA, outliers = F)+
  scale_color_manual(values = archip_col1)+
  labs(color = "Archipelago")+
  theme_bw()+ xlab("Islands") + ylab("Final vulnerability ranking")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        legend.position ="top")

ggsave(here::here("figures","Suppl_fig_bootstrap_marker.tiff"), boot_plot,
       width = 7, height = 6, dpi = 300, compression = "lzw")
  





# 3. E, S, and AC variation ~ archipelagos

###### load data and functions --------

source("R/Calculate_components_VU_FUN.R")

# load data in final format
data_all <- readRDS("outputs/30_all_VU_components_45_isl_266_BM_sp.rds")
# load island information and polygons
isl <- readRDS("data/derived-data/01_shp_45_major_isl_clean.rds")

###### plots --------
VU_sp_isl <- vulnerabilityFUN(data_all)
# join VU data with island information
VU_sp_isl_info <- dplyr::left_join(
  isl |> sf::st_drop_geometry() |> 
    dplyr::select(ID, Archip, Island) |>
    dplyr::mutate(Archip_clean = dplyr::case_when(
      Archip == "Azores" ~ "Azores", 
      Archip == "Canary Islands" ~ "Canaries",
      Archip == "Galapagos Islands" ~ "Galapagos",
      Archip == "Hawaii" ~ "Hawai‘i",
      Archip == "Mascarene Islands" ~ "Mascarenes",
      Archip == "Tristan da Cunha Islands" ~ "Tristan da Cunha" 
    )),
  VU_sp_isl
)

# create a ranking to make sure islands of each archipelago are together in the graph
arch_isl <- paste0(isl$Archip, isl$Island)
archip_order <- isl$Island[match(sort(arch_isl), arch_isl)] |> unique() 
VU_sp_isl_info$Island <- factor(VU_sp_isl_info$Island, levels = archip_order)

# calculate mean, median and sd value per island
VU_median_mean <- 
  VU_sp_isl_info |> 
  dplyr::group_by(Archip, Island, Archip_clean) |> 
  dplyr::summarise(
    Median_E = median(E),
    Mean_E = mean(E),
    SD_E = sd(E),
    Median_S = median(S),
    Mean_S = mean(S),
    SD_S = sd(S),
    Median_AC = median(AC),
    Mean_AC = mean(AC),
    SD_AC = sd(AC)) |> dplyr::ungroup() |>
  tidyr::pivot_longer(cols = Median_E:SD_AC) |>
  tidyr::separate(name, into=c("name", "metric"), sep="_")|>
  tidyr::pivot_wider(names_from=name, values_from=value)



ggplot(VU_median_mean)+
  geom_point(aes(x=Island, y = Mean, col = Archip_clean))+
  geom_errorbar(aes(y=Mean, ymin=Mean-SD, ymax=Mean+SD, x=Island, col=Archip_clean))+
  scale_color_manual("Archipelago", values = archip_col1)+
  theme_bw()+ xlab("Islands") + ylab("Mean marker value")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        legend.position ="none")+
  facet_wrap(~metric, nrow = 3, ncol = 1)



# 4. Sampling effort islands from GBIF

###### load data and functions --------

# load data in final format
data_all <- readRDS("outputs/30_all_VU_components_45_isl_266_BM_sp.rds")
# load island information and polygons
isl <- readRDS("data/derived-data/01_shp_45_major_isl_clean.rds")


# install.packages("magick")
# install.packages("png")

rgbif::map_fetch()

# https://techdocs.gbif.org/en/openapi/v2/maps

se_gbif <- rgbif::map_fetch(
  #hexPerTile=200,
  
  style="purpleYellow.point", #purpleYellow.point, classic.point, green.point
  source = "density", 
  format = "@4x.png",
  basisOfRecord = c("OBSERVATION","HUMAN_OBSERVATION","MACHINE_OBSERVATION"),
  return = "terra",
  plot_terra = FALSE,
  #base_style='gbif-dark'
)

se_gbif
terra::plot(se_gbif)
terra::hist(se_gbif)


# mask islands
se_isl <- terra::mask(se_gbif, isl)
terra::plot(se_isl)
terra::hist(exp(se_isl+1))
# extract values and aggregate at the island level
terra::hist((se_isl))


# initialise output df
se_isl_agg <- data.frame()

# for all islands in Weigelt, get summary of sampling effort
for(i in 1:nrow(isl)){
  # extract pop for each island
  se_isl_i <- terra::extract(se_isl, isl[i,])
  # remove islands without any se value
  if(sum(!is.na(se_isl_i$lyr.1))==0){next}
  # attribute island ID
  se_isl_i$ID <- isl$ID[i]
  # aggregate se value
  se_isl_agg_i <- se_isl_i |>
    dplyr::group_by(ID)|>
    dplyr::summarise(
      nb_cells = dplyr::n(),
      nb_no_na = sum(!is.na(lyr.1)),
      nb_zero_se = sum(lyr.1==0),
      med_se_isl = median(lyr.1, na.rm = T),
      mean_se_isl = mean(lyr.1, na.rm = T),
      min_se_isl = min(lyr.1, na.rm = T),
      max_se_isl = max(lyr.1, na.rm = T),
      sd_se_isl = sd(lyr.1, na.rm = T)
    )
  #print(i)
  se_isl_agg <- rbind(se_isl_agg, se_isl_agg_i)
}

se_isl_info <- dplyr::left_join(
  isl|> sf::st_drop_geometry() |> 
    dplyr::select(ID, Archip, Island) |>
    dplyr::mutate(Archip_clean = dplyr::case_when(
      Archip == "Azores" ~ "Azores", 
      Archip == "Canary Islands" ~ "Canaries",
      Archip == "Galapagos Islands" ~ "Galapagos",
      Archip == "Hawaii" ~ "Hawai‘i",
      Archip == "Mascarene Islands" ~ "Mascarenes",
      Archip == "Tristan da Cunha Islands" ~ "Tristan da Cunha" 
    )), 
  se_isl_agg)


library(ggplot2)
ggplot(se_isl_info)+
  geom_point(aes(y=mean_se_isl, x = Archip_clean, col = Archip_clean), position = "jitter")+
  geom_boxplot(aes(y=mean_se_isl, x = Archip_clean), fill = NA)+
  scale_color_manual(values = archip_col1)+
  theme_bw()+ ylim(c(0,1))+ xlab("")+ylab("Mean sampling effort per island") +
  geom_hline(yintercept = 0.5, lty = 2, col = "red") +
  geom_hline(yintercept = 0.75, lty = 2, col = "blue") +
  geom_hline(yintercept = 0.85, lty = 2, col = "green3") +
  theme(legend.position = "none")

ggplot(se_isl_info)+
  geom_histogram(aes(x = min_se_isl))+
  xlim(c(0,1))+
  theme_bw()





# 5. Spatial autocorrelation in residuals

#install.packages("sfsmisc")

testData = DHARMa::createData(sampleSize = 100, family = poisson(), 
                      spatialAutocorrelation = 3, numGroups = 1,
                      randomEffectVariance = 0)
fittedModel <- glm(observedResponse ~ Environment1 , data = testData, 
                   family = poisson() )
simulationOutput <- DHARMa::simulateResiduals(fittedModel = fittedModel)
DHARMa::testSpatialAutocorrelation(simulationOutput = simulationOutput, x = testData$x,
                           y= testData$y)

plot(simulationOutput)



# load data in final format
data_all <- readRDS("outputs/30_all_VU_components_45_isl_266_BM_sp.rds")
source("R/Calculate_components_VU_FUN.R")

# Calculate VU per species per island

VU_sp_isl <- vulnerabilityFUN(data_all)

# join VU data with island information
VU_sp_isl_info <- dplyr::left_join(
  isl |> sf::st_drop_geometry() |> 
    dplyr::select(ID, Archip, Island, Lat, Long) |>
    dplyr::mutate(Archip_clean = dplyr::case_when(
      Archip == "Azores" ~ "Azores", 
      Archip == "Canary Islands" ~ "Canaries",
      Archip == "Galapagos Islands" ~ "Galapagos",
      Archip == "Hawaii" ~ "Hawai‘i",
      Archip == "Mascarene Islands" ~ "Mascarenes",
      Archip == "Tristan da Cunha Islands" ~ "Tristan da Cunha" 
    )),
  VU_sp_isl
)

# create a ranking to make sure islands of each archipelago are together in the graph
arch_isl <- paste0(isl$Archip, isl$Island)
archip_order <- isl$Island[match(sort(arch_isl), arch_isl)] |> unique() 
VU_sp_isl_info$Island <- factor(VU_sp_isl_info$Island, levels = archip_order)
# calculate mean, median and sd value per island
VU_median_mean <- 
  VU_sp_isl_info |> 
  dplyr::group_by(Archip, Island, Archip_clean, Lat, Long) |> 
  dplyr::summarise(
    Median = median(VU),
    Mean = mean(VU),
    SD = sd(VU)) |> dplyr::ungroup()
isl_order <- VU_median_mean$Island[match(sort(VU_median_mean$Mean), VU_median_mean$Mean)]
VU_median_mean$Island_order <- factor(VU_median_mean$Island, levels = isl_order)


# ANOVA model

av_lm <- lm(Mean~Archip, VU_median_mean)
summary(av_lm)

simulationOutput <- DHARMa::simulateResiduals(fittedModel = av_lm)
DHARMa::testSpatialAutocorrelation(simulationOutput = simulationOutput, 
                                   x = VU_median_mean$Long,
                                   y= VU_median_mean$Lat)



# Mixed effect model

mods <- lme4::lmer(VU~Archip+(1|sci_name), VU_sp_isl_info)
mods
summary(mods)
lme4::fixef(mods)

plot(mods)
performance::check_singularity(mods)

simulationOutput <- DHARMa::simulateResiduals(fittedModel = mods)
DHARMa::testSpatialAutocorrelation(simulationOutput = simulationOutput, 
                                   x = VU_sp_isl_info$Long,
                                   y= VU_sp_isl_info$Lat)



