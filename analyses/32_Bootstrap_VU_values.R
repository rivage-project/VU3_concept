
# Bootstrap for VU ranking -----

## load data and functions --------

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


