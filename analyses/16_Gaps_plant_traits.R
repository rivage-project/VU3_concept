# Patterns in plant traits

rm(list = ls())
library("tidyverse")


#### Load native species list from the 5 archipelagos

isl_subset <- readRDS("data/derived-data/11_cklists_plants_islands.rds")

isl_nat <- isl_subset %>%
  mutate(Status = case_when(
    native == 1 & naturalized == 0 ~ "native",
    native == 1 & is.na(naturalized) ~ "native",
    native == 0 & is.na(naturalized) ~ "non-native",
    native == 0 & naturalized == 1 ~ "naturalized",
    native == 0 & naturalized == 0 ~ "non-native",
    is.na(native) & is.na(naturalized) ~ "unknown")) %>% 
  filter(Status=="native") # remove to have total nb of species

sp_list <- isl_nat %>%
  distinct(work_species, work_author, family, tax_group)

gift_isl_tra <- readRDS("data/derived-data/11_GIFT_traits_isl.rds") %>% ungroup()
sum(table(gift_isl_tra$trait_value_3.3.1))
gift_tr <- gift_isl_tra %>% 
  mutate(lifecycle = factor(trait_value_2.1.1, ordered = T, 
                            levels = c("annual","biennial","perennial")),
         woodiness = factor(trait_value_1.1.1, ordered = T,
                            levels = c("non-woody", "variable", "woody")),
         growthform = factor(trait_value_1.2.1, ordered = T,
                             levels = c("herb", "shrub", "tree"))) %>%
  rename(max_height = trait_value_1.6.2,
         seed_mass = trait_value_3.2.3,
         dispersal_mode = trait_value_3.3.1) %>%
  dplyr::select(work_species:tot_range, max_height, seed_mass, dispersal_mode, 
                lifecycle:growthform)

#mice::md.pattern(gift_tr,rotate.names = T)

# calculate nb of NA trait per species
gift_tr$nb_NA <- rowSums(is.na(gift_tr))


table(gift_tr$growthform)
sum(is.na(gift_tr$growthform))

isl_nat_tr <- left_join(isl_nat, gift_tr)

colnames(isl_nat_tr)

ggplot(isl_nat_tr, aes(y = Island_name))+
  geom_bar(stat = "count")+
  facet_wrap(vars(Archip), scales = "free")

#viridis::magma(7, direction = -1)
col_na = c("0" = "#FCFDBFFF", "1" = "#FEAF77FF", "2" = "#F1605DFF", 
           "3" = "#B63679FF", "4" = "#721F81FF", "5" = "#2D1160FF",
           "6" = "#000004FF")

# Proportion of plants within each Archip
# select a growth form, default is All
plot.na.archip <- function(growth_form = "All"){
  if (growth_form %in% c("herb", "shrub","tree")){
    isl_nat_tr <- left_join(isl_nat, gift_tr) %>% 
      # filter for growthform here
      filter(growthform==growth_form)
  } else {
    isl_nat_tr <- left_join(isl_nat, gift_tr)
  }
  archip1 <- isl_nat_tr %>% 
    select(Archip, work_species, family, tax_group, tot_range:nb_NA) %>% 
    distinct() %>%
    group_by(Archip) %>%
    summarize(nb_nat = n())
  
  archip2<- isl_nat_tr %>% 
    select(Archip, work_species, family, tax_group, tot_range:nb_NA) %>% 
    distinct() %>%
    group_by(Archip, nb_NA) %>%
    summarize(nb_sp = n())
  
  nb_na_archip <- left_join(archip2, archip1) %>%
    mutate(prop = nb_sp/nb_nat) %>%
    mutate(nb_NA=as.character(nb_NA))
  
  # Distribution of species with NA across archipelagos
  p <- ggplot(nb_na_archip, aes(x=Archip, y = prop, fill = nb_NA))+
    geom_bar(stat = "identity")+
    scale_fill_manual(values = col_na)+
    geom_text(aes(y = 1.05, label = nb_nat))+
    theme_classic() +
    ggtitle(growth_form)
  return(p)
}


all <- plot.na.archip()
tree <- plot.na.archip("tree")
shrub <- plot.na.archip("shrub")
herb <- plot.na.archip("herb")

plot.na.archip.family <- function(fam = "All"){
  if (fam != "All"){
    isl_nat_tr <- left_join(isl_nat, gift_tr) %>% 
      # filter for family here
      filter(family==fam)
  } else {
    isl_nat_tr <- left_join(isl_nat, gift_tr)
  }
  
  archip1 <- isl_nat_tr %>% 
    select(Archip, work_species, family, tax_group, tot_range:nb_NA) %>% 
    distinct() %>%
    group_by(Archip) %>%
    summarize(nb_nat = n())
  
  archip2<- isl_nat_tr %>% 
    select(Archip, work_species, family, tax_group, tot_range:nb_NA) %>% 
    distinct() %>%
    group_by(Archip, nb_NA) %>%
    summarize(nb_sp = n())
  
  nb_na_archip <- left_join(archip2, archip1) %>%
    mutate(prop = nb_sp/nb_nat) %>%
    mutate(nb_NA=as.character(nb_NA))
  
  # Distribution of species with NA across archipelagos
  p <- ggplot(nb_na_archip, aes(x=Archip, y = prop, fill = nb_NA))+
    geom_bar(stat = "identity")+
    scale_fill_manual(values = col_na)+
    geom_text(aes(y = 1.05, label = nb_nat))+
    theme_classic() +
    ggtitle(family)
  return(p)
}


all <- plot.na.archip.family()
palm <- plot.na.archip.family("Aracaceae")
aster <- plot.na.archip.family("Asteraceae")

all
palm
aster

ggpubr::ggarrange(all, tree, shrub, herb,
                  nrow = 2, ncol = 2, common.legend = T, legend = "right")

# na pattern for each growth form
growth_form="tree"
mice::md.pattern(
  isl_nat_tr %>% 
    filter(growthform==growth_form) %>%
    select(work_species, tot_range:woodiness) %>% distinct(),
  rotate.names = T)
colnames(isl_nat_tr)

table(gift_tr$lifecycle, gift_tr$woodiness)
table(gift_tr$growthform, gift_tr$woodiness)
table(gift_tr$growthform, gift_tr$lifecycle)

plot(gift_tr$growthform, gift_tr$lifecycle)
plot(gift_tr$lifecycle, gift_tr$woodiness)
plot(gift_tr$growthform, gift_tr$woodiness)


plot(gift_tr$growthform, gift_tr$max_height)

# Missing Plant max height depending on growth form:
na_height_arch <- isl_nat_tr %>%
  distinct(Archip, growthform, work_species, max_height) %>%
  group_by(Archip, growthform) %>%
  summarize(na_height = sum(is.na(max_height)),
            tot_n = n()) %>%
  mutate(prop = na_height/tot_n)

ggplot(na_height_arch, aes(x=Archip, y = prop, fill = growthform))+
  geom_bar(stat = "identity", position = position_dodge(.9))+
  geom_text(aes(y=1, label = tot_n), position = position_dodge(.9))+
  scale_fill_manual(values = c("herb" = "#31688EFF", "shrub" = "#35B779FF", "tree" = "#FDE725FF"))+
  theme_bw()+ ylab("Prop. of NA for plant max height")
position_dodge()


# Are NA rate and mean (max_height) correlated?

na_rate_height <- left_join(
  isl_nat_tr %>%
    distinct(Archip, work_species, max_height, Island_name) %>%
    group_by(Archip, Island_name) %>%
    summarize(na_height = sum(is.na(max_height)),
              mean_max_height = mean(max_height, na.rm = T)),
  isl_nat %>% group_by(Island_name) %>% count()
) %>% mutate(NA_prop = na_height/n)

ggplot(na_rate_height)+
  geom_point(aes(x=NA_prop, y= mean_max_height, color=Archip))+
  geom_smooth(aes(x=NA_prop, y= mean_max_height), method = lm)+
  theme_bw()

cor.test(na_rate_height$NA_prop, na_rate_height$mean_max_height)

# separate by family or taxonomic group?
length(unique(isl_nat_tr$family))
length(unique(isl_nat_tr$tax_group))
table(isl_nat_tr %>% distinct(work_species, tax_group) %>% pull(tax_group))
table(isl_nat_tr %>% distinct(work_species, family) %>% pull(family))

# get orders
# work_ids <- unique(isl_nat_tr$work_ID) # careful, != work_id in traits
# order <- GIFT::GIFT_taxgroup(work_ids, taxon_lvl = "order")
# order_id <- data.frame(work_ID = work_ids, 
#                        Order = order)
# saveRDS(order_id, "data/derived-data/16_Order_native_plants_isl.rds")

order_id <- readRDS("data/derived-data/16_Order_native_plants_isl.rds")

length(unique(order_id$Order))

isl_nat_tr <- left_join(isl_nat_tr, order_id)

length(unique(isl_nat_tr %>% filter(tax_group=="Angiospermae") %>% pull(Order)))
length(unique(isl_nat_tr %>% filter(tax_group=="Angiospermae") %>% pull(family)))


# nb of plants per order

order_tot <- order_id %>%
  group_by(Order) %>% count()

ggplot(order_tot, aes(x=reorder(Order, -n), y = n))+
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90))


# nb of NA per order
order2 <- isl_nat_tr %>% 
  distinct(work_species, nb_NA, Order) %>%
  group_by(Order, nb_NA) %>%
  summarize(nb_sp = n())

nb_na_order <- left_join(order2, order_tot) %>%
  mutate(prop = nb_sp/n) %>%
  mutate(nb_NA=as.character(nb_NA))

ggplot(nb_na_order, aes(x=reorder(Order, -n), y = nb_sp, fill = nb_NA))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = col_na)+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90))

ggplot(nb_na_order, aes(x=reorder(Order, -n), y = prop, fill = nb_NA))+
  geom_bar(stat = "identity")+
  scale_fill_viridis_d(option = "A" , direction = -1)+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90))



tax_group <- isl_nat_tr %>% 
  distinct(work_species, nb_NA, tax_group) %>%
  group_by(tax_group, nb_NA) %>%
  summarize(nb_sp = n())

tax_group2 <- isl_nat_tr %>% 
  distinct(work_species, tax_group) %>%
  group_by(tax_group) %>%
  summarize(nb_group = n())

nb_na_tax_group <- left_join(tax_group, tax_group2) %>%
  mutate(prop = nb_sp/nb_group) %>%
  mutate(nb_NA=as.factor(nb_NA))

ggplot(nb_na_tax_group, aes(x=reorder(tax_group, -nb_group), y = nb_sp, fill = nb_NA))+
  geom_bar(stat = "identity")+
  scale_fill_viridis_d(option = "A" , direction = -1)+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90))




# angiosperm orders
angio <- unique(isl_nat_tr %>% filter(tax_group=="Angiospermae") %>% pull(Order))

ggplot(nb_na_order %>% filter(Order %in% angio), 
       aes(x=reorder(Order, -n), y = nb_sp, fill = nb_NA))+
  geom_bar(stat = "identity")+
  scale_fill_viridis_d(option = "A" , direction = -1)+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90))



# Orders in all archip

ord_arc <- isl_nat_tr %>% 
  filter(Order %in% angio) %>% # keep only angiosperms
  distinct(Order, Archip) %>%
  group_by(Order) %>%
  count()

# only 18 orders are across the 5 archipelagos
# but it would be weird to focus on the most common orders...



# Focus on growth forms ?
table(gift_tr$growthform)
sum(is.na(gift_tr$growthform))

table(isl_nat_tr %>% 
        filter(Order %in% angio) %>% # keep only angiosperms
        distinct(work_species, growthform) %>%
        pull(growthform))
sum(is.na(isl_nat_tr %>% 
            filter(Order %in% angio) %>% # keep only angiosperms
            distinct(work_species, growthform) %>%
            pull(growthform)))

# focus on trees 
trees <- unique(isl_nat_tr %>% filter(growthform=="tree") %>% pull(work_species))








##################################################################

# get species name for NA

na_seed <- gift_tr %>% filter(is.na(seed_mass)) %>% pull(work_species)
na_height <- gift_tr %>% filter(is.na(max_height)) %>% pull(work_species)
na_growth <- gift_tr %>% filter(is.na(growthform)) %>% pull(work_species)
na_disp <- gift_isl_tra %>% filter(is.na(trait_value_3.3.1)) %>% pull(work_species)
na_cycle <- gift_isl_tra %>% filter(is.na(trait_value_2.1.1)) %>% pull(work_species)

# get potential synonyms for species with missing values

gift_name <- unique(c(na_disp, na_growth, na_height, na_seed, na_cycle))





