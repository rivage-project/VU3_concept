
library("tidyverse")
library("GIFT")
library("data.table")

# 1. Distribution data --------------------------------------------------------
## 1.1. Retrieval -------------------------------------------------------------
# Distribution data for all islands in GIFT
gift_isl <- GIFT_checklists(taxon_name = "Tracheophyta",
                            floristic_group = c("all"),
                            complete_floristic = FALSE,
                            geo_type = c("Island"))
gift_isl_metadata <- gift_isl$lists # only metadata per reference
gift_isl <- gift_isl$checklists # species composition is present in this second object

# Add the names of the reference (archipelagos, etc)
gift_ref <- GIFT_references()
gift_isl <- left_join(gift_isl, gift_ref[, c("ref_ID", "geo_entity_ref")],
                      by = "ref_ID")
# Add the names of the regions (individual island only)
gift_regions <- GIFT_regions()
gift_isl <- left_join(gift_isl, gift_regions[, c("entity_ID", "geo_entity")],
                      by = "entity_ID")

# Re-positioning columns
gift_isl <- gift_isl %>% 
  relocate(geo_entity_ref, .after = ref_ID) %>%
  relocate(geo_entity, .after = entity_ID)

# save export from gift for islands
saveRDS(gift_isl, "data/raw-data/02_plant_gift_isl.csv")


## 1.2. Subset ----------------------------------------------------------------
isl_select <- fread("data/derived-data/01_selected_islands.csv")

unique(isl_select$Archip) %in% gift_isl$geo_entity_ref

# Hawaiian Islands and Hawaii sandy islands instead of Hawai
# Mascarenes instead of Mascarene Islands

isl_subset <- c("Hawaiian Islands", "Hawaii sandy islands",
                "Galapagos Islands", "Canary Islands", "Azores", "Mascarenes")

isl_subset <- gift_isl[which(gift_isl$geo_entity_ref %in% isl_subset), ]

# what percentage of isl records our islands represent?
nrow(isl_subset)/nrow(gift_isl)
table(isl_subset$geo_entity_ref)
length(unique(isl_subset$geo_entity))
n_occ_isl_arch <- isl_subset %>% group_by(geo_entity_ref, geo_entity) %>% count()
table(n_occ_isl_arch$geo_entity_ref)

# 2. Traits -------------------------------------------------------------------
# Metadata for the trait table
tra_meta <- GIFT_traits_meta() # from this table we use the column Lvl3 in the next function

# Retrieving traits
tra <- GIFT_traits(trait_IDs = c("1.6.2", # maximal height
                                 "1.3.1", # epiphyte
                                 "1.2.1", "1.2.2", # growth form
                                 "4.6.2")) # Leaf_length_max

gift_isl_tra <- left_join(isl_subset, tra,
                          by = c("work_ID", "work_species", "work_author"))


# 3. Documentation ------------------------------------------------------------
# the database GIFT has many IDs, corresponding to all types of units:
# ref_ID is the identification of a single reference
# list_ID for a list, one reference can have several lists inside (e.g. the flora
# of Canary islands can have a list per island of the archipelago inside)
# entity_ID for the regions, each region has its own shapefile
# work_ID for the species and genus_ID for the genera

# the floristic status is accessible in the columns native, naturalized and 
# endemic (two columns for endemism, one at the reference and one at the list
# level)
# when the status is questionable, a 1 is put in the corresponding quest_ column

# for the traits, each trait ID can be accessed by calling the metadata function
# for traits first
# when retrieving the traits, you get an average value of the individual raw
# trait values (or other summary statistics, for maximal height, we get the max,
# for categorical traits, the most frequent value)
# the value is stored in columns starting with trait_value
# the other columns refer to the agreement score for categorical traits, the
# coefficient of variation for continuous traits, and the references where we
# extracted the values from



table(isl_subset$geo_entity)
length(unique(isl_subset$geo_entity))
table()

