
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

gift_isl <- readRDS("data/raw-data/02_plant_gift_isl.csv")
isl_select <- data.table::fread("data/derived-data/01_selected_islands.csv")

unique(isl_select$Archip) %in% gift_isl$geo_entity_ref

# Hawaiian Islands and Hawaii sandy islands instead of Hawai
# Mascarenes instead of Mascarene Islands

archip <- c("Hawaiian Islands", "Hawaii sandy islands",
                "Galapagos Islands", "Canary Islands", "Azores", "Mascarenes")

isl_subset <- gift_isl[which(gift_isl$geo_entity_ref %in% archip), ] %>%
  distinct(.keep_all = TRUE)

# what percentage of isl records our islands represent?
nrow(isl_subset)/nrow(gift_isl)
table(isl_subset$geo_entity_ref)
length(unique(isl_subset$geo_entity))
n_occ_isl_arch <- isl_subset %>% group_by(geo_entity_ref, geo_entity) %>% count()
table(n_occ_isl_arch$geo_entity_ref)
#island names in GIFT
unique(isl_subset$geo_entity)
#island names in our db
isl_select$Island_name

db_gift <- isl_subset %>% distinct(geo_entity_ref, geo_entity, entity_ID) %>%
  rename(Island_name = geo_entity,
         Archip = geo_entity_ref) %>%
  mutate(Archip = if_else(Archip %in% c("Hawaiian Islands", "Hawaii sandy islands"), "Hawaii", Archip),
         Archip = if_else(Archip == "Mascarenes", "Mascarene Islands", Archip))

db_isl <- isl_select %>% distinct(Archip, Island_name) 
unique(db_isl$Archip)
unique(db_gift$Archip)

HelpersMG::d(db_isl[db_isl$Archip == "Galapagos Islands", ] %>% pull(Island_name))
db_gift[db_gift$Archip == "Galapagos Islands", ] %>% pull(Island_name)


# match by hand
corresp = c(
  "Ilha de Santa Maria" = "Santa Maria",
  "Ilha de Sao Miguel" = "São Miguel",
  "Ilha do Pico" = "Pico",
  "Ilha do Faial" = "Faial", 
  "Ilha de Sao Jorge" = "São Jorge", 
  "Ilha Terceira" = "Terceira",
  "Ilha Graciosa" ="Graciosa", 
  "Ilha das Flores" = "Flores",
  "Ilha do Corvo" = "Corvo",
  "El Hierro" = "El Hierro",
  "Isla de Gran Canaria" = "Gran Canaria",
  "La Gomera" =  "La Gomera",
  "Isla de Tenerife" = "Tenerife",
  "Isla de Fuerteventura" = "Fuerteventura",
  "Lobos" = NA,
  "Isla de La Palma" = "La Palma",
  "Isla de Lanzarote" = "Lanzarote",
  "Isla Graciosa" = NA,
  "Isla de Montana Clara" = NA,
  "Isla de Alegranza" = NA,
  "Island of Hawaii" = "Hawai'i Island",
  "Kahoolawe" = "Kaho'olawe Island", 
  "Lanai" = "Lana'i Island",
  "Maui" = "Maui Island",
  "Molokai" = "Moloka'i Island",
  "Sand Island" = NA,
  "Ford Island" = NA,
  "Oahu" = "O'ahu Island (incl. Mokoli'i Islet)",
  "Niihau" = "Ni'ihau Island",
  "Lehua" = "Lehua Island",
  "Kauai" = "Kaua'i Island",
  "Laysan Island" = "Laysan Island",
  "Lisianski Island" = "Lisianski Island",
  "Isla Espanola" = "Española", 
  "Isla Santa Maria" = "Floreana", 
  "Isla Santa Fe" = "Santa Fé", 
  "Isla San Cristobal" = "San Cristóbal", 
  "Isla Pinzon" = "Pinzón", 
  "Isla Santa Cruz" = "Santa Cruz, Galapagos", 
  "Isla Baltra" = NA, 
  "Isla Rabida" = NA, 
  "Isla Seymour" = NA, 
  "Isla Bartolome" = NA, 
  "Isla Fernandina" = "Fernandina", 
  "Isla San Salvador" = "Santiago, Galapagos", 
  "Isla Isabela" = "Isabela", 
  "Isla Genovesa" = "Genovesa", 
  "Isla Marchena" = "Marchena", 
  "Isla Pinta" = "Pinta", 
  "Isla Wolf" = "Wolf", 
  "Isla Darwin" = "Darwin",
  "Ilet du Gros Galet" = "La Réunion", 
  "Ile d' Ambre" = NA, 
  "Mauritius" = "Mauritius", 
  "Ile Rodrigues" = "Rodrigues"
)

corresp <- data.frame(isl_name_gift = corresp, Island_name = names(corresp))


# make the match between corresp and our islands
db_isl <- left_join(db_isl, corresp)

# match with gift
isl_subset <- inner_join(isl_subset %>% mutate(isl_name_gift = geo_entity), db_isl)

# count all island with species
n_occ_isl_arch <- isl_subset%>% distinct(work_species, Archip, Island_name) %>% 
  group_by(Archip, Island_name) %>% count()
n_occ_arch <- isl_subset %>% distinct(work_species, Archip) %>%
  group_by(Archip) %>% count()

# for the islands with checklists
# native vs non-native species
# https://biogeomacro.github.io/GIFT/articles/GIFT.html#distribution-of-species
# make a status column with distinct categories
colnames(isl_subset)
status <- isl_subset %>%
  mutate(Status = case_when(
    native == 1 & naturalized == 0 ~ "native",
    native == 1 & is.na(naturalized) ~ "native",
    native == 0 & is.na(naturalized) ~ "non-native",
    native == 0 & naturalized == 1 ~ "naturalized",
    native == 0 & naturalized == 0 ~ "non-native",
    is.na(native) & is.na(naturalized) ~ "unknown"
  )) %>%  distinct(work_species, Archip, Island_name, Status) %>% 
  group_by(Archip, Island_name, Status) %>% count()

ggplot(status, aes(y = Island_name, x = n, fill = Status))+
  geom_bar(stat = "identity")+
  facet_wrap(vars(Archip), scales = "free")

# proportion of native vs exotic species
status_w <- status %>% pivot_wider(names_from = Status, values_from = n) %>%
  mutate(naturalized = if_else(is.na(naturalized), 0, naturalized),
         `non-native` = if_else(is.na(`non-native`), 0, `non-native`)) %>%
  mutate(exotic = naturalized + `non-native`) %>%
  mutate(prop_exo = exotic/native)

ggplot(status_w, aes(x=native, y = exotic))+
  geom_point(aes(color = Archip), size = 3)+
  geom_smooth(method = lm)

cor.test(status_w$exotic,status_w$native)

ggplot(status_w, aes(x=Archip, y = prop_exo))+
  geom_boxplot()+
  geom_point(aes(color = Archip), alpha = .5, size = 3, position = "jitter")+
  geom_hline(yintercept = 1, lty=2)


# save island names with GIFT data
saveRDS(db_isl, "data/derived-data/11_isl_with_gift_data.rds")
# save nb of alien plants for exposure to IAS
saveRDS(status_w, "data/derived-data/11_nb_native_alien_plants.rds")




# 2. Traits -------------------------------------------------------------------


# Extract species names from selected islands
# keep only native species
sp <- isl_subset %>%
  mutate(Status = case_when(
    native == 1 & naturalized == 0 ~ "native",
    native == 1 & is.na(naturalized) ~ "native",
    native == 0 & is.na(naturalized) ~ "non-native",
    native == 0 & naturalized == 1 ~ "naturalized",
    native == 0 & naturalized == 0 ~ "non-native",
    is.na(native) & is.na(naturalized) ~ "unknown")) %>% 
  filter(Status=="native") %>% 
  distinct(work_species) %>%
  separate(work_species, c("genus","epithet"), extra = "merge", fill = "left")


#colnames(isl_subset)
#sp <- isl_subset 
length(unique(sp %>% pull(work_species)))

#### Get species range size ####

# get shapes of all polygons in GIFT
# gift_shapes <- GIFT_shapes() # takes ~20min
# saveRDS(gift_shapes, "data/raw-data/11_gift_shapes.rds")
gift_shapes <- readRDS("data/raw-data/11_gift_shapes.rds")
str(gift_shapes)
ggplot(gift_shapes)+
  geom_sf()


# get distribution for each species

# distrib <- data.frame()
# for (i in 1:nrow(sp)){ # takes time ~5h
#   distrib_temp <- GIFT::GIFT_species_distribution(
#     genus = sp$genus[i],
#     epithet = sp$epithet[i]
#   )
#   distrib <- bind_rows(distrib, distrib_temp)
#   
#   saveRDS(distrib, "data/raw-data/11_gift_distrib_native_plants_55isl.rds")
#   print(i)
# }
# 
# # only one species without info : i = 3577
# sp[3577,] # => Marchus kochia littorea
# # try with Marcus-kochia littorea
# distrib_temp <- GIFT::GIFT_species_distribution(
#   genus = "Marcus-kochia",
#   epithet = "littorea"
# )
# distrib <- bind_rows(distrib, distrib_temp)
# saveRDS(distrib, "data/raw-data/11_gift_distrib_native_plants_55isl.rds")


# calculate species range 

distrib <- readRDS("data/raw-data/11_gift_distrib_native_plants_55isl.rds")


head(distrib)

distrib_stat <- distrib %>%
  mutate(native = ifelse(native == 1, "native", "non-native"),
                              naturalized = ifelse(naturalized == 1, "naturalized",
                                                   "non-naturalized"),
                              endemic_list = ifelse(endemic_list == 1, "endemic_list",
                                                    "non-endemic_list")) %>%
  dplyr::select(entity_ID, work_species, native, naturalized, endemic_list) %>%
  mutate(Status = case_when(
    native == "native" & naturalized == "non-naturalized" ~ "native",
    native == "native" & is.na(naturalized) ~ "native",
    native == "non-native" & is.na(naturalized) ~ "non-native",
    native == "non-native" & naturalized == "naturalized" ~ "naturalized",
    native == "non-native" & naturalized == "non-naturalized" ~ "non-native",
    is.na(native) & is.na(naturalized) ~ "unknown"
  )) %>% distinct()

table(distrib_stat$Status)

# filter native and naturalized range 
# gives an idea to how much a plant is adapted to different bioclimatic conditions

distrib_area <- left_join(
  distrib_stat %>% filter(Status %in% c("native", "naturalized")),
  gift_shapes %>% sf::st_drop_geometry()
) %>% 
  filter(!is.na(area)) %>% # some geo entity have no correspondance (wider regions?)
  group_by(work_species, Status) %>%
  summarize(range_size = sum(area)) %>%
  pivot_wider(names_from = Status, values_from = range_size, values_fill = 0) %>%
  mutate(tot_range = native + naturalized) %>%
  rename(native_range = native,
         naturalized_range = naturalized)

ggplot(distrib_area)+
  geom_point(aes(x=native_range, y = tot_range))+
  geom_abline(slope = 1, intercept = 0)

ggplot(distrib_area)+
  geom_histogram(aes(x= tot_range))

nrow(distrib_area) == nrow(sp)



#### Get species level traits from GIFT ####

# Metadata for the trait table
tra_meta <- GIFT_traits_meta() # from this table we use the column Lvl3 in the next function

# Retrieving traits
tra <- GIFT_traits(trait_IDs = c(
  "2.1.1", # life history => lifecyle
  "1.1.1", "1.2.1", "1.6.2", # morphology => woodiness, growth form, max height
  "3.2.3", "3.7.1","3.7.2","3.3.1"# reproduction => seed mass, flowering time, dispersal syndrome
  ))


gift_isl_tra <- left_join(distrib_area, tra %>% select(work_ID:trait_value_3.3.1))

colnames(gift_isl_tra)

colSums(is.na(gift_isl_tra))/nrow(sp)

# for each island, how many traits 


###### ADD TRAITS FROM TRY #####

# 1. create a TRY user account
# 2. select the traits you need
try_tr <- read_tsv("data/raw-data/TRY_trait_list_tde202473114744.txt")
# 3. send a request on the TRY website, for all species
# requested traits (July 31st, 2024):
# plant growth form, dispersal syndrome, seed dry mass, 
# plant height, plant flowering time, seed bank type
# 4. wait for the request (2-3 days if only public data, >2 weeks if non-public ones)

# text file to read with rtry
# install.packages("rtry")

try <- rtry::rtry_import("data/raw-data/TRY_request_traits_35151_01082024012921/35151.txt")

large_data <- data.table::fread(
  "data/raw-data/TRY_request_traits_35151_01082024012921/35151.txt", sep = "\t")


try1 <- readr::read_tsv_chunked(
  "data/raw-data/TRY_request_traits_35151_01082024012921/35151.txt",
  chunk_size = 100000,
  col_names = TRUE,
  col_types = NULL,
  na = c("", "NA"),
  quoted_na = TRUE,
  quote = "\"",
  comment = "",
  trim_ws = TRUE,
  skip = 0,
  skip_empty_rows = TRUE
)

file.info("data/raw-data/TRY_request_traits_35151_01082024012921/35151.txt")$size
# 17 Go

head <- readr::read_delim(
  "data/raw-data/TRY_request_traits_35151_01082024012921/35151.txt", delim = "\t", n_max = 10)
View(head)
colnames(head)

# select only columns of interest
# and select plants from list
sp <- isl_subset %>%
  mutate(Status = case_when(
    native == 1 & naturalized == 0 ~ "native",
    native == 1 & is.na(naturalized) ~ "native",
    native == 0 & is.na(naturalized) ~ "non-native",
    native == 0 & naturalized == 1 ~ "naturalized",
    native == 0 & naturalized == 0 ~ "non-native",
    is.na(native) & is.na(naturalized) ~ "unknown")) %>% 
  filter(Status=="native") %>% 
  distinct(work_species) %>% pull(work_species)
  
  
# Define a function to process each chunck
process_chunk <- function(chunk, pos) {
  # Your processing code here
  chunk_short <- chunk %>%
    dplyr::select(DatasetID, SpeciesName, AccSpeciesName, TraitID, 
           TraitName, OrigValueStr, OrigUnitStr, StdValue, UnitName) %>%
    dplyr::filter(AccSpeciesName %in% sp | SpeciesName %in% sp)
  return(chunk_short)
}
process <- function(x, pos) subset(x, AccSpeciesName %in% sp) 

# Read the file in chunks

chunked_df <- readr::read_delim_chunked(
  "data/raw-data/TRY_request_traits_35151_01082024012921/35151.txt", 
  delim = "\t", 
  callback = DataFrameCallback$new(process), 
  chunk_size = 1000000 # Adjust the chunk size as needed
)
  

file_path <- "data/raw-data/TRY_request_traits_35151_01082024012921/35151.txt"

# Approximation of the number of lines

# open a connection with the file
con <- file(file_path, "r")
# Initialize a counter
line_count <- 0
# Read and count lines
while (length(readLines(con, n = 1000, warn = FALSE)) > 0) {
  line_count <- line_count + 1000
}
# Close the connection
close(con)
# Print the number of lines
print(line_count)

# tot_l = 33805000

all=data.frame()

for(i in 1:34){ # apparently only 11.10^6 rows
  end = i*1000000
  start = end-1000000
  
  temp <- readr::read_delim(
    "data/raw-data/TRY_request_traits_35151_01082024012921/35151.txt", 
    delim = "\t",
    skip = start,
    n_max = 1000000,
    col_names = colnames(head),
    col_select = c(Dataset, SpeciesName, AccSpeciesName, TraitID, 
                   TraitName, OrigValueStr, OrigUnitStr, StdValue, UnitName, Comment)
  ) %>% 
    dplyr::filter(AccSpeciesName %in% sp | SpeciesName %in% sp) %>%
    dplyr::filter(!is.na(TraitID)) %>%
    dplyr::mutate_all(as.character)
  
  all <- bind_rows(all, temp)
  print(i)
}

saveRDS(all, "data/derived-data/11_TRY_traits_for_selected_sp.RDS")

all <- readRDS("data/derived-data/11_TRY_traits_for_selected_sp.RDS")

length(unique(all$AccSpeciesName))

# aggregate the TRY traits at the species level
# and complete the GIFT database





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

