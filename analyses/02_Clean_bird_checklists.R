# Open Birdlife shapefiles
# Previously separated on QGIS to open them in R
# Repair geometries + save valid geometries as .RDS

##### Make the geometry valid ####

# a <- sf::st_read("data/raw-data/Birdlife/A_1_4401_birds.shp")
# a <- sf::st_read("data/raw-data/Birdlife/B_4402_8802_birds.shp")
# a <- sf::st_read("data/raw-data/Birdlife/C_8803_13201_birds_valid.shp")
# a <- sf::st_read("data/raw-data/Birdlife/D_13202_17522_birds.shp")

for(i in 1:nrow(a)){
  # repair potential invalid geometry
  a[i,] <- sf::st_make_valid(a[i,])

  # if geometry is still invalid, decrease precision
  precision = 1e7
  while (!sf::st_is_valid(a[i,])){
    precision = precision/2
    a[i,] <- sf::st_make_valid(sf::st_set_precision(a[i,], precision))
    print(precision)
  }
  print(i)
}

# saveRDS(a, "data/derived-data/02_valid_A_1_4401_birds.rds")
# saveRDS(a, "data/derived-data/02_valid_B_4402_8802_birds.rds")
# saveRDS(a, "data/derived-data/02_valid_C_8803_13201_birds.rds")
# saveRDS(a, "data/derived-data/02_valid_D_13202_17522_birds.rds")


##### Intersection with selected islands ####

rm(list=ls())

# Load selected island shapefiles
isl <- readRDS("data/derived-data/01_shp_45_major_isl.rds")

# Load bird file names
file_names <- list.files("data/derived-data/", pattern = "02_valid", full.names = T)


# Extract checklists of native breeding birds on the 45 isl

bird_ckl <- data.frame()
#f <- file_names[4]
for(f in file_names){ # Loop on each bird file
 
  # open bird file
  bshp <- readRDS(f)
  bshp <- bshp |>
    # keep only native resident and breeding birds
    dplyr::filter(presence %in% c(1:3) & origin %in% c(1:2) & seasonal %in% c(1:2))
  
  print(f)
  print(nrow(bshp))
  
  # intersect bird ranges with island polygons
  inter <- sf::st_intersects(isl, bshp)
  
  names(inter) <- isl$ID
  
  for (i in 1:length(inter)){
    b <- bshp[inter[[i]],] |> sf::st_drop_geometry()
    if(nrow(b)>0){
      b$file <- gsub("data/derived-data/", "", f)
      b$ULM_ID <- isl$ID[i]
      bird_ckl <- dplyr::bind_rows(bird_ckl,b)
    }
  }

}

bird_ckl_ABC <- readRDS("data/derived-data/02_bird_ckl_45_isl_ABC.rds")
bird_ckl_ABCD <- dplyr::bind_rows(bird_ckl_ABC, bird_ckl)
saveRDS(bird_ckl_ABCD, "data/derived-data/02_bird_ckl_45_isl_ABCD.rds")


###### Check the data #####

bird_ckl_ABCD <- readRDS("data/derived-data/02_bird_ckl_45_isl_ABCD.rds")

SR <- bird_ckl_ABCD |>
  dplyr::distinct(sci_name, ULM_ID) |>
  dplyr::group_by(ULM_ID) |>
  dplyr::summarise(SR = dplyr::n())


# check if all species are extant

# load IUCN summary 
path="Z:/THESE/5_Data/IUCN_summary/MAJ_2023_06/"
bnp <- read.csv(paste0(path, "birds_nonpasserif/simple_summary.csv"))
bp <- read.csv(paste0(path, "birds_passerif/simple_summary.csv"))
cate <- dplyr::bind_rows(bnp, bp) |> dplyr::select(scientificName, redlistCategory)

sp_list <- dplyr::left_join(bird_ckl_ABCD |> dplyr::distinct(sci_name) |> dplyr::rename(scientificName=sci_name),
                     cate)
table(sp_list$redlistCategory)

# Are all species in the IUCN Red List?
length(unique(bird_ckl_ABCD$sci_name))
# Yes, 270 species in total


#### Compare with previous data

ckl_before <- readRDS("data/derived-data/12_bird_ckl_islands_clean.RDS")
sp_before <- ckl_before |>
  dplyr::group_by(ULM_ID, Island_name)|>
  dplyr::summarise(SR_before = dplyr::n()) 

length(unique(ckl_before$sci_name_IUCN))
# 219 species before
# 270 now 


# How did richness change?
compa <- dplyr::left_join(SR, sp_before)

library(ggplot2)
ggplot(compa)+
  geom_point(aes(x=SR_before, y = SR))
# more species on most islands, more homogeneous between islands


# check with françois azores
ckl_az <- bird_ckl_ABCD |> dplyr::filter(ULM_ID %in% (isl |> dplyr::filter(Archip == "Azores") |> dplyr::pull(ID)))

length(unique(ckl_az$sci_name))
# with Birdlife: 35 species, and 304 occurrences

ckl_az_bef <- ckl_before |> dplyr::filter(ULM_ID %in% (isl |> dplyr::filter(Archip == "Azores") |> dplyr::pull(ID)))

length(unique(ckl_az_bef$sci_name_IUCN))
# before: 27 species, and 174 occurrences

compa_id <- dplyr::full_join(bird_ckl_ABCD |> dplyr::distinct(sci_name, ULM_ID, yrmodified),
                      ckl_before |> dplyr::rename(sci_name = sci_name_IUCN))



can_ckl <- dplyr::left_join(bird_ckl_ABCD |> 
                              dplyr::distinct(sci_name, ULM_ID) |> 
                              dplyr::rename(ID=ULM_ID),
                     isl |> sf::st_drop_geometry()) |>
  dplyr::filter(Archip == "Canary Islands")


table(can_ckl$Island)


compa_can <- dplyr::full_join(can_ckl, 
                       ckl_before |> 
                         dplyr::filter(ULM_ID %in% can_ckl$ID) |> 
                         dplyr::mutate(control = 1) |>
                         dplyr::rename(sci_name = sci_name_IUCN,
                                ID = ULM_ID))


compa_can <- compa_can |> 
  dplyr::mutate(From_database=dplyr::if_else(is.na(Country), "Litterature_only", "In_both")) |>
  dplyr::mutate(From_database=dplyr::if_else(is.na(control), "Birdlife_only", From_database))

openxlsx::write.xlsx(compa_can, "data/derived-data/02_bird_checklist_canary_issue.xlsx")


######## Manual comparison for all archipelagos #########

# "From_database" column: 
# - In_both: species in the checklist I had before using the litterature + I found again 
# using the Birdlife data; 
# - Litterature only: species I found in the previous version but not with Birdlife; 
# - Birdlife only: species I found using Birdlife but I did not find before). 

# My next step on this point is to accept the species found "In both" as truely in the island, 
# confirmed by several sources, and then to go manually through all species "Litterature only" 
# or "Birdlife only" and check if I should confirm their presence or not. 

new_ckl <- dplyr::left_join(
  bird_ckl_ABCD |> dplyr::distinct(sci_name, ULM_ID) |> dplyr::rename(ID=ULM_ID),
  isl |> sf::st_drop_geometry())


table(new_ckl$Island)
length(unique(new_ckl$sci_name))


table(ckl_before$Island)
length(unique(ckl_before$sci_name_IUCN))
length(unique(ckl_before$ULM_ID))


compa_all <- dplyr::full_join(new_ckl, 
                       ckl_before |> 
                         dplyr::mutate(control = 1) |>
                         dplyr::rename(sci_name = sci_name_IUCN,
                                ID = ULM_ID)) |>
  dplyr::mutate(From_database=dplyr::if_else(is.na(Country), "Litterature_only", "In_both")) |>
  dplyr::mutate(From_database=dplyr::if_else(is.na(control), "Birdlife_only", From_database))


table(compa_all$From_database)

openxlsx::write.xlsx(compa_all, "data/derived-data/02_bird_checklist_6_archip_issue.xlsx")



# open manual file
# to get the precise insular list for JM (Canary)
all <- openxlsx::read.xlsx("data/derived-data/02_bird_checklist_6_archip_issue_manual.xlsx")

can <- all |> dplyr::filter(Archip=="Canary Islands" & Keep ==1) |>
  dplyr::distinct(sci_name, Archip, is_syno)

openxlsx::write.xlsx(can, "data/derived-data/02_bird_list_canary_ordered.xlsx")



############# Clean checklist after manual cleaning ################

rm(list=ls())

# Load selected island shapefiles
isl <- readRDS("data/derived-data/01_shp_45_major_isl.rds")

# read xlsx file

bckl <- openxlsx::read.xlsx("data/derived-data/02_bird_checklist_6_archip_issue_manual.xlsx") 

colSums(is.na(bckl))

bckl_clean <- bckl |>
  dplyr::filter(Keep == 1) |>
  dplyr::mutate(Island = dplyr::if_else(is.na(Island), Island_name, Island)) |>
  dplyr::filter(Island != "Isla Seymour") |> 
  dplyr::distinct(Archip, Island, sci_name, is_syno)

length(unique(bckl_clean$sci_name))
# 240 bird species, 1247 occurrences

colSums(is.na(bckl_clean))

table(bckl$Archip)


sr <- bckl_clean |>
  dplyr::select(Archip, Island, sci_name) |>
  dplyr::group_by(Archip, Island) |>
  dplyr::summarise(SR_current = dplyr::n())

ckl_before <- readRDS("data/derived-data/12_bird_ckl_islands_clean.RDS")
sp_before <- ckl_before |>
  dplyr::group_by(ULM_ID, Island_name)|>
  dplyr::summarise(SR_before = dplyr::n()) |>
  dplyr::rename(Island = Island_name)

compa <- dplyr::left_join(sr, sp_before)

ggplot(compa, aes(x = SR_before, y = SR_current, color = Archip))+
  geom_point()
# better after the cleaning!

# save clean checklist

saveRDS(bckl_clean, "data/derived-data/02_bird_ckl_45_isl.rds")


