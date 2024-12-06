# functions for cleaning coordinates from gbif
# and transforming gbif dataframe to spatial point dataframe


# clean coordinates, basic cleaning, no function applied
clean_coord_base <- function(gbif_occ){
  clean_occ <- gbif_occ |> 
    dplyr::filter(basisOfRecord %in% c("HUMAN_OBSERVATION", "MACHINE_OBSERVATION",
                                "OBSERVATION", "OCCURRENCE")) |>
    dplyr::filter(occurrenceStatus == "PRESENT")  |>
    dplyr::filter(decimalLatitude!= "" | decimalLongitude!= "") |>
    dplyr::mutate(LAT = as.numeric(decimalLatitude),
           LONG = as.numeric(decimalLongitude)) |>
    dplyr::filter(!(is.na(LAT)|is.na(LONG))) |>
    dplyr::select(taxonKey, speciesKey, species, 
                  LAT, LONG, coordinateUncertaintyInMeters,
                  countryCode, year, establishmentMeans)
  return(clean_occ)
}

# use CoordinateCleaner package to remove duplicates, occurrences in capitals or 
# in bdv institutions, country centroids, etc.
# /!\ outliers are not removed
# but points falling into seas are deteled
# thus make sure you only have terrestrial species 

flag_rm_terr <- function(coord){
  
  # require(CoordinateCleaner)
  
  applied_tests <- c("capitals", "centroids", "equal","gbif", "institutions",
                     "zeros", "duplicates")
  
  flags <- CoordinateCleaner::clean_coordinates(x = coord, 
                             lon = "LONG", 
                             lat = "LAT",
                             countries = "countryCode",
                             species = "species",
                             tests = applied_tests
  ) # most test are on by default
  
  ## keep only clean records
  dat_cl <- coord[flags$.summary,]
  
  print(paste0("the function has checked for ", applied_tests))
  print(summary(flags))
  
  print("all flagged records have been removed, only clean records remain")
  return(dat_cl)
}

sp_diff <- function(before, after){
  sp_before <- unique(before$species)
  sp_after <- unique(after$species)
  return(setdiff(sp_before, sp_after))
}

occ_to_spatial_point <- function(clean_occ){
  # create a list of spatial point dataframe for each species
  occ_sp_list <- vector(mode = "list",
                        length = length(unique(clean_occ$speciesKey)))
  names(occ_sp_list) <- sort(unique(clean_occ$speciesKey))
  
  for(sp in names(occ_sp_list)){
    WGScoor <-  clean_occ |>
      dplyr::filter(speciesKey == sp)
    coordinates(WGScoor)=~LONG+LAT
    proj4string(WGScoor)<- CRS("+proj=longlat +datum=WGS84")
    occ_sp_list[[sp]] <- WGScoor
  }
  return(occ_sp_list)
}