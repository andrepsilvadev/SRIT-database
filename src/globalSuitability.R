## Global Suitability Landscapes ##
## Andre P. Silva ##
## December 30th, 2024 ##

source("./src/suitabilityFunction.R")

# read ncdf4 input land-use file
rasterName <- "2015_Scenario1.tif"
## note: memory problems when converting the ncdf4 file with 33 layers
if (file.exists(paste0("./data/", rasterName))) {
  LU <- terra::rast(paste0("./data/", rasterName))
} else {
  # import original land-use map from Chen et al. 2020 but it can be used with any land-use or climate data
  # manually downloaded from SRIT google drive storage to project data folder
  gc()
  nc <- "GCAM_Demeter_LU_ssp1_rcp26_modelmean_2015"
  n <- c(1:33) # run into memory issues when running the 33 layers
  LU <- do.call(c, lapply(n,ncdf2rast,nc = "GCAM_Demeter_LU_ssp1_rcp26_modelmean_2015"))
  # export raster
  terra::writeRaster(LU, paste0("./data/",rasterName), overwrite = FALSE)
  gc()
 }

## select occurrences of sps with complete trait data - output from occurrences.R
GBIF_occurrences <-
 read.csv("./data/GBIF_mammal_30+occurrences_speciesTest.csv", stringsAsFactors = TRUE) %>% 
 na.omit() %>%
 dplyr::select(-year) %>%
 droplevels()

 gc()

# run suitability function for sps with complete trait data
# To complete this step we will need:
#     1. the land-use file
#     2. a list of dataframes with species occurrences

# split gbif occurrences in one dataframe per species
species_occ <- split(GBIF_occurrences, GBIF_occurrences$species)

# apply this function to all species dataframes inside a list with lapply
list_of_SI <- lapply(species_occ, getLUvalues2, rasterFile = LU)

gc()

# bind suitability values for all species in one dataframe
target_sps_Suit <- do.call(rbind, list_of_SI)

gc()

# remove row names since they're a separate column
row.names(target_sps_Suit) <- NULL

# remove uncessary objects
rm(list_of_SI)

gc()
# DELETE NEXT ROW IF NECESARY USE ALL SPECIES INSTEAD OF JUST 3 SPS
#target_sps_Suit <- target_sps_Suit[c(3, 7, 31),]

# remove space bewteen species names
target_sps_Suit$species <- gsub(" ", "", target_sps_Suit$species)

#the following line can be skipped. For some data it does not work, as the levels were already dropped before
# drop factors levels
#target_sps_Suit$species <- droplevels(target_sps_Suit) #for some reason filter levels are still showing up that's why I need this line
#gc()

# save global suitability landscapes
# THIS STEP WAS RUN IN COMPUTER CLUSTER - GAIA2 #

# split the target species suitability index values 
SIn_list <- split(target_sps_Suit, target_sps_Suit$species)

gc()

# names of the species, matching the order of SIn_list
species_names <- unique(target_sps_Suit$species)

# Loop through each SIn data frame
for (i in seq_along(SIn_list)) {
  
  # Extract the SIn data frame and species name
  SIn <- SIn_list[[i]]
  species_name <- species_names[i]
  
  # Remove the first column (assuming it's not part of the suitability values)
  SIn <- SIn[, -1]
  
  # creation of dummy raster with the same extent and resolution as LU
  r <- rast(ncol = 7200, nrow = 3600, xmin = -179.975,
            xmax = 179.975, ymin = -89.975,
            ymax = 89.975)
  
  # stacking dummy raster to match the number of suitability layers (33 layers)
  r <- rep(r, length(SIn))  # Adjust according to the number of columns in SIn
  
  # creating habitat suitability raster (part 1 - looping)
  for (x in seq_along(SIn)) {
    r[[x]] <- LU[[x]] * SIn[1, x]
  }
  
  # creating habitat suitability raster (part 2 - summing)
  r <- sum(r, na.rm = TRUE)
  
  # Create a new folder to save the clipped files
  output_folder <- "/home/ans58sk/Desktop/global_suitability_landscapes2"
  if (!dir.exists(output_folder)) {
    dir.create(output_folder)
  }
  
  # Export habitat suitability raster with species name in the file name
  output_file <- file.path(output_folder, paste0(species_name, "_suitability.tif"))
  writeRaster(r, filename = output_file, overwrite = TRUE)
  
  # Clean up memory
  rm(r)
  gc()
}

# Final cleanup
gc()
