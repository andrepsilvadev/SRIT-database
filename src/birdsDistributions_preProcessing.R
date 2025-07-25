## Birds sps distributions Pre-processing
## Inês Silva
## 25 Jul 2025

# ?? WHY ?? File originating from birdLife available here (https://datazone.birdlife.org/contact-us/request-our-data)
# seem to have a few issues when handled so a different approach was taken compared to
# mammals. ensure_multipolygons() function (user-defined) was used to correct
# the file and dismo::kfold() function to split the file into multiple ones and
# the intersection of bird species distribution with biomes from Dinerstein et al. 2017.
# Some of the smaller section were discarded due to unresolved errors (unable to
# write specific file features)


##########
# STEP 1 # Get bird distribution from BirdLife
##########

# work on a flat Earth
sf_use_s2(FALSE)

# import bird distribution data from BirdLife (IUCN)
IUCN_birds <- read_sf("./BOTW_2024_2.gpkg", # REPLACE WITH FILE PATH ON YOUR WON COMPUTER
                      promote_to_multi = T, type = 0) %>%
  # "correct" file
  ensure_multipolygons() %>%
  st_make_valid()
invisible(gc())

##########
# STEP 2 # Divide the file into smaller subsets
##########

# set a seed for reproducibility
set.seed(123)
kfolds_n <- 10
IUCN_birds$k_folds <- dismo::kfold(IUCN_birds, k = kfolds_n)

output_dir <- "./traits/birdTraits/bird_folds_shp" # REPLACE WITH FOLDER PATH ON YOUR WON COMPUTER
# create new output directory
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# loop through each fold and save as a .shp file to save space in R
for (i in 1:kfolds_n) {
  # file path
  filepath <- file.path(output_dir, paste0("bird_fold_", i, ".shp"))
  # Write the shapefile
  st_write(IUCN_birds[IUCN_birds$k_folds == i, ], filepath, delete_layer = TRUE)
  rm(i, filepath)
}
rm(IUCN_birds, kfolds_n)
invisible(gc())


##########
# STEP 3 # Intersect each .shp file with the biomes from Dinerstein et al 2017
##########

# import the biome data
biomes <- read_sf("./Ecoregions2017/Ecoregions2017.shp") %>% # REPLACE WITH FILE PATH ON YOUR WON COMPUTER
  replace_with_na(list(BIOME_NAME = c("N/A"))) %>%
  dplyr::select('BIOME_NAME') %>%
  distinct() %>%
  drop_na()
invisible(gc())

# import the continent data (https://figshare.com/articles/dataset/Continent_Polygons/12555170)

continents_shp <- list.files("./continent", pattern = "\\.shp$", full.names = TRUE) # REPLACE WITH FILE PATH ON YOUR WON COMPUTER
continent <- read_sf(continents_shp) %>%
  st_make_valid()
rm(continents_shp)

# intersect ecoregions and continents shapefiles
biomes <- st_intersection(st_make_valid(biomes),st_make_valid(continent))
biomes <- biomes %>% 
  dplyr::filter(BIOME_NAME %in% c("Tropical & Subtropical Moist Broadleaf Forests", "Boreal Forests/Taiga", "Mediterranean Forests, Woodlands & Scrub", "Temperate Broadleaf & Mixed Forests"))
invisible(gc())
rm(continent)


output_dir <- "./traits/birdTraits/externalData/bird_folds_shp" # REPLACE WITH FOLDER PATH ON YOUR WON COMPUTER
paths <- list.files(output_dir, pattern = "*.shp", full.names = TRUE)

birds_feat <- list()
for (i in seq_along(paths)) {
  # get message
  message(paste("Reading file", i), ":", basename(paths[i]))
  # import shapefile
  feat <- read_sf(paths[i])
  # make intersections
  birds_feat[[i]] <- st_intersection(st_make_valid(feat),st_make_valid(biomes))
  # remove unnecessary objects
  rm(feat,i)
  invisible(gc())
}

rm(biomes)

# Save an object to an rds file
#saveRDS(birds_feat, file = "birds_intersection_lists.rds") # to be safe

### If necessary, shut down and clean up the R environment here ###

#birds_feat <- readRDS("birds_intersection_lists.rds")
birds_biomes <- do.call(rbind, birds_feat)
#rm(birds_feat)
invisible(gc())

# Save an object to an rds file
#saveRDS(birds_biomes, "birds_biomes.rds") # to be safe
#rm(birds_biomes)

### If necessary **AGAIN**, shut down and clean up the R environment here ###

#birds_biomes <- readRDS("birds_biomes.rds")

# drop geometry to save a to .csv file easily
birds_biomes <- birds_biomes %>%
  st_drop_geometry()

output_dir <- "./traits/birdTraits/externalData" # REPLACE WITH FOLDER PATH ON YOUR OWN COMPUTER
# file path
filepath <- file.path(output_dir, paste0("birds_intersection_biomes_2025-07-25", ".csv"))
write.csv(birds_biomes, file = filepath, row.names = FALSE)
