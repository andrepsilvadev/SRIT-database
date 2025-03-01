## Birds traits ##
## Afonso Barrocal ##
## March 1st, 2025 ##

####################################################################################
##                                                                                ##
## Error:   got 'std::bad_alloc'                                                  ##
## But it works - the 'ensure_multipolygons works!!!                              ##
## Bad_alloc issue might be solvable by dividing the shapefile into n-folds...    ##
## I'll try it by using the 'dismo::kfold' function with an 'if' condition!!!     ##
##                                                                                ##
####################################################################################

##########################################################################
## Things to change: ##
##   1. Update code so that it doesn't have to make spatial processing;
##   2. Check Wang et al. (2017) database;
##   3. Search for tropical trait dataset;
##   4. See what are the limiting factors in the database;
##   5. Make more comments;
##   6. (...)
##########################################################################

if (length(list.files(pattern = "birdTraits_.*\\.csv$")) != 0) {
  birdTraits_data <- read_csv(list.files(pattern = "birdTraits_.*\\.csv$"))
} else {
  
  ### Printing message
  cat(bold(red("No file detected:"),green("creating bird trait database...")))
  
  ## Extract trait data and build a R dataframe with extracted data
  
  # work on a flat Earth
  sf_use_s2(FALSE)
  
  # import bird distribution data from BirdLife (IUCN)
  IUCN_birds <- read_sf("trait_datasets/species/BOTW_2024_2.gpkg",
                        promote_to_multi = T, type = 0) %>%
    ensure_multipolygons() %>%
    st_make_valid()
  invisible(gc())

  # create vector with target bird families
  target_birds_families <- c("Bucerotidae", # hornbills
                             "Bucorvidae", # hornbills
                             "Ramphastidae", # toucans
                             "Picidae") # woodpeckers

  # retrieve species
  target_bird_species <- taxize::downstream(target_birds_families,
                                            downto = "species",
                                            db = "gbif") %>%
    do.call(rbind,.) %>%
    dplyr::select(name)
  target_bird_species <- unique(target_bird_species$name)

  # select target species from IUCN shapefile
  IUCN_birds_target <- IUCN_birds[IUCN_birds$sci_name %in% target_bird_species,]

  # remove unnecessary objects
  rm(target_birds_families,target_bird_species)
  invisible(gc())
  
  # import traits from the IUCN shapefile
  sps_traits <- unique(IUCN_birds_target$sci_name)
  
  # import AVONET data (https://doi.org/10.1111/ele.13898)
  avonet <- read_excel("trait_datasets/AVONET Supplementary dataset 1.xlsx", 
                       sheet = "AVONET1_BirdLife", na = "NA")
  
  # import Storchova & Hořák traits (https://doi.org/10.1111/geb.12709)
  storchova <- read_excel("trait_datasets/Storchova_traitsdata.xlsx", 
                                     sheet = "Sheet2", na = "NA")
  
  # table with info on the n of species per dataset
  datasets_info <- tibble(
    dataset = c("avonet", "storchova"),
    n_sps = c(length(unique(avonet$Species1)), length(unique(storchova$Species))))
  invisible(gc())
  
  # import the biome data
  biomes <- read_sf("ecoregions/Ecoregions2017.shp") %>%
    replace_with_na(list(BIOME_NAME = c("N/A"))) %>%
    select('BIOME_NAME') %>%
    distinct() %>%
    drop_na()
  invisible(gc())
  
  # import the continent data (https://figshare.com/articles/dataset/Continent_Polygons/12555170)
  continent <- read_sf("continent/Continents.shp")[,2]
  
  # intersect ecoregions and continents shapefiles
  biomes <- st_intersection(st_make_valid(biomes),st_make_valid(continent))
  invisible(gc())
  
  # intersect IUCN and ecoregions and continents shapefiles
  biomes <- st_intersection(st_make_valid(biomes),st_make_valid(IUCN_birds_target))
  invisible(gc())
  
  # subset of the necessary variables (biome and sps scientific names)
  sps_biome <- biomes %>%
    dplyr::select('BIOME_NAME', 'sci_name', "CONTINENT") %>% 
  # dplyr::filter(sci_name %in% total_target_sps) %>%
    st_drop_geometry() %>%
    distinct()
  invisible(gc())
  
  # remove unnecessary objects to increase internal memory
  rm(biomes,continent,IUCN_birds_target)
  invisible(gc())
  
  ################################################################################
  ## FINAL DATASET ##
  
  # combining traits
  combined_traits_data <- sps_traits %>%
    left_join(storchova, by = c('sci_name' = 'Species')) %>%
    left_join(avonet, by = c("sci_name" = "Species1")) %>%
    left_join(sps_biome, by = c('sci_name' = 'sci_name')) %>%
    distinct() %>% 
    drop_na()
  
  # csv file with trait data
  write.csv(combined_traits_data, paste0("birdTraits_",Sys.Date(),".csv"), row.names = FALSE)
  
  # summary infor
  complete_data_sps <-data.frame(unique(combined_traits_data$sci_name))
  names(complete_data_sps) <-"Species"
  print(paste("NºSpecies:",nrow(complete_data_sps)))
  
}
