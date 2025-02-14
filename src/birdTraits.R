## Birds traits ##
## Afonso Barrocal ##
## February 14th, 2025 ##

##########################################################################
## Things to change: ##
##   1. Read Santini et al. (check abundances);
##   2. See what are the limiting factors in the database;
##   3. Make more comments;
##   4. (...)
##########################################################################

if (length(list.files(pattern = "birdTraits_.*\\.csv$")) != 0) {
  birdTraits_data <- read_csv(list.files(pattern = "birdTraits_.*\\.csv$"))
} else {
  
  ### Printing message
  cat(bold(red("No file detected:"),green("creating bird trait database...")))
  
  ## Extract trait data and build a R dataframe with extracted data
  
  # import bird distribution data from BirdLife (IUCN)
  IUCN_birds <- read_sf("trait_datasets/species/BOTW_2024_2.gpkg")
  
  # import traits from the IUCN shapefile
  sps_traits<- unique(IUCN_birds$sci_name)
  invisible(gc())
  
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
  
  # work on a flat Earth
  sf_use_s2(FALSE)
  
  # import the biome data
  biomes <- read_sf("ecoregions/Ecoregions2017.shp") %>%
    st_make_valid() %>%
    replace_with_na(list(BIOME_NAME = c("N/A"))) %>%
    select('BIOME_NAME') %>%
    distinct() %>%
    drop_na()
  invisible(gc())
  
  # import the continent data (https://figshare.com/articles/dataset/Continent_Polygons/12555170)
  continent <- read_sf("continent/Continents.shp")[,2] %>%
    st_make_valid()
  
  # intersect ecoregions and continents shapefiles
  biomes <- st_intersection(st_make_valid(biomes),st_make_valid(continent))
  invisible(gc())
  
  # intersect IUCN and ecoregions and continents shapefiles
  biomes <- st_intersection(biomes,IUCN_birds)
  invisible(gc())
  
  # subset of the necessary variables (biome and sps scientific names)
  sps_biome <- biomes %>%
    dplyr::select('BIOME_NAME', 'sci_name', "CONTINENT") %>% 
    dplyr::filter(sci_name %in% total_target_sps) %>%
    st_drop_geometry() %>%
    distinct()
  invisible(gc())
  
  # remove unnecessary objects to increase internal memory
  rm(biomes,continent,IUCN_birds)
  invisible(gc())
  
  ################################################################################
  ## FINAL DATASET ##
  
  # combining traits
  combined_traits_data <- sps_traits %>%
    left_join(storchova, by = c('sci_name' = 'iucn2020_binomial')) %>%
    left_join(avonet, by = c("sci_name" = "Species1")) %>%
    left_join(sps_biome, by = c('sci_name' = 'Species'))
  
  ## see later
    dplyr::select('sci_name', 'family.x', 'order_', 'BIOME_NAME', 'CONTINENT',
                  'Diet.Meat', 'Diet.Plant', 'Mass.g', 'max_longevity_d', 'weaning_mass_g',
                  'litter_size_n', 'litters_per_year_n', 'Mating System','PredMd','up75',
                  'trophic_level', 'Max_Home_Range_km2', 'Min_Home_Range_km2') %>%
    distinct() %>% 
    drop_na()
  
  # csv file with trait data
  write.csv(combined_traits_data, paste0("birdTraits_",Sys.Date(),".csv"), row.names = FALSE)
  
  # summary infor
  complete_data_sps <-data.frame(unique(combined_traits_data$sci_name))
  names(complete_data_sps) <-"Species"
  print(paste("NºSpecies:",nrow(complete_data_sps)))
}