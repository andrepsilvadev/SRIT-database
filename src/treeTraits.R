## Tree traits ##
## Afonso Barrocal ##
## February 20th, 2025 ##

##########################################################################
## Things to change: ##
##   1. See what are the limiting factors in the database;
##   2. Make more comments;
##   3. (...)
##########################################################################

if (length(list.files(pattern = "treeTraits_.*\\.csv$")) != 0) {
  treeTraits_data <- read_csv(list.files(pattern = "treeTraits_.*\\.csv$"))
} else {
  
  ### Printing message
  cat(bold(red("No file detected:"),green("creating tree trait database...")))
  
  ## Extract trait data and build a R dataframe with extracted data
  
  # work on a flat Earth
  sf_use_s2(FALSE)
  
  # import tree distribution data from IUCN
  IUCN_trees <- read_sf("trait_datasets/TREES/TREES.shp")
  invisible(gc())
    
  # import traits from the IUCN shapefile
  sps_traits <- IUCN_trees %>%
    distinct(sci_name, .keep_all = TRUE) %>%
    dplyr::select(sci_name, family, order_) %>%
    st_drop_geometry()
  invisible(gc())
  
  # import life-history traits from BIEN database (https://doi.org/10.1111/2041-210X.12861)
  bien_traits <- do.call(rbind,lapply(sps_traits$sci_name, FUN = BIEN_trait_species))
  
  # collect trait names and units from BIEN database
  traits_info <- bien_traits %>% 
    select(trait_name,unit) %>%
    distinct() %>%
    drop_na()
  
  # export trait names and units
  if(!(file.exists("treeTraits_info_units.csv"))){
    write.csv(traits_info, "treeTraits_info_units.csv")
  }
  
  # remove unnecessary objects to free internal memory
  rm(traits_info)
  invisible(gc())
  
  # process continuous data from BIEN database
  tree_traits <- bien_traits %>% 
    select(scrubbed_species_binomial,
           trait_name,trait_value) %>%
    group_by(scrubbed_species_binomial,trait_name) %>%
    summarise(trait_value = mean(as.numeric(trait_value), na.rm = TRUE)) %>%
    pivot_wider(names_from = trait_name,
                values_from = trait_value) %>%
    select(scrubbed_species_binomial,
           `diameter at breast height (1.3 m)`,
           `leaf nitrogen content per leaf dry mass`,
           `leaf phosphorus content per leaf dry mass`,
           `plant flowering begin`,
           `plant flowering duration`,
           `seed mass`,
           `stem wood density`,
           `vessel lumen area`,
           `vessel number`,
           `whole plant height`)
  
  # process categorical data from BIEN database
  tree_traits2 <- bien_traits %>%
    pivot_wider(names_from = trait_name,
                values_from = trait_value) %>%
    select(scrubbed_species_binomial,
           `whole plant sexual system`,
           `whole plant dispersal syndrome`,
           `flower pollination syndrome`,
           `whole plant growth form`,
           `whole plant woodiness`,
           `whole plant vegetative phenology`,
           `whole plant growth form diversity`) %>%
    unique() 
  tree_traits2$`whole plant growth form` <- tolower(tree_traits2$`whole plant growth form`)
  tree_traits2 <- tree_traits2 %>%
    group_by(scrubbed_species_binomial) %>%
    summarise(across(everything(), ~ .[order(is.na(.))][1]), .groups = 'drop')
  
  # table with info on the n of species per dataset
  datasets_info <- tibble(
    dataset = c("iucn","iucn+bien_continuous","iucn+bien_categorical"),
    n_sps = c(length(unique(sps_traits$sci_name)),
              length(unique(tree_traits$scrubbed_species_binomial)),
              length(unique(tree_traits2$scrubbed_species_binomial))))
  invisible(gc())
  
  # import the biome data
  biomes <- read_sf("ecoregions/Ecoregions2017.shp") %>%
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
  biomes <- st_intersection(st_make_valid(biomes),st_make_valid(IUCN_trees))
  invisible(gc())
  
  # subset of the necessary variables (biome and sps scientific names)
  sps_biome <- biomes %>%
    dplyr::select('BIOME_NAME', 'sci_name', "CONTINENT") %>% 
    dplyr::filter(sci_name %in% sps_traits$sci_name) %>%
    st_drop_geometry() %>%
    distinct()
  invisible(gc())
  
  # remove unnecessary objects to increase internal memory
  rm(biomes,continent,IUCN_trees)
  invisible(gc())
  
  ################################################################################
  ## FINAL DATASET ##
  
  # combining traits
  combined_traits_data <- sps_traits %>%
    left_join(tree_traits, by = c('sci_name' = 'scrubbed_species_binomial')) %>%
    left_join(tree_traits2, by = c('sci_name' = 'scrubbed_species_binomial')) %>%
    left_join(sps_biome, by = c('sci_name' = 'sci_name')) %>%
    distinct() %>% 
    drop_na()
  
  # csv file with trait data
  write.csv(combined_traits_data, paste0("treeTraits_",Sys.Date(),".csv"), row.names = FALSE)
  
  # summary infor
  complete_data_sps <-data.frame(unique(combined_traits_data$sci_name))
  names(complete_data_sps) <-"Species"
  print(paste("NºSpecies:",nrow(complete_data_sps)))
  
}
