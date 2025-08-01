## Mammal traits ##
## Afonso Barrocal & Inês Silva ##
## August 01st, 2025 ##

setwd("C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE")

if (length(list.files(pattern = "mammalTraits_.*\\.csv$")) != 0) {
  mammalTraits_data <- read_csv(list.files(pattern = "mammalTraits_.*\\.csv$"))
} else {
  
  ## Step 1 - BIND TERRESTRIAL WITH FRESHWATER MAMMALS ##
  
  # printing message
  cat(bold(red("No file detected:"),green("creating mammal trait database...")))
  
  # import IUCN terrestrial mammals .shp
  IUCN_terrMammals <- sf::st_read("./SRIT_ANDRE/external_data/MAMMALS_TERRESTRIAL_ONLY/MAMMALS_TERRESTRIAL_ONLY.shp")
  
  # import IUCN freshwater mammals .shp
  IUCN_freshMammals <- sf::st_read("./MAMMALS_FRESHWATER/MAMMALS_FRESHWATER.shp")
  
  if (all(colnames(IUCN_freshMammals) == colnames(IUCN_terrMammals))) {
    # if all columns match, bind the 2 datasets
    combined_data <- rbind(IUCN_terrMammals, IUCN_freshMammals)
    
  } else {
    
    # see whcich columns in IUCN_mammals are missing in 
    missing_cols <- setdiff(names(IUCN_terrMammals), names(IUCN_freshMammals))
    
    # Add missing columns to galemys (filled with NA)
    IUCN_freshMammals[missing_cols] <- NA
    
    # Reorder galemys columns to match IUCN_mammals
    IUCN_freshMammals <- IUCN_freshMammals[, names(IUCN_terrMammals)]
    
    # Combine both datasets
    IUCN_mammals <- rbind(IUCN_terrMammals, IUCN_freshMammals)
  }
  
  # remove unecessary objects
  rm(IUCN_freshMammals, IUCN_terrMammals)
  invisible(gc())
  
  ## Step 2 - INTERSECT MAMMALS' RANGE DISTRIBUTION WITH ECOREGIONS & CONTINENTS ##
  
  # work on a flat earth
  sf_use_s2(FALSE)
  
  # import the biome data
  biomes <- read_sf("./SRIT_ANDRE/external_data/Ecoregions2017/Ecoregions2017.shp") %>%
    st_make_valid() %>%
    naniar::replace_with_na(list(BIOME_NAME = c("N/A"))) %>%
    dplyr::select('BIOME_NAME') %>%
    distinct() %>%
    drop_na()
  invisible(gc())
  
  # import the continent data (https://figshare.com/articles/dataset/Continent_Polygons/12555170)
  continent <- read_sf("./SRIT_ANDRE/external_data/continent/Continents.shp")[,2]
  
  # intersect ecoregions and continents shapefiles
  biomes <- st_intersection(st_make_valid(biomes),st_make_valid(continent))
  invisible(gc())
  
  # intersect IUCN and ecoregions and continents shapefiles
  biomes <- st_intersection(st_make_valid(biomes),st_make_valid(IUCN_mammals))
  invisible(gc())
  
  # subset of the necessary variables
  sps_biome <- biomes %>%
    dplyr::select('BIOME_NAME', 'sci_name', "CONTINENT") %>% 
    st_drop_geometry() %>%
    distinct()
  invisible(gc())
  
  # remove unnecessary objects
  rm(biomes,continent)
  invisible(gc())
  
  
  ## Step 3 - IMPORT TRAIT DATASETS ##
  
  # import traits from the IUCN shapefile
  sps_traits<- IUCN_mammals %>%
    distinct(sci_name, .keep_all = TRUE) %>%
    dplyr::select(sci_name, family, order_) %>%
    st_drop_geometry()
  invisible(gc())
  
  # import PHYLACINE1.2 data
  PHYLACINE1.2_traits <- read_csv("./SRIT_ANDRE/external_data/PHYLACINE_1.2.1/Data/Traits/Trait_data.csv")
  # modify columns as needed
  PHYLACINE1.2_traits$Binomial.1.2 <- str_replace_all(PHYLACINE1.2_traits$Binomial.1.2, "_", " ") #remove "_" between the generic name and the specific epithet and replace with a "space"
  PHYLACINE1.2_traits$Diet.Meat <- PHYLACINE1.2_traits$Diet.Vertebrate+PHYLACINE1.2_traits$Diet.Invertebrate
  
  # download PANTHERIA data
  #pantheria <- read_tsv("https://esapubs.org/archive/ecol/E090/184/PanTHERIA_1-0_WR05_Aug2008.txt") %>%
  # naniar::replace_with_na_all(condition = ~.x == -999)
  
  # import SANTINI et al 2022
  santini2022 <- read_excel("./SRIT_ANDRE/external_data/geb13476-sup-0002-tables1.xls") #reports error of RMSE_bvs instead of numeric (it's ok)
  
  # import ANIMAL DIVERSITY WEB data
  ADW_202404081457 <- read_excel("./SRIT_ANDRE/external_data/ADW_202404081457.xls")
  unique(ADW_202404081457$`Mating System`)
  
  # import COMBINE database
  combine <- read_csv("./SRIT_ANDRE/external_data/ecy3344-sup-0001-datas1/COMBINE_archives/trait_data_imputed.csv")
  
  # import home range data
  HomeRangeData <- read_csv("HomeRangeData_2025_04_11_1/HomeRangeData_2025_04_11.csv")
  #GetHomeRangeData
  
  # filter home range 
  HomeRange_sps <- HomeRangeData #%>% dplyr::filter(n() > 30)
  
  # processing data (tacking character values from the isopleth column)
  HomeRange_sps <- HomeRange_sps[!(HomeRange_sps$Isopleth_Size %in% c("Home range","Core area",
                                                                      "70 - 100","75-95","95-99",
                                                                      "60 - 90","65 - 90","50-95",
                                                                      ">95")),]
  
  # processing data (tacking NAs out)
  HomeRange_sps <- HomeRange_sps[-(which(is.na(HomeRange_sps$Isopleth_Size))),]
  # processing data (make isopleth column)
  HomeRange_sps$Isopleth_Size <- as.numeric(HomeRange_sps$Isopleth_Size)
  # processing data (selecting home ranges with isopleth that contain 90% or more)
  HomeRange_sps <- HomeRange_sps[HomeRange_sps$Isopleth_Size >= 90,]
  # calculate maximum and minimum Home Range dimension in km2
  HomeRange_sps <- HomeRange_sps %>%
    group_by(Species) %>%
    mutate(Max_Home_Range_km2 = max(Home_Range_km2),
           Mean_HomeRange_km2 = mean(Home_Range_km2),
           Min_Home_Range_km2 = min(Home_Range_km2)) %>%
    dplyr::select(Species, Max_Home_Range_km2,Min_Home_Range_km2, Mean_HomeRange_km2) %>%
    distinct(Species, .keep_all = TRUE)
  
  
  # table with info on the n of species per dataset
  datasets_info <- tibble(
    dataset = c("PHYLACINE1.2_traits", "santini2022", "ADW_202404081457", "combine", "HomeRange_sps"),
    n_sps = c(length(unique(PHYLACINE1.2_traits$Binomial.1.2)), length(unique(santini2022$Species)),
              length(unique(combine$iucn2020_binomial)), length(unique(combine$iucn2020_binomial)),
              length(unique(HomeRange_sps$Species))))
  invisible(gc())
  
  ################################################################################
  ## FINAL DATASET ##
  
  # combining traits
  combined_mammalTraits <- sps_traits %>%
    left_join(combine, by = c('sci_name' = 'iucn2020_binomial')) %>%
    left_join(PHYLACINE1.2_traits, by = c("sci_name" = "Binomial.1.2")) %>%
    #left_join(pantheria, by = c("sci_name" = "scientificNameStd")) %>% 
    left_join(ADW_202404081457, by = c('sci_name'= 'Species')) %>% 
    left_join(santini2022, by = c('sci_name'= 'Species')) %>% 
    left_join(HomeRange_sps, by = c('sci_name' ='Species')) %>%
    left_join(sps_biome, by = c('sci_name' = 'sci_name')) %>%
    mutate(Combined_HomRange_km2 = coalesce(Mean_HomeRange_km2, home_range_km2)) %>% 
    dplyr::select(
      # taxonomic info from IUCN & combine
      'sci_name', 'family.x', 'order_', 
      # Diet & Body size info from PHYLACINE 1.2
      'Diet.Meat', 'Diet.Plant','trophic_level','Mass.g',
      # Age & Reproduction info from COMBINE
      'max_longevity_d', 'weaning_mass_g','age_first_reproduction_d', 'litter_size_n', 'litters_per_year_n', 
      # Mating system from ADW
      'Mating System',
      # Abundance info from Santini et al 
      'PredMd','up75',
      # habitat info from ecorregionn, continents, homeRange pckg and COMBINE
      'BIOME_NAME', 'CONTINENT', 'Mean_HomeRange_km2', 'Max_Home_Range_km2', 'Min_Home_Range_km2', 'home_range_km2', 'Combined_HomRange_km2') %>%
    distinct() %>%
    #drop_na() %>% 
    filter(!if_all(3:22, is.na))
  
  # csv file with trait data
  write.csv(combined_mammalTraits, paste0("mammalTraits_",Sys.Date(),".csv"), row.names = FALSE)
  
  # summary infor
  complete_data_sps <-data.frame(unique(combined_mammalTraits$sci_name))
  names(complete_data_sps) <-"Species"
  print(paste("NºSpecies:",nrow(complete_data_sps)))
  mammalsPerBiomeContinet <- combined_mammalTraits %>%
    group_by(BIOME_NAME, CONTINENT) %>%
    summarise(n_species = n_distinct(sci_name), .groups = "drop") %>%
    arrange(desc(n_species))
  print(mammalsPerBiomeContinet)
}
