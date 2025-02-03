## Mammal traits ##
## Afonso Barrocal & Inês Silva ##
## February 3rd, 2025 ##

##########################################################################
## Things to change: ##
##   1. Read Santini et al. (check abundances);
##   2. See what are the limiting factors in the database;
##   3. See mass from HerbiTrait and CarniTrait (do they have new data?);
##   4. Make more comments;
##   5. (...)
##########################################################################

if (file.exists("combined_traits_data.csv")) {
  combined_traits_data <- read_csv("combined_traits_data.csv")
} else {

  ### EXTRACT HOME RANGE DATA ###
  
  # import home range data
  HomeRangeData <- GetHomeRangeData()
  
  # filter home range 
  HomeRange_sps <- HomeRangeData %>%
    dplyr::filter(Species %in% mammal_sps) %>%
    dplyr::filter(n() > 30)
  
  # create vector with all the species
  target_sps2 <- unique(HomeRange_sps$Species)
  target_sps <- mammal_sps
  tmp <- unique(c(target_sps, target_sps2))
  out <- data.frame(target_sps = target_sps[match(tmp, target_sps)],
                    target_sps2 = target_sps2[match(tmp, target_sps2)])
  sapply(out, function(x)  sum(is.na(x)))
  extra_sps_HomeRange <- out[is.na(out$target_sps), 2]
  total_target_sps <- c(target_sps, extra_sps_HomeRange)
  
  ### ADDITION 22nd APRIL ###
  ### MAX HOMERANGE PER SPECIES ###
  
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
           Min_Home_Range_km2 = min(Home_Range_km2)) %>%
    dplyr::select(Species, Max_Home_Range_km2,Min_Home_Range_km2) %>%
    distinct(Species, .keep_all = TRUE)
  
  ################################################################################
  
  ## Step 4 - Extract trait data and build a R dataframe with extracted data
  
  # import traits from the IUCN shapefile
  sf_use_s2(FALSE)
  sps_traits<- IUCN_mammals %>%
    distinct(sci_name, .keep_all = TRUE) %>%
    dplyr::filter(sci_name %in% total_target_sps) %>%
    dplyr::select(sci_name, family, order_) %>%
    st_drop_geometry()
  invisible(gc())
  
  # import PHYLACINE1.2 data
  PHYLACINE1.2_traits <- read_csv("trait_datasets/PHYLACINE_1.2.1/Data/Traits/Trait_data.csv")
  PHYLACINE1.2_traits$Binomial.1.2 <- str_replace_all(PHYLACINE1.2_traits$Binomial.1.2, "_", " ") #remove "_" between the generic name and the specific epithet and replace with a "space"
  PHYLACINE1.2_traits$Diet.Meat <- PHYLACINE1.2_traits$Diet.Vertebrate+PHYLACINE1.2_traits$Diet.Invertebrate
  
  # download PANTHERIA data
  pantheria <- read_tsv("https://esapubs.org/archive/ecol/E090/184/PanTHERIA_1-0_WR05_Aug2008.txt") %>%
  replace_with_na_all(condition = ~.x == -999)
  
  # import SANTINI et al 2022
  santini2022 <- read_excel("trait_datasets/geb13476-sup-0002-tables1.xls") #reports error of RMSE_bvs instead of numeric (it's ok)
  
  # import ANIMAL DIVERSITY WEB data (deprecated)
  ADW_202404081457 <- read_excel("trait_datasets/ADW_202404081457.xls")
  unique(ADW_202404081457$`Mating System`)
  
  # import COMBINE database
  combine <- read_csv("trait_datasets/ecy3344-sup-0001-datas1/COMBINE_archives/trait_data_imputed.csv")
  
  # table with info on the n of species per dataset
  datasets_info <- tibble(
    dataset = c("PHYLACINE1.2_traits", "santini2022", "ADW_202404081457", "combine"),
    n_sps = c(length(unique(PHYLACINE1.2_traits$Binomial.1.2)), length(unique(santini2022$Species)),
              length(unique(combine$iucn2020_binomial)), length(unique(combine$iucn2020_binomial))))
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
  biomes <- st_intersection(st_make_valid(biomes),st_make_valid(IUCN_mammals))
  invisible(gc())
  
  # subset of the necessary variables (biome and sps scientific names) from the mammals_forests dataframe
  sps_biome <- biomes %>%
    dplyr::select('BIOME_NAME', 'sci_name', "CONTINENT") %>% 
    dplyr::filter(sci_name %in% total_target_sps) %>%
    st_drop_geometry() %>%
    distinct()
  invisible(gc())
  
  # remove unnecessary objects to increase internal memory
  rm(biomes,continent,IUCN_mammals)
  invisible(gc())
  
  ################################################################################
  ## FINAL DATASET ##
  
  # combining traits
  combined_traits_data <- sps_traits %>%
    left_join(combine, by = c('sci_name' = 'iucn2020_binomial')) %>%
    left_join(PHYLACINE1.2_traits, by = c("sci_name" = "Binomial.1.2")) %>%
    #left_join(pantheria, by = c("sci_name" = "scientificNameStd")) %>% 
    left_join(ADW_202404081457, by = c('sci_name'= 'Species')) %>% 
    left_join(santini2022, by = c('sci_name'= 'Species')) %>% 
    left_join(HomeRange_sps, by = c('sci_name' ='Species')) %>%
    left_join(sps_biome, by = c('sci_name' = 'sci_name')) %>%
    dplyr::select('sci_name', 'family.x', 'order_', 'BIOME_NAME', 'CONTINENT',
                  'Diet.Meat', 'Diet.Plant', 'Mass.g', 'max_longevity_d', 'weaning_mass_g',
                  'litter_size_n', 'litters_per_year_n', 'Mating System','PredMd','up75',
                  'trophic_level', 'Max_Home_Range_km2', 'Min_Home_Range_km2') %>%
    distinct()
  
  # process family and order data
  combined_traits_data$family.x <- str_to_title(combined_traits_data$family.x)
  combined_traits_data$order_ <- str_to_title(combined_traits_data$order_)
  
  # prepared for RangeShifter
  combined_traits_data <- combined_traits_data %>%
    rename('Species' = 'sci_name',
           'Biome' = 'BIOME_NAME',
           'Region' = 'CONTINENT',
           'Family' = 'family.x',
           'Order' = 'order_',
           'IndsHaCell' = 'PredMd',
           'TargetHaDensity' = 'up75',
           'MaxAge' ='max_longevity_d',
           'Harem' = 'Mating System',
           'Stage0bodyMass' = 'weaning_mass_g',
           'Stage1Fecundity' = 'litter_size_n',
           'RepSeasons' = 'litters_per_year_n',
           'MaxHomeRange' = 'Max_Home_Range_km2', 
           'MinHomeRange' = 'Min_Home_Range_km2') %>%
    mutate(BodyMass = Mass.g/1000,  #convert mass in grams to kilograms
           Trophic = ifelse(Diet.Meat >= 90, "Carnivore",
                            ifelse(Diet.Plant >= 90, "Herbivore", "Omnivore")),
           Stage0Mortality = Stage0bodyMass^-0.25, #based on McCarthy 2008 and Savage 2004
           Stage1Mortality = BodyMass^-0.25,
           MeanDisp = ifelse(Trophic == "Carnivore", 3.45*BodyMass^0.89,  #based on Schloss 2012
                             ifelse(Trophic == "Herbivore", 1.45*BodyMass^0.54, NA)), 
           LongDisp = ifelse(Trophic == "Carnivore", 40.7*BodyMass^0.81,
                             ifelse(Trophic == "Herbivore", 3.31*BodyMass^0.65, NA)),
           MaxAge = MaxAge/365,
           trophic_level=replace(trophic_level, trophic_level==1, "Herbivore"),
           trophic_level=replace(trophic_level, trophic_level==2, "Omnivore"),
           trophic_level=replace(trophic_level, trophic_level==3, "Carnivore")) %>%
    dplyr::select('Species', 'Family', 'Order', 'Trophic', 'trophic_level',
                  'BodyMass', 'IndsHaCell', 'TargetHaDensity', 'MaxAge', 'Harem',
                  'Stage0bodyMass', 'Stage0Mortality', 'Stage1Fecundity', 'Stage1Mortality',
                  'RepSeasons', 'MeanDisp', 'LongDisp', 'MaxHomeRange', 'Biome', 'Region')
  
  # excluded incomplete data
  final_mammal_traits <- combined_traits_data %>% drop_na()
  
  # csv file with trait data
  write.csv(final_mammal_traits, paste0("mammalTraits_",Sys.Date(),".csv"), row.names = FALSE)
  
  complete_data_sps <-data.frame(unique(final_mammal_traits$Species))
  names(complete_data_sps) <-"Species"
  
  # summary info
  sps_summarised <- final_mammal_traits %>%
    group_by(Trophic, trophic_level, Family) %>%
    summarise(Mean_BodyMass = mean(BodyMass), n = n())
}
