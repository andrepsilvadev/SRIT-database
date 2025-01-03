# Traits -------------------------------------------------------------
# Description: Takes external trait data from different sources and combines it in one dataframe "combined_traits_data.csv".
# Adapted from SRIT1.1.R

if (file.exists("data/combined_traits_data.csv")) {
  combined_traits_data <- read_csv("data/combined_traits_data.csv")
} else {

HomeRangeData <- GetHomeRangeData()
HomeRange_sps <- HomeRangeData %>%
  dplyr::filter(Species %in% mammal_sps) %>%
  dplyr::filter(n() > 30)

target_sps2 <- unique(HomeRange_sps$Species)

tmp <- unique(c(target_sps, target_sps2))
out <- data.frame(target_sps = target_sps[match(tmp, target_sps)],
                  target_sps2 = target_sps2[match(tmp, target_sps2)])


sapply(out, function(x)  sum(is.na(x)))
#dplyr::filter(out, target_sps %in% NA)
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

sf_use_s2(FALSE)
sps_traits<- IUCN %>%
  distinct(sci_name, .keep_all = TRUE) %>%
  dplyr::filter(sci_name %in% total_target_sps) %>%
  dplyr::select(sci_name, family, order_) %>%
  st_drop_geometry()

#Import PHYLACINE1.2 data
PHYLACINE1.2_traits <- read_csv("PHYLACINE_1.2.1/Data/Traits/Trait_data.csv")
PHYLACINE1.2_traits$Binomial.1.2 <- str_replace_all(PHYLACINE1.2_traits$Binomial.1.2, "_", " ") #remove "_" between the generic name and the specific epithet and replace with a "space"

#Download PANTHERIA data from the traitdata package
#data("pantheria")

#Import SANTINI et al 2022
santini2022 <- read_excel("geb13476-sup-0002-tables1.xls") #reports error of RMSE_bvs instead of numeric (it's ok)

#Import ANIMAL DIVERSITY WEB data
ADW_202404081457 <- read_excel("ADW_202404081457.xls")
unique(ADW_202404081457$`Mating System`)

#Import combine
combine <- read_csv("ecy3344-sup-0001-datas1/COMBINE_archives/trait_data_imputed.csv")

#table with info on the n of species per dataset
datasets_info <- tibble(
  dataset = c("PHYLACINE1.2_traits", "santini2022", "ADW_202404081457", "combine"),
  n_sps = c(length(unique(PHYLACINE1.2_traits$Binomial.1.2)), length(unique(santini2022$Species)), length(unique(combine$iucn2020_binomial)), length(unique(combine$iucn2020_binomial))))

gc()

## subset of the necessary variables (biome and sps scientific names) from the mammals_forests dataframe 
sps_biome <- mammals_forests %>%
  dplyr::filter(sci_name %in% total_target_sps) %>%
  dplyr::select('BIOME_NAME', 'sci_name') %>% #since mammals_forests is so big, only species and each correspondent biome were subseted 
  st_drop_geometry() %>% 
  distinct()

################################################################################
## FINAL DATASET ##

combined_traits_data <- sps_traits %>%
  left_join(combine, by = c('sci_name' = 'iucn2020_binomial')) %>%
  left_join(PHYLACINE1.2_traits, by = c("sci_name" = "Binomial.1.2")) %>%
  #left_join(pantheria, by = c("sci_name" = "scientificNameStd")) %>% 
  left_join(ADW_202404081457, by = c('sci_name'= 'Species')) %>% 
  left_join(santini2022, by = c('sci_name'= 'Species')) %>% 
  left_join(sps_biome, by = c('sci_name' = 'sci_name')) %>%
  left_join(HomeRange_sps, by = c('sci_name' ='Species')) %>%
  dplyr::select('BIOME_NAME','sci_name', 'family.x', 'order_', 'Diet.Vertebrate', 'Diet.Plant' , 'Mass.g', 'max_longevity_d', 'weaning_mass_g', 'litter_size_n', 'litters_per_year_n', 'Mating System','PredMd','up75', 'trophic_level', 'Max_Home_Range_km2', 'Min_Home_Range_km2') %>%
  rename('Biome' = 'BIOME_NAME',
         'Species' = 'sci_name', 
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
         Trophic = ifelse(Diet.Vertebrate >= 90, "Carnivore",
                          ifelse(Diet.Plant >= 90, "Herbivore", NA)),
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
  filter(BodyMass > 5) %>%
  filter(!Species %in% c("Macropus giganteus", "Dendrolagus lumholtzi")) %>% #remove kangaroo species
  drop_na('Biome','Trophic', 'BodyMass', 'IndsHaCell', 'TargetHaDensity', 'MaxAge', 'Harem', 'Stage0bodyMass', 'Stage0Mortality', 'Stage1Fecundity', 'Stage1Mortality', 'RepSeasons', 'MeanDisp', 'LongDisp')  %>%
  dplyr::select('Biome', 'Species', 'Family', 'Order', 'Trophic', 'trophic_level', 'BodyMass', 'IndsHaCell', 'TargetHaDensity', 'MaxAge', 'Harem', 'Stage0bodyMass', 'Stage0Mortality', 'Stage1Fecundity', 'Stage1Mortality', 'RepSeasons', 'MeanDisp', 'LongDisp', 'MaxHomeRange') 

#csv file with trait data
write.csv(combined_traits_data, "combined_traits_data.csv", row.names = FALSE)

complete_data_sps <-data.frame(unique(combined_traits_data$Species))
names(complete_data_sps) <-"Species"
################################################################################

#Calculate occurrences for each sps with complete data

GBIF_occurrences <- read_delim("0008674-240425142415019/0008674-240425142415019.csv", 
                                       delim = "\t", escape_double = FALSE, 
                                       trim_ws = TRUE)
GBIF_occurrences_filtered <- left_join(complete_data_sps, GBIF_occurrences, by =c("Species" = "species"))

occurrences_sps <- aggregate(numberOfOccurrences~Species, data=GBIF_occurrences_filtered, sum)


################################################################################

#summary info
sps_summarised <- combined_traits_data %>%
  group_by(Biome, Trophic, trophic_level, Family) %>%
  summarise(Mean_BodyMass = mean(BodyMass), n = n())
}
