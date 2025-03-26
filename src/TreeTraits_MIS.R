#########################
# EXPLORING TREE TRAITS #
#########################
# Inês Silva
# 07 Mar 2025

# packages
library(readr)
library(dplyr)
library(tidyr)
library(here)
library(xlsx)


#"C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/treeTraits/"

# JUST SO I KNEW WHAT TO LOOK FOR AND NOT CHOOSE SMALL LITTLE PLANTS
tree_genera <- c(
  # Tropical Forests
  "dipterocarpus", "swietenia", "ceiba",  # Large Canopy Trees
  "virola", "inga",                       # Mid-Canopy & Understory
  "cecropia", "ochroma",                   # Pioneer Species
  
  # Boreal Forests
  "picea", "abies", "pinus",               # Large Canopy Trees
  "betula", "larix",                        # Mid-Canopy & Understory
  "populus", "salix"                        # Pioneer Species
)

############
# DATASETS #
############

# IUCN TREE SHAPEFILE ----------------------------------------------------------

## taxonomic info
## https://www.iucnredlist.org/resources/spatial-data-download (Plants > Trees)

trees_shp <- sf::st_read(here("externalData/TREES","TREES.shp"))

#plot(trees_shp[trees_shp$sci_name == "Sequoia sempervirens",])

trees_IUCN <- trees_shp %>% 
  # select taxonomic information
  dplyr::select(sci_name, order_, family, genus) %>% 
  sf::st_drop_geometry() %>% 
  mutate(genus = tolower(genus)) # use only lowercase letters

# LEDA DATABASE ----------------------------------------------------------------

## seed number (per ramet or plant)
## https://uol.de/en/landeco/research/leda/data-files (the first downloaded file need to have the first rows deleted)

seed_number <- read_delim(here("externalData", "seed_number.txt"), 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% 
  # select the same measurment unit as the next database
  dplyr::filter(`reproduction unit measured`  == "per ramet/tussock or individual plant") %>% 
  # select useful columns
  dplyr::select(`SBS name`, `single value`, `reference`) %>% 
  group_by(`SBS name`) %>%
  # average seed number values per species (ensures one row per species)
  summarise(`single value` = mean(`single value`, na.rm = TRUE),
            `reference` = paste(unique(reference), collapse = "; "), 
            .groups = "drop") %>% 
  rename("sci_name" = "SBS name")

# BOGDZIEWICZ ET AL 2023 -------------------------------------------------------

## seed number (per species)
## https://onlinelibrary.wiley.com/doi/10.1111/geb.13652 (supplementary materials)

tree_seedNumber <- read_delim(here("externalData/geb13652-sup-0001-supinfo", "GEB_13652_data_file.csv"), 
                              delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% 
  mutate(species_full = sub("_", " ", species_full))

# TRY DATABASE TRAITS ----------------------------------------------------------

## seed number (per ramet or plant)
## dispersal distances (m)
## planting density - maximum caryying capacity
## https://www.try-db.org/TryWeb/Prop1.php (select traitsID number from list and wait about 1 day to have file sent to email)

TRYtraits <- read_delim(here("externalData/39708_TRYdatabase", "39708.txt"), 
                            delim = "\t", escape_double = FALSE, 
                            trim_ws = TRUE) 
unique(TRYtraits$DataName)
invisible(gc())

TRYtraits_df <- TRYtraits %>%
  # select possibly useful traits
  dplyr::filter(DataName %in% c(
    "Seed germination efficiency (seed viability, germination rate, gemination success rate )",
    "Seed number per plant",
    "Seed number per reproducton unit (seed / ovule ratio)",
    "Disperal kernel measurement: distance",
    "Dispersal distance",
    "Planting Density per Acre, Maximum",
    "Dispersal distance max max",
    "Dispersal distance mean mean",
    "Seed number per ramet/tussock or individual plant",
    "Whole plant wet biomass",
    "Annual seed production")) %>%
  # combine trait name with measurement units
  mutate(trait_unit = paste(DataName, OrigUnitStr)) %>%
  pivot_wider(
    names_from = trait_unit,
    values_from = OrigValueStr) %>%
  # select useful traits with useful units
  dplyr::select(
    AccSpeciesName,
    TraitID,
    ValueKindName,
    `Seed germination efficiency (seed viability, germination rate, gemination success rate ) %`,
    `Seed number per plant 1/plant`,
    `Seed number per reproducton unit (seed / ovule ratio) 1/reproduction_unit`,
    `Disperal kernel measurement: distance m`,
    `Planting Density per Acre, Maximum 1/acre`,
    `Seed number per ramet/tussock or individual plant 1/plant`,
    `Whole plant wet biomass g`,
    `Annual seed production (number)`,
    `Dispersal distance max max m`,
    `Dispersal distance mean mean m`) %>%
  mutate(AccSpeciesName = as.character(AccSpeciesName)) %>%
  # ensure one row per species by averaging repeated values for the same trait
  group_by(AccSpeciesName) %>%
  summarise(across(
    .cols = c(
      `Seed germination efficiency (seed viability, germination rate, gemination success rate ) %`,
      `Disperal kernel measurement: distance m`,
      `Whole plant wet biomass g`,
      `Planting Density per Acre, Maximum 1/acre`,
      `Seed number per ramet/tussock or individual plant 1/plant`,
      `Dispersal distance max max m`,
      `Dispersal distance mean mean m`),
    .fns = ~ mean(suppressWarnings(as.numeric(.)), na.rm = TRUE))) %>%
  ungroup() %>%
  # create a genus and species column
  separate(AccSpeciesName, into = c("genus", "Species"), sep = " ", extra = "merge", remove = FALSE) %>%
  mutate(genus = tolower(genus)) %>%
  rename("sci_name" = "AccSpeciesName") %>%
  # Create mean and max dispersal distance columns
  mutate(
    mean_dispDist_m = coalesce(`Disperal kernel measurement: distance m`, `Dispersal distance mean mean m`),
    max_dispDist_m = `Dispersal distance max max m`) %>%
  select(-`Disperal kernel measurement: distance m`, -`Dispersal distance max max m`, -`Dispersal distance mean mean m`)


# LOSOSOVA ET AL 2023 ----------------------------------------------------------

## dispersal distances classes
## https://onlinelibrary.wiley.com/doi/full/10.1111/geb.13712 (supplementary material)
## https://link.springer.com/article/10.1007/s00035-007-0797-8

dispDist_2023 <- read_csv(here("externalData", "geb13712-sup-0001-supplementary.csv"))

dispDist_2023 <- dispDist_2023 %>% 
  # transform dispersal distance classes into mean dispersal distance
  mutate(mean_dispDist_m = case_when(
    `Dispersal distance class (1-6)` == 1 ~ 0.1, 
    `Dispersal distance class (1-6)` == 2 ~ 1,    
    `Dispersal distance class (1-6)` == 3 ~ 2,  
    `Dispersal distance class (1-6)` == 4 ~ 10,  
    `Dispersal distance class (1-6)` == 5 ~ 10,  
    `Dispersal distance class (1-6)` == 6 ~ 400,  
    TRUE ~ NA_real_ ),
    # transform dispersal distance classes into max dispersal distance
  max_dispDist_m = case_when(
    `Dispersal distance class (1-6)` == 1 ~ 1,
    `Dispersal distance class (1-6)` == 2 ~ 5,
    `Dispersal distance class (1-6)` == 3 ~ 15,
    `Dispersal distance class (1-6)` == 4 ~ 150,
    `Dispersal distance class (1-6)` == 5 ~ 500,
    `Dispersal distance class (1-6)` == 6 ~ 1500,
    TRUE ~ NA_real_)) %>% 
  rename("sci_name" = "Taxon")

# BIEN DATABASE ----------------------------------------------------------------

## diameter at breast height (1.3m)
## stem wood density
## whole plant height
## https://bien.nceas.ucsb.edu/bien/ (used the combined Trait databased compiled by Afonso)

treeTraits_2025_02_20 <- read_csv(here("externalData","treeTraits_2025-02-20.csv"))

# filtering for the DBH, stem wood desinty & whole plant height traits
treeTraits_2025_02_20 <- treeTraits_2025_02_20 %>% 
  separate(sci_name, into = c("genus", "Species"), sep = " ", extra = "merge", remove = FALSE) %>% 
  group_by(sci_name) %>%
  summarise(across(
    .cols = c(`diameter at breast height (1.3 m)`, `stem wood density`, `whole plant height`),
    .fns = ~ mean(suppressWarnings(as.numeric(.)), na.rm = TRUE)
  )) %>%
  ungroup() 

# PlantNE ----------------------------------------------------------------------

## plant wet biomass (g)

PlantNE_Biomass <- read_csv(here("externalData/ecy2840-sup-0001-supinfo/ecy2840-sup-0002-DataS1", "PlantNE_Biomass.csv")) %>% 
  filter(Unit == "g") %>% 
  group_by(Species) %>% 
  summarise(CK_M = mean(CK_M, na.rm = TRUE)) %>% 
  rename("sci_name" = "Species")

# GlobAllomeTree DATABASE ------------------------------------------------------

## plant height (H_m)
## above ground biomass (ABG_kg)
## continent
## ecoregion
## http://www.globallometree.org/data/raw-data/?q=&H_m__gte=&H_m__lte=&DBH_cm__gte=&DBH_cm__lte=&Volume_m3__gte=&Volume_m3__lte=&Family=&Genus=&Species=&Country=&Zone_FAO=&Zone_Holdridge=&Ecoregion_Udvardy=&Ecoregion_WWF=Tropical+and+subtropical+moist+broadleaf+forests&Division_Bailey=&Min_Latitude=&Max_Latitude=&Min_Longitude=&Max_Longitude=&Point_Latitude=&Point_Longitude=&Point_Distance=&Reference=&Reference_author=&Reference_year=&B=&Bd=&Bg=&Bt=&L=&Rb=&Rf=&Rm=&S=&T=&F=&page=1#

biomass <- read.table(here("externalData/GlobAllomeTree", "GlobAllomeTree_biomass.txt"), 
                      sep = "\t", 
                      header = TRUE, 
                      fileEncoding = "UTF-16LE", 
                      fill = TRUE, 
                      colClasses = "character") 

# clearing database
biomass <- biomass %>% 
  dplyr::select(`Family`, `Genus`, `Species`,
                `DBH_cm`, `H_m`, `ABG_kg`, `BGB_kg`, `Tot_Biomass_kg`,
                `Tree_type`,
                `Continent`, `Ecoregion_WWF`) %>% 
  mutate(DBH_cm = as.numeric(DBH_cm),
         H_m = as.numeric(H_m),
         ABG_kg = as.numeric(ABG_kg)) %>% 
  # remove rows for which species was unknown
  dplyr::filter(Species != "unknown") %>% 
  # create sci_name column
  mutate(sci_name = paste0(Genus, " ", Species)) %>% 
  dplyr::filter(sci_name %in% trees_IUCN$sci_name)

# average values per genus to compleemnt anything if we need to
biomass_genus <- biomass %>% 
  dplyr::select(`Family`, `Genus`, `Species`,
                `DBH_cm`, `H_m`, `ABG_kg`, `BGB_kg`, `Tot_Biomass_kg`,
                `Tree_type`,
                `Continent`, `Ecoregion_WWF`) %>% 
  mutate(DBH_cm = as.numeric(DBH_cm),
         H_m = as.numeric(H_m),
         ABG_kg = as.numeric(ABG_kg)) %>% 
  group_by(Genus) %>% 
  summarise(ABG_kg_avg = mean(ABG_kg, na.rm = TRUE))

######################
# COMBINING DATASETS #
######################


# attempt 2  - BEST OUTCOME IN MY OPNION #

## Uses TRY DATABASE for germination efficiency (%) & planting density (plants/acre)
## Uses Bogdziewicz et al 2023 for seed number per species (SN or SSP columns)
## Uses Lososová et al 2023 for dispersal distances (Dispersal distance class (1-6) column)
## Uses BIEN DATABASE for diameter at breast height (1.3m), stem wood density & whole plant height
## Can use the PLantNE for plant biomass in g

# Combine TRY DATABASE & Lososova data for dispersal distances
combined_dispDist <- bind_rows(
  TRYtraits_df %>% select(sci_name, mean_dispDist_m, max_dispDist_m),
  dispDist_2023
) %>%
  group_by(sci_name) %>%
  summarise(mean_dispDist_m = mean(mean_dispDist_m, na.rm = TRUE),
            max_dispDist_m = mean(max_dispDist_m, na.rm = TRUE)) %>%
  ungroup() %>%
  separate(sci_name, into = c("genus", "Species"), sep = " ", extra = "merge", remove = FALSE) %>%
  mutate(genus = tolower(genus)) 


attempt <- TRYtraits_df %>% 
  # join tree seed numbers from Bogdziewicz et al 2023
  full_join(tree_seedNumber, by = c("sci_name" = "species_full")) %>% 
  # join dispersal distances from Lososová et al 2023
  full_join(combined_dispDist, by = "sci_name") %>% 
  full_join(treeTraits_2025_02_20) %>% 
  #left_join(PlantNE_Biomass) %>% # join plant wet biomass from PlantNE  - no data for us
  #drop_na(SN, SSP) %>% # keep only species with seed number
  dplyr::select(sci_name,
                genus.x,
                `Seed germination efficiency (seed viability, germination rate, gemination success rate ) %`,
                `Planting Density per Acre, Maximum 1/acre`, SN, SSP,
                mean_dispDist_m.y, max_dispDist_m.y,
                `stem wood density`, `diameter at breast height (1.3 m)`, `whole plant height`) %>%
  rename(genus = `genus.x`, 
         germination_success_pct = `Seed germination efficiency (seed viability, germination rate, gemination success rate ) %`,
         planting_density_acre = `Planting Density per Acre, Maximum 1/acre`,
         seed_n = SN,
         seed_production = SSP,
         mean_dispDistance_m = mean_dispDist_m.y, 
         max_dispDist_m = max_dispDist_m.y, 
         wood_density = `stem wood density`,
         DBH_m = `diameter at breast height (1.3 m)`,
         plant_height_m = `whole plant height`) %>% 
  # filter for those existent in the IUCN shapefile
  dplyr::filter(sci_name %in% trees_IUCN$sci_name)


sapply(attempt, function(x) sum(is.na(x))) # number NA per column
sapply(attempt, function(x) sum(is.na(x)/length(x))) # proportion NA per column


# write  .CSV or .XLSX before averaging across genus
write.csv(attempt, file = file.path(here("treeTraitsBeforeAveragingPerGenus.csv")), row.names = FALSE)
#write.xlsx(attempt, file = file.path(here("treeTraitsBeforeAveragingPerGenus.xlsx")), sheetName = "Treeparameters", 
#           col.names = TRUE, row.names = FALSE, append = FALSE)

# IF WE WANT TO AVERAGE ACROSS GENUS GLUE THE NEXT CODE TO THE ONE BEFORE
  # average variables and use the genus-level averages to impute the missing values
  group_by(genus.x) %>%
  mutate(across(
    .cols = c(`Seed germination efficiency (seed viability, germination rate, gemination success rate ) %`,
              `Planting Density per Acre, Maximum 1/acre`,
              `mean_dispDist_m.y`, `max_dispDist_m.y`
    ),
    .fns = ~ ifelse(all(is.na(.)), NA_real_, # Handle all-NA case
                    ifelse(is.na(.), mean(as.numeric(.), na.rm = TRUE), as.numeric(.)))
  )) %>%
  ungroup() %>%
  # using traits from BIEN to estimate above ground biomass
  filter(complete.cases(.)) %>% 
  mutate(
    # estimate above ground biomass from Chaves et al. 2024 (pantropical equation)
    aboveGroundBiomass_drytonnes = 0.0673 *((`stem wood density` * (`diameter at breast height (1.3 m)`^2) * `whole plant height`)^0.976),
    # convert from dry tonnes to grams
    aboveGroundBiomass_g = aboveGroundBiomass_drytonnes * 1000000,
    # esyimate mortality rate from Marbà et al. 2007
    mortalityRate = aboveGroundBiomass_g^-0.25)

#write.csv(attempt, "C:/Users/maria/Desktop/treeTraits.csv")

# check NA'S per column
sapply(attempt, function(x) sum(is.na(x))) # number NA per column


################################################################################

# attempt 1 - NOT VERY SUCCESSFULL#

# Uses TRY DATABASE for seed number, germination efficiency, planting density,
# Uses LEDA to complement seed number
## Uses Lososová et al 2023 for dispersal distances
## Uses BIEN DATABASE for diameter at breast height (1.3m), stem wood density & whole plant height

# Combine TRY Database and LEDA for the most compelet seed numbers
trees <- bind_rows(TRYtraits_df, seed_number) %>% 
  dplyr::select(sci_name, `single value`, `Seed number per ramet/tussock or individual plant 1/plant`)

# pivot seed numbers columns to the long format to "merge" column from two sources
trees_seedNumber <- trees %>%
  pivot_longer(cols = c(`single value`, `Seed number per ramet/tussock or individual plant 1/plant`),
               names_to = "source",
               values_to = "SeedNumber") %>%
  drop_na(SeedNumber) %>%
  filter(!is.nan(SeedNumber)) %>%
  group_by(sci_name) %>%
  filter(n() > 1 | n() == 1) %>% # do the next step if there is more than 1 value, or exactly 1 value.
  mutate(max_val = max(SeedNumber, na.rm = TRUE),
         min_val = min(SeedNumber, na.rm = TRUE),
         diff_percent = (max_val - min_val) / ((max_val + min_val)/2) * 100) %>%
  filter(n() == 1 | diff_percent <= 50 | is.na(diff_percent)) %>% # filter rows with only one value and those where the difference between thw two existing value sin under 50%
  summarise(SeedNumber = mean(SeedNumber, na.rm = TRUE), .groups = "drop") # remove NaN values 


trees_final <- trees_seedNumber %>% 
  # add the other traits from TRY DATABASE again
  left_join(TRYtraits_df) %>% 
  # add dispersal distance data from Bogdziewicz et al 2023
  left_join(dispDist_2023) %>% 
  # select needed variables
  dplyr::select(`sci_name`, `genus`, `Species`,`Seed germination efficiency (seed viability, germination rate, gemination success rate ) %`,
                `Planting Density per Acre, Maximum 1/acre`,
                `Whole plant wet biomass g`,
                `SeedNumber`,
                `mean_dispDist_m`, `max_dispDist_m`) %>% 
  drop_na(`Planting Density per Acre, Maximum 1/acre`) %>% # PLANTING DENSITY SEEMED TO BE THE LIMITING FACTOR
  left_join(treeTraits_2025_02_20) %>% 
  group_by(genus) %>%
  # average variables and use the genus-level averages to impute the missing values
  mutate(across(
    .cols = c(`Seed germination efficiency (seed viability, germination rate, gemination success rate ) %`,
              `Planting Density per Acre, Maximum 1/acre`,
              `SeedNumber`,
              `mean_dispDist_m`, `max_dispDist_m`,
              `diameter at breast height (1.3 m)`, `stem wood density`),
    .fns = ~ ifelse(all(is.na(.)), NA_real_, # Handle all-NA case
                    ifelse(is.na(.), mean(as.numeric(.), na.rm = TRUE), as.numeric(.)))
  )) %>%
  ungroup()


# check NA'S per column
sapply(trees_final, function(x) sum(is.na(x))) # number NA per column
