#########################
# EXPLORING BIRD TRAITS #
#########################
# Inês Silva
# 19 Mar 2025

# IF ANYONE NEEDS TO ACCESS EXTERNAL FILES USED IN THIS SCRIPT THERE IS A LINK IN MY TASKS
# GOOGLE DOCS WITH FULL ACCESS TO MY FOLDER 

# packages
library(readr)
library(readxl)
library(janitor)
library(taxize) # to get species taxonomic info
library(here)
library(dplyr)
library(stringr)
library(tidyr)
here()

####################
# GET BIRD SPECIES #
####################

birdTraits_2025_03_03 <- read_csv("C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/traits/birdTraits/externalData/birdTraits_2025-03-03.csv")

birdTraits_IUCN <- birdTraits_2025_03_03 %>% 
  dplyr::select(sci_name, Order, Family, # taxonomic info
                Mass, Mass.Source, # Weights
                Trophic.Level, # trophic position
                Range.Size, BIOME_NAME, CONTINENT) # habitat info

####################################
# GET DATA FROM DATABASES & PAPERS #
####################################

# WHAT WE ARE LOOKING FOR?
## Abundance
## Carrying capacity
## Net reproduction rate (or clutch size * years of reproduction)
## Dispersal distances (allometric scalling)
## Survival rates (allometric scalling)

# SANTINI ET AL 2023 -----------------------------------------------------------

## https://onlinelibrary.wiley.com/doi/10.1111/geb.13688
## abundance (Predicted_Density_n_km2)
## carrying capacity (Q75)

# We did not use the TETRADENSITY DATABASE because it uses literature, does not
# have up75 and has multiple records for the same species

santini2023 <- read_csv("C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/traits/birdTraits/externalData/geb13688-sup-0003-appendixs2.csv") %>% 
  dplyr::select(Species, `Predicted_Density_n_km2`, Q75) %>% 
  mutate(Species = str_replace(Species, "_", " "))

# JETZ ET AL 2008 --------------------------------------------------------------

## clutch size

## https://journals.plos.org/plosbiology/article?id=10.1371/journal.pbio.0060303

jetz2008 <- read_excel("C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/traits/birdTraits/externalData/jetz2008_avianClutch.xlsx") %>% 
  # select only important columns (remaining ones are just references which are not all in the same column)
  dplyr::select(Order, Family, Genus, Species, Clutch) %>% 
  mutate(sci_name = paste0(Genus," ", Species))

## AMNIOTE DATABASE-------------------------------------------------------------

## https://esajournals.onlinelibrary.wiley.com/doi/10.1890/15-0846R.1
## combined_clutch_size (litters_or_clutches_per_y, no)
## combined_longevity (longevity_y, years)
## sexual maturity (female_maturity_d, days)

Amniote2015 <- read_csv("C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/traits/birdTraits/externalData/Amniote_Database_Aug_2015.csv") %>%
  naniar::replace_with_na_all(condition = ~.x == -999) %>%
  mutate(sci_name = paste0(genus," ", species))
colnames(Amniote2015)

## The BodyMass trait is available in both the AVONET and AMNIOTE DATABASES (with
## the same units grams). Amniote has more sps available (16677>1688) with BodyMass data, but
## 56 are are only included in the AVONET database, so later on these two databases
## are coalesced

# sum(!is.na(birdTraits_2025_03_03$Mass)) #1688 sps
# sum(!is.na(Amniote2015$adult_body_mass_g)) #16677 sps
# 
# common_species <- intersect(
#   birdTraits_2025_03_03$sci_name[!is.na(birdTraits_2025_03_03$Mass)],
#   Amniote2015$sci_name[!is.na(Amniote2015$adult_body_mass_g)])
# 
# missing_from_amniote <- setdiff(
#   birdTraits_2025_03_03$sci_name[!is.na(birdTraits_2025_03_03$Mass)],
#   Amniote2015$sci_name[!is.na(Amniote2015$adult_body_mass_g)])


## Bird et al 2020 -------------------------------------------------------------

## https://conbio.onlinelibrary.wiley.com/doi/epdf/10.1111/cobi.13486
## longevity
## age at first reproduction
## survival rate

bird_et_al_2020 <- read_excel("C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/traits/birdTraits/externalData/cobi13486-sup-0002-tables2.xlsx") %>% 
  dplyr::select(Species, Adult_survival_M, Age_at_first_reproduction_M, Maximum_longevity_M)


#####################
# COMBINIG DATASETS #
#####################

birdTraits <- birdTraits_IUCN %>% 
  left_join(santini2023, by = c("sci_name" = "Species")) %>% # for abundance and carrying capacity
  left_join(jetz2008, by = c("sci_name" = "sci_name")) %>%  # for clutch 
  left_join(bird_et_al_2020, by = c("sci_name" = "Species")) %>% # for max age, age of first breeding and survival rate
  left_join(Amniote2015, by = c("sci_name" = "sci_name")) %>% 
  dplyr::filter(BIOME_NAME %in% c("Tropical & Subtropical Moist Broadleaf Forests", "Boreal Forests/Taiga")) %>% 
  dplyr::select(# taxonomic info
    sci_name, Order.x, Family.x,
    # clutch size
    Clutch,
    # mass (g) & trophic level
    Mass, adult_body_mass_g, Trophic.Level, 
    # habitat info
    Range.Size, BIOME_NAME, CONTINENT,
    # abundance & carrying capacity (no. indiv per km2)
    Predicted_Density_n_km2, Q75,
    # age & survival rates
    Adult_survival_M, Age_at_first_reproduction_M, Maximum_longevity_M) %>% 
  group_by(sci_name) %>%
  mutate(
    # combine body mass in g 
    combined_bodyMass_g = coalesce(adult_body_mass_g, Mass)) %>%
  separate(sci_name, into = c("Genus", "Species"), sep = " ", remove = FALSE, extra = "merge") # Separate sci_name
#sapply(birdTraits, function(x) sum(is.na(x))) # number NA per column

# get_taxonomy <- function(species_name) {
#   tryCatch({
#     classification <- classification(species_name, db = 'ncbi')[[1]]
#     if (!is.null(classification)) {
#       family <- classification$name[classification$rank == 'family'][1]
#       order <- classification$name[classification$rank == 'order'][1]
#       data.frame(Sci_name = species_name, Family_taxize = family, Order_taxize = order)
#     } else {
#       data.frame(Family_taxize = NA, Order_taxize = NA)
#     }
#   }, error = function(e) {
#     data.frame(Family_taxize = NA, Order_taxize = NA)
#   })
# }

#taxonomy_data <- lapply(birdTraits$sci_name, get_taxonomy)
#taxonomy_df <- bind_rows(taxonomy_data)
#write.csv(taxonomy_df, here("birds_taxonomy.csv")) 
taxonomy_df <- read_csv("C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/traits/birdTraits/birds_taxonomy.csv")

birdTraits_df <- birdTraits %>% 
  left_join(taxonomy_df, by = c("sci_name" = "Sci_name")) %>% 
  mutate(order = coalesce(Order.x, `Order_taxize`),
         family = coalesce(Family.x, Family_taxize)) %>%
  dplyr::select(-c(Order.x, Family.x, Order_taxize, Family_taxize)) %>% 
  dplyr::select(# taxonomy
    sci_name, order, family, Genus, Species,
    # traits from databases
    Clutch, combined_bodyMass_g, Predicted_Density_n_km2, Q75,  Adult_survival_M, Age_at_first_reproduction_M, Maximum_longevity_M, Trophic.Level,
    # habitat info
    BIOME_NAME, CONTINENT) %>%
  tidyr::drop_na(sci_name,Clutch,combined_bodyMass_g, Predicted_Density_n_km2, Q75,  Adult_survival_M, Age_at_first_reproduction_M, Maximum_longevity_M) %>% 
  distinct(sci_name, CONTINENT, .keep_all = TRUE) 

# see which and how many bird species we have
bird_sps <- unique(birdTraits_df$sci_name[birdTraits_df$BIOME_NAME == "Tropical & Subtropical Moist Broadleaf Forests"])
length(unique(birdTraits_df$sci_name))

# write traits into csv file
write.csv(birdTraits_df, "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/traits/birdTraits_20250705.csv", row.names = FALSE)

################################################################################

# OCCURRENCES #
library(rgbif)

gbif_taxon_keys <- bird_sps %>% #use the sps names from the file
 name_backbone_checklist() %>% #match to backbone
 filter(!matchType == "NONE") %>% #get matched names
 pull(usageKey) #get the GBIF taxon keys

occ_download(pred_in("taxonKey", gbif_taxon_keys),
           format = "SIMPLE_CSV",
           user = "maria_ines_silva", pwd = "SRIT2024!", email = "ncisines@gmail.com")

occ_download_wait('0016677-250426092105405') # checks if download is finished
gc()
d <- occ_download_get('0016677-250426092105405') #retrieve the download from GBIF to my computer
gbif_data <- occ_download_import(d) #load the download from my computer to Rstudio

GBIF_bird_sps <- gbif_data %>%
  drop_na(c(decimalLatitude, decimalLongitude)) %>% #remove occurrences without coordinates
  group_by(species) %>%
  filter(n() > 30)
#Write species occurences, with the following variables, species name, geographic coordinates and year of occurrence, into a csv file
write.csv(GBIF_bird_sps[, c("species", "decimalLatitude", "decimalLongitude", "year")],
          "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/traits/GBIF_birds_30+occurrences_20250705.csv",
          row.names = FALSE)
gc()
head(GBIF_bird_sps)

target_sps <- unique(GBIF_bird_sps$species) #species that inhabit tropical or boreal forests with more than 30 occurrences
#TOTAL = 105 target sps

# remove sps that even if they have compelet trait do not have more than 30 occ
birdTraits <- birdTraits %>% 
  dplyr::filter(sci_name %in% target_sps)

# write traits into csv file
write.csv(birdTraits_df, "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/traits/birdTraits_20250705.csv", row.names = FALSE)

rm(birdTraits_2025_03_03)
rm(Amniote2015, jetz2008, santini2023, taxonomy_df)
rm(bird_et_al_2020)
rm(birdTraits_IUCN)
gc()
rm(gbif_taxon_keys)
rm(d)

#######################################
## OLD STUFF - DELETE AFTER FINISHED ##
#######################################

## ADW QUAARDVARK ----------------------------------------------------------------

## https://animaldiversity.ummz.umich.edu/quaardvark/backpack/
## sexual maturity (age at sexual maturity for females, days) !!not really it is actually in hours !!

# age_longevity_birds <- read_excel(here("externalData","age_longevity_birds.xlsx")) %>% 
#   dplyr::select("Species", "Age at sexual or reproductive maturity (female or asexual) - average - days", "Lifespan (wild, undetermined) - undetermined - average - years") %>% 
#   # age is actually in hours (makes no sense, 1 year = 8760 hours)
#   mutate(age_sexual_maturity_days = `Age at sexual or reproductive maturity (female or asexual) - average - days`/24)
# 
