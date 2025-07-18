########################
# BIRD TRAITS DATABASE #
########################
# Inês Silva & Afonso Barrocal
# 15 July 2025

##########
# STEP 1 # Get bird species distributions from IUCN (BirdLife)
##########

# Since this file is way to big and has some structural problems we need to use
# custim function ensure_multipolygons() to correct the file and then use the
# dismo package function kfold() to give each row a random number to use later 
# to split the bigger dataset into smaller ones (save each a .shp instead of .gpkg)


# work on a flat Earth
sf_use_s2(FALSE)

# import bird distribution data from BirdLife (IUCN)
IUCN_birds <- read_sf("C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/BOTW_2024_2.gpkg",
                      promote_to_multi = T, type = 0) %>%
  ensure_multipolygons() %>%
  st_make_valid()
invisible(gc())

str(IUCN_birds) 
length(unique(IUCN_birds$sci_name)) # potentially we can access 11181 sps

# set a seed for reproducibility
set.seed(123)
kfolds_n <- 30
IUCN_birds$k_folds <- dismo::kfold(IUCN_birds, k = kfolds_n)

output_dir <- "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/traits/birdTraits/bird_folds_shp"
# create new output directory
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# loop through each fold and save as SHAPEFILE!
for (i in 1:kfolds_n) {
  
  # file path
  filepath <- file.path(output_dir, paste0("bird_fold_", i, ".shp"))
  
  # Write the shapefile
  st_write(IUCN_birds[IUCN_birds$k_folds == i, ], filepath, delete_layer = TRUE)
  
  rm(i, filepath)
}
rm(IUCN_birds)
rm(kfolds_n)
invisible(gc())

##########
# STEP 2 # Intersect each .shp file with the biomes from xxxx
##########

# import the biome data
biomes <- read_sf("C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/traits/Ecoregions2017/Ecoregions2017.shp") %>%
  replace_with_na(list(BIOME_NAME = c("N/A"))) %>%
  dplyr::select('BIOME_NAME') %>%
  distinct() %>%
  drop_na()
invisible(gc())

# import the continent data (https://figshare.com/articles/dataset/Continent_Polygons/12555170)
continent <- read_sf("C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/traits/continent/Continents.shp")[,2] %>%
  st_make_valid()

# intersect ecoregions and continents shapefiles
biomes <- st_intersection(st_make_valid(biomes),st_make_valid(continent))
biomes <- biomes %>% 
  dplyr::filter(BIOME_NAME %in% c("Tropical & Subtropical Moist Broadleaf Forests", "Boreal Forests/Taiga", "Mediterranean Forests, Woodlands & Scrub", "Temperate Broadleaf & Mixed Forests"))
invisible(gc())
rm(continent)



output_dir <- "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/traits/birdTraits/bird_folds_shp"
birds_feat <- list()



paths <- list.files(output_dir, pattern = "*.shp", full.names = TRUE)
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
gc()

filepath <- file.path(output_dir, paste0("birds_biomes_intersection",".shp"))

# get everything together again
birds_biomes <- do.call(rbind, birds_feat)
# write into a new shapefile to avoid re-runnig very heavy code
st_write(st_make_valid(birds_biomes), filepath, delete_layer = TRUE)


##########
# STEP 3 # Filter species with available occurrences in GBIF (30+)
##########

# MAYBE HERV E I WILL NEED TO EXTRACT A NEW FILE SEE IF THERE ARE TOO MANY SPS
read_csv("C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/traits/GBIF_birds_30+occurrences_20250705.csv")

# get species names into an object
# filter the previous dataframe by the sps names object

##########
# STEP 4 # Intersect with choosen trait databases
##########

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

# JETZ ET AL 2008 --------------------------------------------------------------

## clutch size

## https://journals.plos.org/plosbiology/article?id=10.1371/journal.pbio.0060303

jetz2008 <- read_excel("C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/traits/birdTraits/externalData/jetz2008_avianClutch.xlsx") %>% 
  # select only important columns (remaining ones are just references which are not all in the same column)
  dplyr::select(Order, Family, Genus, Species, Clutch) %>% 
  mutate(sci_name = paste0(Genus," ", Species))

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

##########
# STEP 5 # Export the full database
##########

