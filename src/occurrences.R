## Taxa occurrences ##
## Andre P. Silva ##
## January 2nd, 2025 ##

# Use IUCN species names
# downloaded manually, later find a way to download automatically through R
IUCN_mammals <- sf::st_read("./data/MAMMALS_TERRESTRIAL_ONLY/MAMMALS_TERRESTRIAL_ONLY.shp")
mammal_sps <- unique(IUCN_mammals$sci_name)

# for testing only
#speciesTest <- c("Alces alces", "Canis lupus")
#mammal_sps <- as.data.frame(mammal_sps) %>% dplyr::filter(mammal_sps %in% speciesTest)

# Extract occurrences available in GBIF (e.g. mammals). Filter species >30 records
gbif_taxon_keys <-
    as.data.frame(mammal_sps) %>%
    pull("mammal_sps") %>% #use the sps names from the file
    name_backbone_checklist() %>% #match to backbone
    filter(!matchType == "NONE") %>% #get matched names
    pull(usageKey) #get the GBIF taxon keys

test <- occ_download(
    pred_in("taxonKey", gbif_taxon_keys),
    format = "SIMPLE_CSV",
    user = "maria_ines_silva", # needs registration before, I think
    pwd = "SRIT2024!",
    email = "ncisines@gmail.com")

# check if download is finished
occ_download_wait('0055274-241126133413365')
gc()

# retrieve the download from GBIF to my computer
d <- occ_download_get(
    key = '0055274-241126133413365',
    path = "./data"
)

# import download to current session
gbif_data <- occ_download_import(d)

GBIF_mammal_sps <- 
    gbif_data %>%
    # remove occurrences without coordinates
    drop_na(c(decimalLatitude, decimalLongitude)) %>% 
    group_by(species) %>%
    # filter individuals with more than 30 occurrences
    dplyr::filter(n() > 30)

# Write species occurences, with the subselection of variables
write.csv(
    GBIF_mammal_sps[, c("species", "decimalLatitude", "decimalLongitude", "year")],
    "./data/GBIF_mammal_30+occurrences_speciesTest.csv",
    row.names = FALSE
    )
gc()
