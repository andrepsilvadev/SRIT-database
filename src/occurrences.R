## Taxa occurrences ##
## Andre P. Silva, Afonso Barrocal & Inês Silva ##
## 20 July 2025 ##

##########
# STEP 1 # get species names into an object (e.g. mammals species form IUCN files)
##########

species <- read.csv("./birdTraits_2025-07-19.csv") %>% 
  dplyr::pull(sci_name) %>% 
  unique() 

# for testing only
#speciesTest <- c("Alces alces", "Canis lupus")
#mammal_sps <- as.data.frame(mammal_sps) %>% dplyr::filter(mammal_sps %in% speciesTest)

##########
# STEP 2 # Extract occurrences available in GBIF (e.g. mammals). 
##########

gbif_taxon_keys <- species %>% #use the sps names from the file
    name_backbone_checklist() %>% #match to backbone
    filter(!matchType == "NONE") %>% #get matched names
    pull(usageKey) #get the GBIF taxon keys

# to download datasets from gbif credentials are necessary.
# Register at https://www.gbif.org/user/profile

##########
# STEP 3 # Download occurrences to own computer
##########

test <- occ_download(
    pred_in("taxonKey", gbif_taxon_keys),
    format = "SIMPLE_CSV",
    user = "" , # ADD USERNAME HERE
    pwd = "", # ADD PASSWORD HERE
    email = "") # ADD EMAIL ASSOCIATE WITH ACCOUNT 
test # check download key here

# check if download is finished
occ_download_wait('0017918-250717081556266')
invisible(gc())

# retrieve the download from GBIF to my computer
d <- occ_download_get(
    key = '0017918-250717081556266',
    path = ""
)
# with the download key we can go directly to gbif and download the folder with
# the data without running the script again

# import download to current session
gbif_data <- occ_download_import(d)

##########
# STEP 4 # Filter occurrences
##########

GBIF_mammal_sps <- 
    gbif_data %>%
    # remove occurrences without coordinates
    drop_na(c(decimalLatitude, decimalLongitude)) %>% 
    group_by(species) %>%
    # filter individuals with more than 30 occurrences
    dplyr::filter(n() > 30)

##########
# STEP 5 # Write final file
##########

# Write species occurences, with the subselection of variables
write.csv(
    GBIF_mammal_sps[, c("species", "decimalLatitude", "decimalLongitude", "year")],
    "./GBIF_birds_30+occurrences_speciesTest.csv",
    row.names = FALSE
    )
invisible(gc())
