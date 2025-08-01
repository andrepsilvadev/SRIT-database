## Birds traits ##
## Inês Silva & Afonso Barrocal ##
## 19 July 2025 ##

setwd("C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe")
if (length(list.files(pattern = "birdTraits_.*\\.csv$")) != 0) {
  birdTraits_data <- read_csv(list.files(pattern = "birdTraits_.*\\.csv$"))
} else {
  
  # Printing message
  cat(bold(red("No file detected:"),green("creating bird trait database...")))
  
  # get started from an RDS object saved in this repo
  birds_biomes <- read.csv("./traits/birdTraits/externalData/birds_intersection_biomes_2025-07-25.csv") %>% 
    rename(sci_name = sci_nam)
  
  # alternatively use the .rds object
  # birds_biomes <- readRDS(file = "birds_biomes.rds") %>% 
  #   rename(sci_name = sci_nam) %>% 
  #   st_drop_geometry()
  # invisible(gc())
  
  # import AVONET data (https://doi.org/10.1111/ele.13898)
  avonet <- read_excel("./traits/birdTraits/externalData/AVONET Supplementary dataset 1.xlsx", 
                       sheet = "AVONET1_BirdLife", na = "NA") %>% 
    # select important columns 
    dplyr::select(Species1, Trophic.Level) %>% # Range.Size comes from this database
    dplyr::mutate(
      Trophic_level_mod = ifelse(Trophic.Level == "Scavenger", "Carnivore", Trophic.Level))
  invisible(gc())
  
  # import santini et al. 2023 (https://onlinelibrary.wiley.com/doi/10.1111/geb.13688)
  santini2023 <- read_csv("./traits/birdTraits/externalData/geb13688-sup-0003-appendixs2.csv") %>% 
    # select important columns 
    dplyr::select(Species, `Predicted_Density_n_km2`, Q75) %>% 
    mutate(Species = str_replace(Species, "_", " "))
  invisible(gc())
  
  # import Jetz et al 2008 (https://journals.plos.org/plosbiology/article?id=10.1371/journal.pbio.0060303)
  jetz2008 <- read_excel("./traits/birdTraits/externalData/jetz2008_avianClutch.xlsx") %>% 
    # select important columns (remaining ones are just references which are not all in the same column)
    dplyr::select(Order, Family, Genus, Species, Clutch) %>% 
    mutate(sci_name = paste0(Genus," ", Species))
  invisible(gc())
  
  # import AMNIOTE database (https://esajournals.onlinelibrary.wiley.com/doi/10.1890/15-0846R.1)
  Amniote2015 <- read_csv("./traits/birdTraits/externalData/Amniote_Database_Aug_2015.csv") %>%
    naniar::replace_with_na_all(condition = ~.x == -999) %>%
    mutate(sci_name = paste0(genus," ", species))
  invisible(gc())
  
  # import Bird et al 2020 (https://conbio.onlinelibrary.wiley.com/doi/epdf/10.1111/cobi.13486)
  bird_et_al_2020 <- read_excel("./traits/birdTraits/externalData/cobi13486-sup-0002-tables2.xlsx") %>% 
    # select important columns 
    dplyr::select(Species, Adult_survival_M, Age_at_first_reproduction_M, Maximum_longevity_M)
  invisible(gc())
  
  ## FINAL DATABASE ##
  combined_birdTraits <- birds_biomes %>% 
    left_join(avonet, by = c("sci_name" = "Species1")) %>% # for trophic level
    left_join(santini2023, by = c("sci_name" = "Species")) %>% # for abundance and carrying capacity
    left_join(jetz2008, by = c("sci_name" = "sci_name")) %>%  # for clutch 
    left_join(bird_et_al_2020, by = c("sci_name" = "Species")) %>% # for max age, age of first breeding and survival rate
    left_join(Amniote2015, by = c("sci_name" = "sci_name")) %>%  # for adult body mass (grams)
    dplyr::select(sci_name, BIOME_NAME, CONTINENT,
                  Order, Family,
                  # clutch size
                  Clutch,
                  # mass (g) & trophic level
                  adult_body_mass_g, adult_body_mass_g, Trophic.Level, Trophic_level_mod,
                  # habitat info
                  BIOME_NAME, CONTINENT,
                  # abundance & carrying capacity (no. indiv per km2)
                  Predicted_Density_n_km2, Q75,
                  # age & survival rates
                  Adult_survival_M, Age_at_first_reproduction_M, Maximum_longevity_M) %>% 
    distinct() %>% 
    drop_na(c(Clutch, adult_body_mass_g, Trophic_level_mod,
              Predicted_Density_n_km2, Q75, Adult_survival_M,
              Age_at_first_reproduction_M, Maximum_longevity_M ))
  
  # csv file with trait data
  write.csv(combined_birdTraits, paste0("birdTraits_",Sys.Date(),".csv"), row.names = FALSE)
  
  # final check-ups & messages
  cat("No. of species:", length(unique(combined_birdTraits$sci_name)), "\n") # how many species
  
  sapply(combined_birdTraits, function(x) sum(is.na(x))) # number NA per column
  
  birdsPerBiomeContinet <- combined_birdTraits %>%
    group_by(BIOME_NAME, CONTINENT) %>%
    summarise(n_species = n_distinct(sci_name), .groups = "drop") %>%
    arrange(desc(n_species))
  View(birdsPerBiomeContinet)
}
