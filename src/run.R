# Working directories for current simulation ----------------------------------
runname <- "example"
runpath <- file.path("./data", runname)
dir.create(runpath, showWarnings = TRUE)

# Settings & libraries ---------------------------------------------------------
source("./src/libraries.R") # libraries
source("./src/customFunctions.R") # customized functions
source("./src/generalSettings.R") # paths and spatial settings

# species occurrences -----------------------------------------------------------
source("./src/occurrences.R") # collect gbif species occurrences

# species traits ----------------------------------------------------------------
source("./src/mammalTraits.R") # combines mammal traits from online data sources

# global suitability landscapes --------------------------------------------------
source("./src/globalSuitability.R") # builds global suitability landscapes
