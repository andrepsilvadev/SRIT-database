## libraries ##
## Andre P. Silva & Afonso Barrocal ##
## January 7th, 2025 ##

library(easypackages)
easypackages::packages(
  "tidyverse",
  "readr",
  "tidyr",
  "dplyr",
  "pbapply",
  "gridExtra",
  "readxl",
  "ncdf4",
  "sf",
  "terra",
  "DescTools",
  "data.table",
  "ggplot2",
  "tibble",
  "stringr",
  "rnaturalearth",
  "rgbif",
  "DT",
  "HomeRange",
  "naniar",
  prompt = FALSE)

### trait data is on github
# remotes::install_github("RS-eco/traitdata")
# remotes::install_github("SHoeks/HomeRange", subdir='pkg') # this package also needs to be downloaded from github
# devtools::install_github("RangeShifter/RangeShiftR-package", ref="main", force=TRUE)
# credentials::set_github_pat("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") # last token used for github connection)
