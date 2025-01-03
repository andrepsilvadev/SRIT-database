## libraries ##
## Andre P. Silva ##
## December 31st, 2024 ##

library(easypackages)
easypackages::packages(
  "RangeShiftR",
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
  prompt = FALSE)

### trait data is on github
#remotes::install_github("RS-eco/traitdata")
#devtools::install_github("RangeShifter/RangeShiftR-package", ref="main", force=TRUE)
# credentials::set_github_pat("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") # last token used for github connection)