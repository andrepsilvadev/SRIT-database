# SRIT - database modules

Note: it is suggested to run the scripts in the order below

[![Developer Access](https://img.shields.io/badge/Google%20Drive-Developer-blue?logo=google-drive&logoColor=white)](https://drive.google.com/drive/folders/1gur2K5EGnqHHCz1oui8ms6ZOz8LxGFQD?usp=drive_link) [![User Access](https://img.shields.io/badge/Google%20Drive-User-green?logo=google-drive&logoColor=white)](https://drive.google.com/drive/folders/1NFKtJSeuMdrmDR3_VyQ_AWVj3g9cWhKe?usp=drive_link)

## settings & libraries

```         
generalSettings.R <-
libraries.R <-
customFunctions.R <-
```

## occurrences

```         
occurrences.R <- download occurrence data for different taxa from GBIF or other sources  
```

## Traits

`mammalTraits.R` \<- (v2 being updated by Afonso)\
`birdTraits.R` \<- builds a comprehensive trait database for bird species that combines multiple open-access datasets

1.  Check for existing data

2.  Loads species lists from a local .rds file (birds_intersection_lists.rds)

3.  Import external bird trait datasets

    -   AVONET (Trophic level) — Tobias et al. 2022

    -   Santini et al. 2023 (Abundance, carrying capacity) — GEB

    -   Jetz et al. 2008 (Clutch size) — PLOS Biology

    -   Amniote database (Body mass) — Jones et al. 2009

    -   Bird et al. 2020 (Survival, reproduction, longevity)

4.  Merge all trait data & harmonize columns

5.  Incomplete records are removed to ensure consistent traits per species.

6.  Save end result as .csv file (birdTraits2\_<date>.csv)

7.  Produce a summary output

## suitability (general module)

```         
globalSuitability.R <- produces input suitability landscapes with species specific resolution for the different models
    (old simulatedLandscapes.R) 
```

Files for users and developers can be found at: <https://drive.google.com/drive/folders/1gur2K5EGnqHHCz1oui8ms6ZOz8LxGFQD?usp=drive_link>
