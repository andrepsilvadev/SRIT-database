# SRIT - database modules

Note: it is suggested to run the scripts in the order below

[![Developer Access](https://img.shields.io/badge/Google%20Drive-Developer-blue?logo=google-drive&logoColor=white)](https://drive.google.com/drive/folders/1gur2K5EGnqHHCz1oui8ms6ZOz8LxGFQD?usp=drive_link) [![User Access](https://img.shields.io/badge/Google%20Drive-User-green?logo=google-drive&logoColor=white)](https://drive.google.com/drive/folders/1NFKtJSeuMdrmDR3_VyQ_AWVj3g9cWhKe?usp=drive_link)

## settings & libraries
         
`generalSettings.R` <-
`libraries.R` <-
`customFunctions.R` <-

## Traits

Trait data required by the scripts are available in the **SRIT shared Drive folder**, using the corresponding names.

`mammalTraits.R` \<- (v2 being updated by Afonso)\
`birdTraits.R` \<- builds a comprehensive trait database for bird species that combines multiple open-access datasets

⚙️ What the Scripts Do

Each script follows a consistent pipeline:

1. **Checks** for an existing `.csv` file — if found, it loads it directly.
2. **Loads** species distribution data and intersects it with ecoregion .shp to filter our target species.
3. **Imports** relevant trait datasets (see below).
4. **Merges & harmonizes** trait columns (e.g., species name, clutch size, trophic level, body mass).
5. **Exports** the final trait dataset as a dated `.csv`, and prints a summary.

These are the primary sources used in the bird and mammal trait scripts. You can customize this table as needed:

| Taxa     | Dataset Name                    | Retrieved information                              | Link / Source                      | Available? |
|----------|----------------------------------|---------------------------------------------------|------------------------------------|------------|
| Birds    | BOTW_2024_2.gpkg                | IUCN/BirdLife Species Distribution maps            | [Website Link](https://www.iucnredlist.org/resources/spatial-data-download)            | ⬜         |
| Birds    | AVONET Supplementary dataset 1.xlsx                         | Trophic level  | [Tobias et al. 2022](https://doi.org/10.1111/ele.13898) | ✅         |
| Birds    | geb13688-sup-0003-appendixs2.csv             | Abundance, carrying capacity                         | [Santini et al. 2023](https://doi.org/10.1111/geb.13688) | ✅         |
| Birds    | jetz2008_avianClutch.xlsx                | Clutch size                                          | [Jetz et al. 2008](https://doi.org/10.1371/journal.pbio.0060303) | ✅         |
| Birds    | Amniote_Database_Aug_2015.csv | Body mass                                            | [Myhrvold et al. 2015](https://doi.org/10.1890/15-0846R.1) | ✅         |
| Birds    | cobi13486-sup-002-tables2.xlsx             | Survival, reproduction, longevity                    | [Bird et al. 2020](https://doi.org/10.1111/cobi.13486) | ✅         |
| Mammals  | [Dataset Name Placeholder]       | [Trait description placeholder]                      | [URL / DOI Placeholder]            | ⬜         |
| Mammals  | [Dataset Name Placeholder]       | [Trait description placeholder]                      | [URL / DOI Placeholder]            | ⬜         |
| Mammals  | [Dataset Name Placeholder]       | [Trait description placeholder]                      | [URL / DOI Placeholder]            | ⬜         |
| Mammals  | [Dataset Name Placeholder]       | [Trait description placeholder]                      | [URL / DOI Placeholder]            | ⬜         |
| Mammals  | [Dataset Name Placeholder]       | [Trait description placeholder]                      | [URL / DOI Placeholder]            | ⬜         |
| Mammals  | [Dataset Name Placeholder]       | [Trait description placeholder]                      | [URL / DOI Placeholder]            | ⬜         |
| None  | continent.shp       | World continents shapefile                     | [URL / DOI Placeholder]            | ✅         |
| None  | Ecoregions2017.shp       | World Ecoregions shapefile                      | [Dinerstein et al. 2017](https://academic.oup.com/bioscience/article/67/6/534/3102935)            | ✅         |


> ✅ = Available in Drive folder  
> ⬜ = File too heavy (download to your own computer from source)


## occurrences

```         
occurrences.R <- download occurrence data for different taxa from GBIF or other sources  
```


## suitability (general module)

```         
globalSuitability.R <- produces input suitability landscapes with species specific resolution for the different models
    (old simulatedLandscapes.R) 
```

Files for users and developers can be found at: <https://drive.google.com/drive/folders/1gur2K5EGnqHHCz1oui8ms6ZOz8LxGFQD?usp=drive_link>
