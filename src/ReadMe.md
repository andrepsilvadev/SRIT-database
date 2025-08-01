# SRIT - database modules

*Note: it is suggested to run the scripts in the order below*

[![Developer Access](https://img.shields.io/badge/Google%20Drive-Developer-blue?logo=google-drive&logoColor=white)](https://drive.google.com/drive/folders/1Fk4xWm7bE8PIMBsodZd9ks2awqs8SULa?usp=sharing) [![User Access](https://img.shields.io/badge/Google%20Drive-User-green?logo=google-drive&logoColor=white)](https://drive.google.com/drive/folders/1NFKtJSeuMdrmDR3_VyQ_AWVj3g9cWhKe?usp=drive_link)

## Settings & Libraries

`generalSettings.R` \<- create file directories to save inputs and outputs

`libraries.R` \<- install & loads all necessary packages to run the repo

`customFunctions.R` \<- load user created functions

## Traits

Traits datastets required by the scripts are available in the **SRIT shared Drive folders**, using the corresponding names.

`mammalTraits.R` \<- builds a trait dataset for both terrestrial and freshwater mammal species by combining multiple open-access datasets

<br>\
<br> `birdsDistributions_preProcessing.R` \<- Read and handle bird species distributions .gpkg file from IUCN/BirdLife, and intersect with ecorregions .shp file from Dinerstein et al. 2017 *This step was necessary because files were too big and code was computationally demanding*\
<br> `birdTraits.R` \<- builds a trait dataset for bird species by combining multiple open-access datasets

### ⚙️ What the Scripts Do

Each script (`mammalTraits.R` & `birdTraits.R`) follows a consistent pipeline:

1.  **Check** for an existing `.csv` file --- if found, it loads it directly.
2.  **Load** species distribution data and intersects it with ecoregion .shp to filter our target species.
3.  **Import** relevant trait datasets (see below).
4.  **Select, merge & harmonize** trait columns (e.g., species name, clutch size, trophic level, body mass).
5.  **Export** the final trait dataset as a dated `.csv`, and prints a summary.

These are the primary sources used in the bird and mammal trait scripts. You can customize this table as needed:

| Taxa                  | Dataset Name                        | Retrieved information                                                                                                           | Link / Source                                                                             | Available? |
|---------------|---------------|---------------|---------------|---------------|
| Birds                 | BOTW_2024_2.gpkg                    | IUCN/BirdLife Species Distribution maps & taxonomic info                                                                        | [IUCN Spatial Data Download](https://www.iucnredlist.org/resources/spatial-data-download) | ⬜         |
| Birds                 | AVONET Supplementary dataset 1.xlsx | Trophic level                                                                                                                   | [Tobias et al. 2022](https://doi.org/10.1111/ele.13898)                                   | ✅         |
| Birds                 | geb13688-sup-0003-appendixs2.csv    | Abundance (indiv./km2) & carrying capacity (indiv./km2)                                                                         | [Santini et al. 2023](https://doi.org/10.1111/geb.13688)                                  | ✅         |
| Birds                 | jetz2008_avianClutch.xlsx           | Clutch size (n)                                                                                                                 | [Jetz et al. 2008](https://doi.org/10.1371/journal.pbio.0060303)                          | ✅         |
| Birds                 | Amniote_Database_Aug_2015.csv       | Body mass (grams)                                                                                                               | [Myhrvold et al. 2015](https://doi.org/10.1890/15-0846R.1)                                | ✅         |
| Birds                 | cobi13486-sup-002-tables2.xlsx      | Survival rate (%, adults), Longevity (years) and Age at first reproduction (years)                                              | [Bird et al. 2020](https://doi.org/10.1111/cobi.13486)                                    | ✅         |
| Mammals (terrestrial) | MAMMALS_TERRESTRIAL_ONLY.shp        | Terrestrial mammals distribution maps & taxonomic info                                                                          | [IUCN Spatial Data Download](https://www.iucnredlist.org/resources/spatial-data-download) | ✅         |
| Mammals (freshwater)  | MAMMALS_FRESHWATER.shp              | Freshwater mammals distribution maps & taxonomic info                                                                           | [IUCN Spatial Data Download](https://www.iucnredlist.org/resources/spatial-data-download) | ✅         |
| Mammals               | Trait_data.csv                      | Trophic level & Body mass (grams)                                                                                               | [PHYLACINE 1.2](https://megapast2future.github.io/PHYLACINE_1.2/)                         | ✅         |
| Mammals               | geb13476-sup-0002-tables1.xls       | Abundance (indiv./km2) & carrying capacity (indiv./km2)                                                                         | [Santini et al. 2022](https://onlinelibrary.wiley.com/doi/10.1111/geb.13476)              | ✅         |
| Mammals               | ADW_202404081457.xls                | Mating system                                                                                                                   | [ADW Quaardvark](https://animaldiversity.ummz.umich.edu/quaardvark/)                      | ✅         |
| Mammals               | trait_data_imputed.csv              | Maximum longevity (days), Weaning body mass (grams), Age at first reproduction (days), Litter size (n) and Litters per year (n) | [COMBINE database](https://esajournals.onlinelibrary.wiley.com/doi/10.1002/ecy.3344)      | ✅         |
| None                  | continent.shp                       | World continents shapefile                                                                                                      | [Continent Polygon](https://figshare.com/articles/dataset/Continent_Polygons/12555170)    | ✅         |
| None                  | Ecoregions2017.shp                  | World Ecoregions shapefile                                                                                                      | [Dinerstein et al. 2017](https://academic.oup.com/bioscience/article/67/6/534/3102935)    | ✅         |

> ✅ = Available in Drive folder\
> ⬜ = File too heavy (request from source & download to your own computer)

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
