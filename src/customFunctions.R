## Custom functions ##
## Andre P. Silva ##
## June 5th, 2024 ##

## landscape functions --------------------------------------------------------

grep.original.landscapes <- function(
  species) {
  species.no.space <- gsub(" ", "", species)

  list.original.landscapes <- list.files(
    path = "data/",
    pattern = paste0(species.no.space),
    full.names = TRUE
    )

  grep.original <- grep(
    pattern = "originalFile",
    x = list.original.landscapes,
    value = TRUE
    )

    raster.stack <- terra::rast(grep.original)
    names(raster.stack) <- tools::file_path_sans_ext(basename(grep.original))

    return(raster.stack)
}

grep.calibration.landscapes <- function(species) {
  
  species.no.space <- gsub(" ", "", species)

  list.calibration.landscapes <- list.files(
    path = paste0("output/", runname, "/Inputs"),
    pattern = paste0(species.no.space),
    full.names = TRUE)

  grep.calibration <- grep(
    pattern = "calibrationCells",
    x = list.calibration.landscapes,
    value = TRUE
    )

    raster.stack <- terra::rast(grep.calibration)
    names(raster.stack) <- tools::file_path_sans_ext(basename(grep.calibration))

    return(raster.stack)
}


aggregate_to_species_specific_modelling_resolution <- function(
  species, spData, original.resolution) {
  # purpose: landscape preparation per species
  # looks for the original suitability rasters in the project data folder
  # original files need to be named as originalFile
  # modelling resolution is in km so original resolution also needs to be in km
  
  species.no.space <- gsub(" ", "", species)

  list.original.landscapes <- list.files(
    path = "data/",
    pattern = paste0(species.no.space),
    full.names = TRUE
    )

  grep.original <- grep(
    pattern = "originalFile",
    x = list.original.landscapes,
    value = TRUE
    )

  modelling.resolution <- spData %>%
    dplyr::filter(Species %in% species) %>%
    dplyr::select(ModellingRes) %>%
    as.numeric()

  aggregation.prop <- modelling.resolution/original.resolution
  aggregation.factor <- ifelse(
    aggregation.prop<1,1,aggregation.prop)

  raster.stack <- terra::rast(grep.original)
  
  names(raster.stack) <- tools::file_path_sans_ext(
    basename(grep.original))

  aggregated.raster <- terra::aggregate(
    x = raster.stack,
    fact = aggregation.factor,
    fun = mean
    )

  names(aggregated.raster) <- gsub(
    "originalFile",
    "aggregated",
    names(aggregated.raster)
    )

  terra::writeRaster(
      x = aggregated.raster,
      filename = paste0("data/", names(aggregated.raster), ".asc"),
      filetype = "AAIGrid",
      overwrite = FALSE
      )
    return(aggregated.raster)
  }

project_spatial_input <- function(
  species, spData, projectedCRS) {
  # projects rasters for rangeshifter at the needed resolution
  # uses modelling resolution from spData to calculate projected resolution in meters
  # reads the aggregated files from function - aggregate_to_species_specific_modelling_resolution
  # sets NA values and decimal cases as needed for rangeshifter
  # saves projected raster in the working directory
  # you still need to remove the decimal cases in the asc file to avoid
  # problems in the input file for spatial visualization you might need
  # to backtransform but values will again differ slightly
  # input raster needs to have the correct name for output
  
  species.no.space <- gsub(" ", "", species)

  proj.resolution <- spData %>%
    dplyr::filter(Species %in% species) %>%
    dplyr::select(ModellingRes)*1000 %>% #conversion to meters
    as.numeric()

  list.aggregated.landscapes <- list.files(
    path = paste0(getwd(), "/data"),
    pattern = paste0(species.no.space),
    full.names = TRUE,
    )

  grep.aggregated <- grep(
    pattern = "aggregated.asc",
    x = list.aggregated.landscapes,
    value = TRUE
    )

  grep.aggregated <- grep.aggregated[!grepl(
      pattern = "aux.xml",
      grep.aggregated
      )]
    
  aggregated.stack <- raster::stack(grep.aggregated)
  
  # project raster
  # projecting slightly changes the raster values,
  # I think because of the resampling function
  
  # with raster
  r.ext <- raster::projectExtent(
    aggregated.stack,
    projectedCRS
    )

  r.proj <- raster::projectRaster(
    aggregated.stack,
    res = as.integer(proj.resolution),
    crs = r.ext,
    method = "bilinear"
  )

  # change NA values according to RS and round values
  #raster::NAvalue(r.proj) <- -9999
  raster::NAvalue(r.proj) <- -9999
  r.proj <- round(r.proj, 0)

  raster::writeRaster(
    x = r.proj,
    filename = paste0("output/", runname, "/Inputs/", names(r.proj), "_proj.asc"),
    bylayer = TRUE,
    overwrite = FALSE
    )
    return(r.proj)
}

calibration_Cells <- function(
  species, cellId, calibrationValues, runname) { #cellId, calibrationValues
  # creates a new raster with three cells from the projected landscape in the
  # input within the run folder
  # attributes the values from the original raster to the selected cells
  
  species.no.space <- gsub(" ", "", species)

  list.projected.landscapes <- list.files(
    path = paste0("output/", runname, "/Inputs"),
    pattern = species.no.space,
    full.names = TRUE
  )

  grep.aggregated <- grep(
    pattern = "aggregated_proj.asc",
    x = list.projected.landscapes,
    value = TRUE
    )

  projected.stack <- raster::stack(grep.aggregated)

for(i in 1:raster::nlayers(projected.stack)){
  
  calibrationCells <- raster::rasterFromCells(
    projected.stack[[1]],
    cellId,
    values = FALSE)

  raster::values(calibrationCells) <- cbind(
    raster::getValues(projected.stack[[1]])[cellId[1]],
    raster::getValues(projected.stack[[1]])[cellId[2]],
    raster::getValues(projected.stack[[1]])[cellId[3]]
  )
  
  raster::values(calibrationCells) <- calibrationValues
  
  raster::writeRaster(
    calibrationCells,
    filename = paste0("output/", runname, "/Inputs/", names(projected.stack[[i]]), "_calibrationCells.asc"),
    fsep = "",
    overwrite = FALSE)
}

list.calibration.landscapes <- list.files(
    path = paste0("output/", runname, "/Inputs/"),
    pattern = paste0(species.no.space),
    full.names = TRUE
    )

  grep.calibration <- grep(
    pattern = "calibrationCells",
    x = list.calibration.landscapes,
    value = TRUE
    )

    raster.stack <- terra::rast(grep.calibration)
    names(raster.stack) <- tools::file_path_sans_ext(
      basename(grep.calibration))

return(raster.stack)

}

