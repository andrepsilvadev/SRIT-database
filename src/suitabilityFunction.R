## Example ##
## Andre P. Silva & Ines Silva & Afonso Barrocal ##
## Last major update: December 30th, 2024 ##
## Description: Based on srit_1.2.1_28Aug.R

# creating function to import land use data converting ncdf to raster
ncdf2rast <- function(i,nc){
  
  # load packages
  require(ncdf4) # package for netcdf manipulation
  require(raster) 
  require(terra) 
  #require(rgdal) # package for geospatial analysis
  
  # open NetCDF file
  nc_data <- nc_open(paste0("data/",nc,".nc"))
  
  # Save the print(nc) dump to a text file
  {
    sink(paste0(nc,".txt"))
    print(nc_data)
    sink()
  }
  
  # retrieving the longitude
  lon <- ncvar_get(nc_data, "longitude")
  lon[lon > 180] <- lon[lon > 180] - 360
  
  # retrieving the latitude
  lat <- ncvar_get(nc_data, "latitude", verbose = F)
  
  # retrieving variables
  var <- attributes(nc_data$var)
  var <- var$names
  
  # create array for variable nº i
  array <- ncvar_get(nc_data, var[i]) # store the data in a 3-dimensional array
  
  # retrieve fill value of variable nº i
  fillvalue <- ncatt_get(nc_data, var[i], "_FillValue")
  
  # change fill value to NA
  array[array == fillvalue$value] <- NA
  
  # turn array into raster object
  r <- raster::raster(array, xmn=min(lon), 
              xmx=max(lon),
              ymn=min(lat),
              ymx=max(lat),
              crs=CRS(SRS_string="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  
  # free unused memory
  invisible(gc())
  
  # creat SpatRaster object
  r <- terra::rast(r)
  
  # change name of layers to name of variables
  names(r) <- var[i]
  
  # close NetCDF file
  nc_close(nc_data)

  return(r)
}

# function to calculate suitability values based on occurrence points and a given base raster file
getLUvalues2 <- function(vector, rasterFile) {
  
  # The getLUvalues2 calculates the suitability of a species
  # It uses as input a vector (dataframe) with the occurrences of ONE species (must include species name, longitude and latitude variables)
  # It also uses as input a raster file (either a NetCDF, .tif or other raster files formats)
  
  # import the required package
  require("terra", quietly = T)
  
  # process dataset
  points <- subset(vector, select = c("decimalLongitude","decimalLatitude")) # make sure to check the names of files names for latitude and longitude
  
  # convert to a data frame
  points <- as.data.frame(points)
  
  # extract values from the raster at the points
  valuesatpoints <- terra::extract(rasterFile, points)
  
  # calculate each portion of the final expression
  sum1 <- colSums(valuesatpoints[,-1], na.rm = TRUE)
  max1 <- max(sum1, na.rm = TRUE)
  
  # calculate suitability
  suitability <- as.data.frame(t(sum1/max1))
  
  # add sps names
  species <- c(`species` = unique(vector$species))
  
  # bind everything
  suitability <- cbind(species, suitability)
  
  #return the suitability values dataframe
  return(suitability)
}
