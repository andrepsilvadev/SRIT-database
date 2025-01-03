## General Settings ##
## Andre P. Silva ##
## July 9th, 2024 ##

# spatial projections --------------------------------------------------
# UTM North-Sweden minimizes local distortion
# Tried this before. Generates the following:
# "unequal horizontal and vertical resolutions. Such data cannot be stored in arc-ascii format"
utm34 <-  "+proj=utm +zone=34 +datum=WGS84 +units=m +no_defs"

# Lambert Azimuthal Equal Area - preserves the relative sizes of areas
# throughout the projection. This property makes it unsuitable for
# large regions
crs.laea <- "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

# wgs84
wgs84 <- "+proj=longlat +datum=WGS84 +no_defs"