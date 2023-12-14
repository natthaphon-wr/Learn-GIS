# Description ----
# Learn how to fundamental read and write spatial data in R
# https://mgimond.github.io/Spatial/reading-and-writing-spatial-data-in-r.html

# Download sample files ----
download.file("https://github.com/mgimond/Spatial/raw/main/Data/Income_schooling.zip", 
              destfile = "Income_schooling.zip" , mode='wb')
unzip("Income_schooling.zip", exdir = ".")
file.remove("Income_schooling.zip")

download.file("https://github.com/mgimond/Spatial/raw/main/Data/rail_inters.gpkg", 
              destfile = "./rail_inters.gpkg", mode='wb')

download.file("https://github.com/mgimond/Spatial/raw/main/Data/elev.img",  
              destfile = "./elev.img", mode='wb') 


# Spatial data format ----
# There are several ways to represent data, and they are used different packages
#   1. sf: vector rep, sf package, new standard
#   2. raster: raster rep, raster package,
#   3. SpatRaster: terra rep, terra package, replacing raster 
#   4. SpatialPoints*, SpatialPolygons*, SpatialLines*, SpatialGrid*: vector/raster rep, sp/spdep package
#   5. ppp, owin: vector rep, spatstat package, for point pattern analysis
#   6. im: raster rep, spatstat package, for point pattern analysis


# Create spatial objects ----
# The following sections demonstrate different spatial data object creation strategies.

## Reading shapefile ----
# Note that the sf object stores not only the geometry but the coordinate system information and attribute data as well
library(sf)
s.sf <- st_read("Income_schooling.shp")
head(s.sf, 5) 

## Reading GeoPackage ----
# A geopackage can store more than one layer. -> Can extract each layer 
st_layers("rail_inters.gpkg")
inter.sf <- st_read("rail_inters.gpkg", layer="Interstate")
rail.sf  <- st_read("rail_inters.gpkg", layer="Rail")

## Reading Raster ----
# terra have already replaced raster in new version.
# terra can read different raster format.
terra::gdal(drivers = TRUE) |> subset(type == "raster")
library(terra)
elev.r <- rast("elev.img")
class(elev.r)
# It's must be concerned about memory usage in raster data, can set to load entire data into memory
inMemory(elev.r)
set.values(elev.r)
inMemory(elev.r)
elev.r

## Creating a spatial object from a data frame ----
df <- data.frame(lon = c(-68.783, -69.6458, -69.7653),
                 lat = c(44.8109, 44.5521, 44.3235),
                 Name= c("Bangor", "Waterville", "Augusta"))
# Note that the crs= 4326 parameter assigns a WGS84 coordinate system to the spatial object
p.sf <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326) 
p.sf  

## Geocoding street address ----
# Convert street addresses to latitude/longitude coordinate pairs using geocoding services
# Some services require API keys, but some are not
# https://jessecambon.github.io/tidygeocoder/articles/geocoder_services.html 
library(tidygeocoder)
options(pillar.sigfig = 7) # Increase significant digits in displayed output
dat <- data.frame(
  name = c("Colby College", "Bates College", "Bowdoin College"),
  address = c("4000 Mayflower drive, Waterville, ME , 04901",
              "275 College st, Lewiston, ME 04240",
              "255 Maine St, Brunswick, ME 04011"))

geocode(.tbl = dat, address = address, method = "osm")           

## Converting from an sf object ----
# Packages such as spdep (older versions only) and spatsat do not support sf objects.
### To Spatial* object (spdep/sp) ----
# Current version of spdep will now accept sf objects, can convert point, polyline or polygon
s.sp <- as_Spatial(s.sf)
class(s.sp)

### To owin object ----
library(spatstat)
s.owin <- as.owin(s.sf)
class(s.owin)

### To ppp object ----
# You will need to project the point object to a projected coordinate system.
p.sf.utm <- st_transform(p.sf, 32619) # project from geographic to UTM
p.ppp <- as.ppp(p.sf.utm)             # Create ppp object
class(p.ppp)

## Converting a SpatRaster object to an im object ----
# You will need to first create a three column dataframe from the SpatRaster objects with the first two columns 
#   defining the X and Y coordinate values of each cell, and the third column defining the cell values
df <- as.data.frame(elev.r,xy=TRUE)
elev.im <- as.im(df)
class(elev.im)

## Converting to an sf object ----
st_as_sf(p.ppp)  # For converting a ppp object to an sf object
st_as_sf(s.sp)   # For converting a Spatial* object to an sf object

# Dissecting the sf file object ----
head(s.sf,3)
st_crs(s.sf)
# You can extract the object’s table to a dedicated data frame. -> Manupulate DF
s.df <- data.frame(s.sf)
class(s.df)
head(s.df, 5)
str(s.df)
s.nogeom.df <- st_set_geometry(s.sf, NULL) 
head(s.nogeom.df, 5)

# Exporting to different data file formats ----
# Export sf object to many format
st_write(s.sf, "shapefile_out.shp", driver = "ESRI Shapefile")  # create to a shapefile 
st_write(s.sf, "s.gpkg", driver = "GPKG")  # Create a geopackage file
# Export raster 
writeRaster(elev.r, "elev_out.tif", gdal = "GTiff" ) # Create a geoTiff file
writeRaster(elev.r, "elev_out.img", gdal = "HFA" )  # Create an Imagine raster file
