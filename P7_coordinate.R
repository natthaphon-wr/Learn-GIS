# Description ----
# Learn coordinate systems, projection
# https://mgimond.github.io/Spatial/coordinate-systems-in-r.html 

# Initialize ----
library(terra)
library(sf)   

z <- gzcon(url("https://github.com/mgimond/Spatial/raw/main/Data/elev.RDS"))
elev.r <- unwrap(readRDS(z))
z <- gzcon(url("https://github.com/mgimond/Spatial/raw/main/Data/s_sf.RDS"))
s.sf <- readRDS(z)

sf_extSoftVersion()[1:3] #check package version


# Checking coordinate system ----
st_crs(s.sf)
st_crs(elev.r)


# Proj4 coordinate syntax ----
# https://proj4.org/en/9.3/usage/projections.html 


# Assign coordinate system ----
# define coordinate system using Proj4 syntax
s.sf <- st_set_crs(s.sf, "+proj=utm +zone=19 +ellps=GRS80 +datum=NAD83") 
st_crs(s.sf)

s.sf <- st_set_crs(s.sf, 26919)
st_crs(s.sf)

crs(elev.r) <- "+proj=utm +zone=19 +ellps=GRS80 +datum=NAD83"
st_crs(elev.r) #not correct with unknown type

crs(elev.r) <- "+init=EPSG:26919"
st_crs(s.sf)


