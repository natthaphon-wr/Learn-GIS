# Description ----
# Learn raster operation
# https://mgimond.github.io/Spatial/raster-operations-in-r.html 

# Load sample files ----
library(terra)
library(sf)

z <- gzcon(url("https://github.com/mgimond/Spatial/raw/main/Data/elev_world.RDS"))
elev <- unwrap(readRDS(z))
z <- gzcon(url("https://github.com/mgimond/Spatial/raw/main/Data/bath_world.RDS"))
bath <- unwrap(readRDS(z))
z <- gzcon(url("https://github.com/mgimond/Spatial/raw/main/Data/continent_global.RDS"))
cont <- readRDS(z)


# Local operations and functions ----
## Unary operations and functions  ----
bath2 <- bath * (-1)
bath3 <- bath2 < 0
library(tmap)
tm_shape(bath3) + tm_raster(palette = "Greys") + 
  tm_legend(outside = TRUE, text.size = .8) 

# re-classify with dept threshold
m <- c(0, 100, 100,  100, 500, 500,  500, 1000,  1000, 1000, 11000, 11000)
m <- matrix(m, ncol=3, byrow = T)
m # 0-100 -> 500, 100-500 -> 500
bath3 <- classify(bath, m, right = T)
tm_shape(bath3) + tm_raster(style="cat") + tm_legend(outside = TRUE, text.size = .8) 

bath3[bath3 == 100] <- NA # assign NA values to cells that are equal to 100
tm_shape(bath3) + tm_raster(showNA=TRUE, colorNA="grey") + 
  tm_legend(outside = TRUE, text.size = .8) 

## Binary operations and functions ----
elevation <- elev - bath
tm_shape(elevation) + tm_raster(palette="-RdBu") + 
  tm_legend(outside = TRUE, text.size = .8) 


# Focal operations and functions ----
# Applied focally to rasters involve user defined neighboring cells
# It's like filter technique in imaage processing
f1 <- focal(elevation, w = 5, fun = mean)
tm_shape(f1) + tm_raster(palette="-RdBu") + 
  tm_legend(outside = TRUE, text.size = 0.8) 

m  <- matrix(c(1,1,1,1,0,1,1,1,1)/8, nrow = 3) 
f2 <- focal(elevation, w=m, fun=sum)
tm_shape(f2) + tm_raster(palette="Greys") + 
  tm_legend(legend.show = FALSE) 

Sobel <- matrix(c(-1,0,1,-2,0,2,-1,0,1) / 4, nrow=3) 
f3    <- focal(elevation, w=Sobel, fun=sum) 
tm_shape(f3) + tm_raster(palette="Greys") + 
  tm_legend(legend.show = FALSE) 


# Zonal operations and functions ----
z1 <- aggregate(elevation, fact=2, fun=mean, expand=TRUE)
tm_shape(z1) + tm_raster(palette="-RdBu",n=6) + 
  tm_legend(outside = TRUE, text.size = .8) 
res(elevation)
res(z1)
cont.elev <- extract(elevation, cont, fun=mean, bind = TRUE) 
cont.elev.sf <- st_as_sf(cont.elev)
tm_shape(cont.elev.sf) + tm_polygons(col="band1") + 
  tm_legend(outside = TRUE, text.size = .8)


# Global operations and functions ----
# Use of all input cells of a grid in the computation of an output cell value.
r1   <- rast(ncols=100, nrows=100, xmin=0, xmax=100, ymin=0, ymax=100)
r1[] <- NA                # Assign NoData values to all pixels
r1[c(850, 5650)] <- 1     # Change the pixels #850 and #5650  to 1
crs(r1) <- "+proj=ortho"  # Assign an arbitrary coordinate system (needed for mapping with tmap)
tm_shape(r1) + tm_raster(palette="red") + 
  tm_legend(outside = TRUE, text.size = .8) 

r1.d <- distance(r1)      #euclidean distance
tm_shape(r1.d) + tm_raster(palette = "Greens", style="order", title="Distance") + 
  tm_legend(outside = TRUE, text.size = .8) +
  tm_shape(r1) + tm_raster(palette="red", title="Points") 


# Computing cumulative distances ----
# Generate a cumulative distance raster.
# One objective will be to demonstrate the influence “adjacency cells” wields in the final results.
library(gdistance)
r   <- rast(nrows=100,ncols=100,xmin=0,ymin=0,xmax=100,ymax=100)
r[] <- rep(1, ncell(r))
h4   <- transition(raster(r), transitionFunction = function(x){1}, directions = 4) # 4 adjacency
h8   <- transition(raster(r), transitionFunction = function(x){1}, directions = 8) # 8 adjacency
h16  <- transition(raster(r), transitionFunction = function(x){1}, 16, symm=FALSE)     # 16 adjacency with asymmetric
hb   <- transition(raster(r), transitionFunction=function(x){1},"bishop",symm=FALSE) # 4 diagonal adjacency

# Corrects for distance distortions associated with data in a geographic coordinate system. 
h4    <- geoCorrection(h4,  scl=FALSE)
h8    <- geoCorrection(h8,  scl=FALSE)
h16   <- geoCorrection(h16, scl=FALSE)
hb    <- geoCorrection(hb,  scl=FALSE)

# map the cumulative distance
A       <- c(50,50) # Location of source cell
h4.acc  <- accCost(h4,A)
h8.acc  <- accCost(h8,A)
h16.acc <- accCost(h16,A)
hb.acc  <- accCost(hb,A) 

