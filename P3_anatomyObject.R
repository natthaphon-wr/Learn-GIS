# Description ----
# Learn basic anatomy of objects data
# https://mgimond.github.io/Spatial/anatomy-of-simple-feature-objects.html#extracting-geometry-from-an-sf-object 


# Point sf objects ----
## Step 1: Create the point geometry ----
library(sf)
p1.sfg <- st_point(c(-70, 45)) 
p2.sfg <- st_point(c(-69, 44)) 
p3.sfg <- st_point(c(-69, 45)) 
class(p1.sfg)

## Step 2: Create a column of simple feature geometries ----
p.sfc <- st_sfc( list(p1.sfg, p2.sfg, p3.sfg), crs = 4326 )
class(p.sfc)
p.sfc
p.sfc[[1]]

## Step 3: Create the simple feature object ----
# Combine the point geometries into a single object.
p.sf <- st_sf(p.sfc)
p.sf
attributes(p.sf)
names(p.sf) <- "coords"       #rename column 
st_geometry(p.sf) <- "coords" #also re-define geometry column
p.sf
typeof(p.sf$coords) 

p.sf$val1 <- c("A", "B", "C") #add attributes to sf object
p.sf
plot(p.sf, pch = 16, axes = TRUE, main = NULL)

df <- data.frame(col1 = c("A", "B","C")) #add geometry column to existing non-spatial dataframe
st_geometry(df) <- p.sfc
df


# Polyline sf Objects ----
l <- rbind( c(-70, 45), c(-69,44), c(-69,45))   #line
l.sfg <- st_linestring(l)                       #polyline
l.sfc <- st_sfc(list(l.sfg), crs = 4326)        #create the simple feature column
l.sf <- st_sf(l.sfc)                            #create simple object
l.sf
plot(l.sf, type = "b", pch = 16, main = NULL, axes = TRUE)

## Create branches polylines ----
l1 <- rbind( c(-70, 45), c(-69, 44), c(-69, 45) )
l2 <- rbind( c(-69, 44), c(-70, 44) )
l3 <- rbind( c(-69, 44), c(-68, 43) )
l.sfg <- st_multilinestring(list(l1, l2, l3))   #linestring -> mutilinestring
l.sfc <- st_sfc(list(l.sfg), crs = 4326)
l.sf <- st_sf(l.sfc)
plot(l.sf, type = "b", pch = 16, axes = TRUE)


# Polygon sf object ----
# Closed polylines -> polygon 
# 2 Types of polygon: POLYGON, MULTIPOLYGON

## POLYGON ----
poly1.crd <- rbind( c(-66, 43), c(-70, 47), c(-70,43),  c(-66, 43) )
poly1.geom <- st_polygon( list(poly1.crd ) )
poly1.geom  #polyhon geometry
poly.sfc <- st_sfc( list(poly1.geom), crs=4326)
poly.sfc    #simple feature column
poly.sf <- st_sf(poly.sfc)
poly.sf     #sf object
names(poly.sf) <- "coords"
st_geometry(poly.sf) <- "coords"
poly.sf
plot(poly.sf, col = "bisque", axes = TRUE)

## POLYGON with a hole ----
poly1.outer.crd <- rbind( c(-66,43), c(-70,47), c(-70,43), c(-66,43))  # Outer ring
poly1.inner.crd  <- rbind( c(-68,44), c(-69,44), c(-69,45), c(-68,44)) # Inner ring
poly1.geom <- st_polygon( list(poly1.outer.crd, poly1.inner.crd))      # polygon geometry
poly.sfc <- st_sfc( list(poly1.geom), crs = 4326 )                     # simple feature column
poly.sf <- st_sf(poly.sfc)                                             # sf object
names(poly.sf) <- "coords"
st_geometry(poly.sf) <- "coords"
poly.sf
plot(poly.sf, col = "bisque", axes = TRUE)

## Combine polygons ----
poly2.crd <- rbind( c(-67, 45),c(-67, 47), c(-69,47), c(-67, 45) ) 
poly2.geom <- st_polygon( list(poly2.crd))
poly.sfc <- st_sfc( list(poly1.geom , poly2.geom), crs = 4326 )
poly.sf <- st_sf(poly.sfc)
names(poly.sf) <- "coords"
st_geometry(poly.sf) <- "coords"
poly.sf
plot(poly.sf, col = "bisque", axes = TRUE)

## Adding attributes ----
poly.sf$id <- c("A", "B")
poly.sf
plot(poly.sf["id"],  axes = TRUE, main = NULL)

## MULTIPOLYGON ----
mpoly1.sfg  <- st_multipolygon( list(
  list( poly1.outer.crd,  # Outer loop
        poly1.inner.crd), # Inner loop
  list( poly2.crd)) )     # Separate polygon
mpoly.sfc <- st_sfc( list(mpoly1.sfg), crs = 4326)
mpoly.sf <- st_sf(mpoly.sfc)
mpoly.sf
plot(mpoly.sf, col = "bisque", axes = TRUE)

## Mixing singlepart and multipart elements ----
poly3.coords <- rbind( c(-66, 44), c(-64, 44), c(-66,47), c(-66, 44) )
poly4.coords <- rbind( c(-67, 43), c(-64, 46), c(-66.5,46), c(-67, 43) ) 
mpoly1.sfg  <- st_multipolygon( list(
  list( poly1.outer.crd,  # Outer loop
        poly1.inner.crd), # Inner loop
  list( poly2.crd)) )     # Separate poly
mpoly2.sfg  <- st_multipolygon( list(
  list(poly3.coords)))    # Unique polygon
mpoly3.sfg  <- st_multipolygon( list(
  list(poly4.coords)) )   # Unique polygon
mpoly.sfc <- st_sfc( list(mpoly1.sfg, mpoly2.sfg, mpoly3.sfg), crs = 4326)
mpoly.sf <- st_sf(mpoly.sfc)
mpoly.sf$ids <- c("A", "B", "C")
plot(mpoly.sf["ids"], axes = TRUE, main = NULL,
     pal = sf.colors(alpha = 0.5, categorical = TRUE))


# Extract geometry from sf object ----
st_geometry(mpoly.sf)
st_geometry(mpoly.sf)[[1]]
st_geometry(mpoly.sf)[[1]][]
