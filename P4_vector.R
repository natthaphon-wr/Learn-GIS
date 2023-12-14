# Description ----
# Learn vector representation/operations 
# https://mgimond.github.io/Spatial/vector-operations-in-r.html

# Load data ----
library(sf)
library(ggplot2)

z <- gzcon(url("https://github.com/mgimond/Spatial/raw/main/Data/Income_schooling_sf.rds"))
s1.sf <- readRDS(z) #delineates Maine counties (USA)
z <- gzcon(url("https://github.com/mgimond/Spatial/raw/main/Data/Dist_sf.rds"))
s2.sf <- readRDS(z) #delineates distances to Augusta (Maine) as concentric circles
z <- gzcon(url("https://github.com/mgimond/Spatial/raw/main/Data/Highway_sf.rds"))
l1.sf <- readRDS(z) #interstate highway system that runs through Maine.
ggplot() + 
  geom_sf(data = s1.sf) +
  geom_sf(data = s2.sf, alpha = 0.5, col = "red") +
  geom_sf(data = l1.sf, col = "blue")

# Dissolving geometries ----
## Dissolving by contiguous shape ----

# Option 1: use st_union
#   Dissolving process removed all attributes from the original spatial object.
#   t_union returns an sfc object even though the input object is sf
ME <- st_union(s1.sf, by_feature = FALSE)
ggplot(ME) + geom_sf(fill = "grey")

# Option 2: use dplyr package
#   This option will also remove any attributes associated with the input spatial object, 
#   however, the output remains an sf object
library(dplyr)
ME <- s1.sf %>% 
  group_by() %>% 
  summarise()
ggplot(ME) + geom_sf(fill = "grey")

## Dissolving by attribute ----
s1.sf$med <- s1.sf$Income > median(s1.sf$Income) #True if its income > median income
ggplot(s1.sf) + geom_sf(aes(fill = med))

# Option 1: use aggregate
ME.inc <- aggregate(s1.sf["med"], by = list(diss = s1.sf$med), FUN = function(x)x[1], do_union = TRUE)
st_drop_geometry(ME.inc) # Print the layer's attributes table

# Option 2: use group by
ME.inc <- s1.sf %>% 
  group_by(med) %>% 
  summarise() 
st_drop_geometry(ME.inc) 
ggplot(ME.inc) + geom_sf(aes(fill = med))
ME.inc <- s1.sf %>%  
  group_by(med) %>%   
  summarize(medinc = median(Income)) 
ggplot(ME.inc) + geom_sf(aes(fill = medinc))
st_drop_geometry(ME.inc)


# Subsetting by attribute ----
# Select/Filter attribute using dpylr same as df data
ME.ken <- s1.sf[s1.sf$NAME == "Kennebec",]
ME.ken <- s1.sf %>% 
  filter(NAME == "Kennebec")
ggplot(ME.ken) + geom_sf()

ME.inc2 <- s1.sf %>% 
  filter(Income < median(Income))
ggplot(ME.inc2) + geom_sf()


# Intersecting layers ----
clp1 <- st_intersection(s1.sf, s2.sf)
ggplot(clp1) + geom_sf()
st_drop_geometry(clp1) # new polygons are created


# Clipping spatial objects using other spatial objects ----
clp2 <- st_intersection(s2.sf, st_union(s1.sf)) 
ggplot(clp2) + geom_sf()
clp2 <- st_intersection(s1.sf, st_union(s2.sf)) 
ggplot(clp2) + geom_sf()

clp3 <- st_intersection(l1.sf, st_union(s2.sf))
ggplot(clp3) + 
  geom_sf(data = clp3) +
  geom_sf(data = st_union(s2.sf), col = "red", fill = NA )


# Union layers ----
# Union operation can generate many overlapping geometries
un1  <- st_union(s2.sf,s1.sf)
ggplot(un1) + geom_sf(aes(fill = NAME), alpha = 0.4)


# Buffering geometries ----
l1.sf.buf <- st_buffer(l1.sf, dist = 10000) # generates 10,000 m buffer around the polyline
ggplot(l1.sf.buf) + geom_sf() + coord_sf(ndiscr = 1000) 
l1.sf.buf.dis <- l1.sf.buf %>% 
  group_by()  %>% 
  summarise()
ggplot(l1.sf.buf.dis) + geom_sf() 

l1.sf.buf.dis <- l1.sf.buf %>% 
  group_by(Number)  %>% 
  summarise()
ggplot(l1.sf.buf.dis, aes(fill=Number) ) + geom_sf(alpha = 0.5)
