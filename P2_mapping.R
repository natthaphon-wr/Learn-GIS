# Description ----
# Learn how to map spatial data in different ways for suitable
# https://mgimond.github.io/Spatial/mapping-data-in-r.html 

# Download sample files ----
library(sf)
library(terra)
z <- gzcon(url("https://github.com/mgimond/Spatial/raw/main/Data/elev.RDS"))
elev.r <- unwrap(readRDS(z))
z <- gzcon(url("https://github.com/mgimond/Spatial/raw/main/Data/inter_sf.RDS"))
inter.sf <- readRDS(z)
z <- gzcon(url("https://github.com/mgimond/Spatial/raw/main/Data/rail_sf.RDS"))
rail.sf <- readRDS(z)
z <- gzcon(url("https://github.com/mgimond/Spatial/raw/main/Data/s_sf.RDS"))
s.sf <- readRDS(z)
z <- gzcon(url("https://github.com/mgimond/Spatial/raw/main/Data/p_sf.RDS"))
p.sf <- readRDS(z)
# Data consist of 5 layers
# 1. Elevation raster (elev.r)
# 2. Interstate polyline layer (inter.sf)
# 3. Railroad polyline layer (rail.sf) 
# 4. Maine counties polygon layer (s.sf).
# 5. Point cities layer (p.sf)

# tmap ----
# The tmap package is specifically developed for mapping spatial data.
# The package recognizes sf, raster and Spatial* objects.
library(tmap) 
tm_shape(s.sf) + tm_polygons(col="grey", border.col="white")
tm_shape(s.sf) + tm_polygons(col="Income", border.col = "white")
tm_shape(s.sf) + tm_polygons("Income",  border.col = "white") + 
  tm_legend(outside = TRUE)
tm_shape(s.sf) + 
  tm_polygons("Income",  border.col = "white", legend.show=FALSE) +
  tm_layout(frame = FALSE)
tm_shape(s.sf) + 
  tm_polygons("Income", border.col = NULL) + 
  tm_legend(outside = TRUE)

## Combining layers ----
# Stack layers by piecing together additional tm_shape functions
tm_shape(s.sf) + 
  tm_polygons("Income", border.col = NULL) + 
  tm_legend(outside = TRUE) +
  tm_shape(rail.sf) + tm_lines(col="grey70") +
  tm_shape(p.sf) + tm_dots(size=0.3, col="black") #point on top of rail

## Tweaking classification schemes ----
# Modify mapping via tm_polygons function
tm_shape(s.sf) + 
  tm_polygons("Income", style = "quantile", n = 6, palette = "Greens") + 
  tm_legend(outside = TRUE)
tm_shape(s.sf) + 
  tm_polygons("Income", style = "fixed", palette = "Greens",
              breaks = c(0, 23000, 27000, 100000)) + 
  tm_legend(outside = TRUE)
tm_shape(s.sf) + 
  tm_polygons("Income", style = "fixed",palette = "Greens",
              breaks = c(0, 23000, 27000, 100000 ),
              labels = c("under $23,000", "$23,000 to $27,000", "above $27,000"),
              text.size = 1) + 
  tm_legend(outside = TRUE)

## Tweaking colors ----
# tmap provide many color palettes: sequential, divergent, categorical schemes
tm_shape(s.sf) + 
  tm_polygons("NAME", palette = "Pastel1") + 
  tm_legend(outside = TRUE)
tm_shape(s.sf) + 
  tm_polygons("NoSchool", style="quantile", palette = "YlOrBr", n=8, 
              title="Fraction without \na HS degree") + 
  tm_legend(outside = TRUE)
#can inverse palette with - such as palette = "-YlOrBr"

## Adding labels ----
# You can add text and labels using the tm_text function.
tm_shape(s.sf) + 
  tm_polygons("NAME", palette = "Pastel1", border.col = "white") + 
  tm_legend(outside = TRUE) +
  tm_shape(p.sf) +   
  tm_dots(size=  .3, col = "red") +
  tm_text("Name", just = "left", xmod = 0.5, size = 0.8)

## Adding a grid or graticule ----
# You can add a grid or graticule to the map using the tm_grid function.
# You will need to modify the map’s default viewport setting via the tm_layout function to provide space for the grid labels.
# UTM coordinate
tm_shape(s.sf) + 
  tm_polygons("NAME", palette = "Pastel1") + 
  tm_legend(outside = TRUE) +
  tm_layout(outer.margins = c(.1,.1,.1,.1)) +
  tm_grid(labels.inside.frame = FALSE,
          n.x = 4, n.y = 10)
# Latitude/Longtitude coordinate by specific projection parameter
tm_shape(s.sf) + 
  tm_polygons("NAME", palette = "Pastel1") + 
  tm_legend(outside = TRUE) +
  tm_layout(outer.margins = c(.1,.1,.1,.1)) +
  tm_grid(labels.inside.frame = FALSE, 
          x = c(-70.5, -69, -67.5),
          y = c(44, 45, 46, 47),
          projection = "EPSG:4326")
tm_shape(s.sf) + 
  tm_polygons("NAME", palette = "Pastel1") + 
  tm_legend(outside = TRUE) +
  tm_layout(outer.margins = c(.1,.1,.1,.1)) +
  tm_grid(labels.inside.frame = FALSE, 
          x =  c(-70.5, -69, -67.5) ,
          y = c(44, 45, 46, 47),
          projection = "+proj=longlat",
          labels.format = list(fun=function(x) {paste0(x,intToUtf8(176))} ) )

## Adding hist plots ----
tm_shape(s.sf) + 
  tm_polygons("NoSchool", palette = "YlOrBr", n = 6, 
              legend.hist = TRUE, title = "% no school") + 
  tm_legend(outside = TRUE, hist.width = 2) 

## Mapping raster files ----
# Raster objects can be mapped by specifying the tm_raster function
tm_shape(elev.r) + 
  tm_raster(style = "cont", title = "Elevation (m)", palette = terrain.colors(64))+
  tm_legend(outside = TRUE)
tm_shape(elev.r) + 
  tm_raster(style = "fixed", title = "Elevation (m)",
            breaks = c(0, 50, 100, 500, 750, 1000, 15000),
            palette = terrain.colors(5))+
  tm_legend(outside = TRUE)
tm_shape(elev.r) + 
  tm_raster(style = "quantile", n = 15, title = "Elevation (m)",
            palette = colorRampPalette( c("darkolivegreen4","yellow", "brown"))(15),
            legend.hist = TRUE)+
  tm_legend(outside = TRUE, hist.width = 2)

## Changing coordinate systems ----
# tmap can change the output’s coordinate system without needing to reproject the data layers. 
# Define the Albers coordinate system
aea <-  "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +ellps=GRS80 +datum=NAD83"
tm_shape(elev.r, projection = aea) + 
  tm_raster(style = "quantile", n = 12,
            palette = colorRampPalette( c("darkolivegreen4","yellow", "brown"))(12),
            legend.show = FALSE) +
  tm_shape(rail.sf) + tm_lines(col = "grey70")+
  tm_shape(p.sf) +tm_dots(size=0.5) +
  tm_layout(outer.margins = c(.1,.1,.1,.1)) +
  tm_grid(labels.inside.frame = FALSE, 
          x = c(-70.5, -69, -67.5),
          y = c(44, 45, 46, 47),
          projection = "+proj=longlat")

## Side-by-side maps ----
inc.map <- tm_shape(s.sf) + tm_polygons(col="Income")+
  tm_legend(outside=TRUE) 
school.map <- tm_shape(s.sf) + tm_polygons(col="NoSchool")+
  tm_legend(outside=TRUE) 
name.map <- tm_shape(s.sf) + tm_polygons(col="NAME")+
  tm_legend(outside=TRUE) 
tmap_arrange(inc.map, school.map, name.map)

## Splitting data by polygons or group of polygons ----
tm_shape(s.sf) + tm_polygons(col = "Income") +
  tm_legend(outside = TRUE) +
  tm_facets( by = "NAME", nrow = 2)


# ggplot2 ----
# The key geom used when mapping spatial data is geom_sf()
# By default, ggplot will add a graticule to the plot, 
#   even if the coordinate system associated with the layer is in a projected coordinate system.
library(ggplot2)
ggplot(data = s.sf) + geom_sf()
ggplot() + geom_sf(data = s.sf) #default mapping into longitude/latitude
ggplot(data = s.sf) + geom_sf() + theme_void() #remove grid coordinate
ggplot(data = s.sf) + geom_sf() + coord_sf(datum = NULL) #adopt the layer’s native coordinate system from data
ggplot(data = s.sf) + geom_sf() + 
  scale_x_continuous(breaks = c(-70, -69, -68)) +
  scale_y_continuous(breaks = 44:47)
ggplot(data = s.sf) + geom_sf() + 
  coord_sf(datum = NULL) +
  scale_x_continuous(breaks = c(400000, 500000, 600000)) +
  scale_y_continuous(breaks = c(4900000, 5100000))
ggplot(data = s.sf, aes(fill = Income)) + geom_sf() 
ggplot() + geom_sf(data = s.sf, aes(fill = Income)) 
ggplot(data = s.sf, aes(fill = Income)) + 
  geom_sf(col = "white") 

## Tweaking classification schemes ----
ggplot(data = s.sf, aes(fill = Income)) + geom_sf() +
  scale_fill_stepsn(colors = c("#D73027", "#FC8D59", "#FEE08B", 
                               "#D9EF8B", "#91CF60") ,
                    breaks = c(22000, 25000, 27000, 30000))
ggplot(data = s.sf, aes(fill = Income)) + geom_sf() +
  scale_fill_fermenter(type = "div", palette = "PRGn", n.breaks = 4) #can be provide type classification
ggplot(data = s.sf, aes(fill = Income)) + geom_sf() +
  scale_fill_fermenter(type = "div", palette = "PRGn", n.breaks = 4, direction = 1) #flip color direction
ggplot(data = s.sf, aes(fill = Income)) + geom_sf() +
  scale_fill_stepsn(colors = c("#D73027", "#FC8D59", "#FEE08B", 
                               "#D9EF8B", "#91CF60", "#1A9850") ,
                    breaks = c(22000, 25000, 26000, 27000, 30000),
                    values = scales::rescale(c(22000, 25000, 26000, 27000, 30000), c(0,1)),
                    guide = guide_coloursteps(even.steps = FALSE,
                                              show.limits = TRUE,
                                              title = "Per capita Income \n(US dollars)",
                                              barheight = unit(2.2, "in"),
                                              barwidth = unit(0.15, "in"))) 

## Combining layers ----
ggplot() + 
  geom_sf(data = s.sf, aes(fill = Income)) +
  geom_sf(data = rail.sf, col = "white") +
  geom_sf(data = p.sf, col = "green")
ggplot() + 
  geom_tile(data = as.data.frame(elev.r, xy=TRUE, na.rm = TRUE), 
              aes(x = x, y = y, fill = elev)) +
  scale_fill_gradientn(colours = terrain.colors(7)) +
  geom_sf(data = rail.sf, col = "white") +
  geom_sf(data = p.sf, col = "black") +
  theme(axis.title = element_blank())  # Removes axes labels


# plot_sf ----
# This is a convenient way to generate simple plots without additional packages.
# It is limited in its customization options.
# Note that plot_sf requires that the layers be in the same coordinate system.
plot(s.sf)
plot(s.sf["Income"])
plot(st_geometry(s.sf))
plot(st_geometry(s.sf), col ="grey", border = "white")
plot(st_geometry(s.sf), col ="grey", border = "white", graticule = TRUE, axes= TRUE)

## Combine layers ----
plot(st_geometry(s.sf), col ="grey", border = "white", graticule = TRUE, axes= TRUE)
plot(rail.sf,  col = "grey20", add = TRUE)
plot(elev.r, col = terrain.colors(30))
plot(st_geometry(rail.sf), col ="grey", border = "white", add = TRUE)

## Tweaking colors ----
OP <- par(las = 1, omi=c(0,0,0,0.6))
p1 <- plot(s.sf["Income"], breaks = c(20000, 22000, 25000, 26000, 27000, 30000, 33000),
           pal = c("#D73027", "#FC8D59", "#FEE08B", 
                   "#D9EF8B", "#91CF60", "#1A9850"),
           key.width = 0.2,
           at = c(20000, 22000, 25000, 26000, 27000, 30000, 33000))
par(OP)
