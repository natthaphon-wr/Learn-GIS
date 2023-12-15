# Description ----
# Learn interpolation, estimate the precipitation values where data were not observed
#   I skip this tutorial because my understanding isn't enough.
# https://mgimond.github.io/Spatial/interpolation-in-r.html


# Data prep ----
library(sf)
library(tmap)

# Load precipitation data
z <- gzcon(url("https://github.com/mgimond/Spatial/raw/main/Data/precip.rds"))
P <- readRDS(z)
p <- st_as_sf(P)
# Load Texas boudary map
z <- gzcon(url("https://github.com/mgimond/Spatial/raw/main/Data/texas.rds"))
W <- readRDS(z)
w <- st_as_sf(W)

# Replace point boundary extent with that of Texas
tm_shape(w) + tm_polygons() +
  tm_shape(p) +
  tm_dots(col="Precip_in", palette = "RdBu", auto.palette.mapping = FALSE,
          title="Sampled precipitation \n(in inches)", size=0.7) +
  tm_text("Precip_in", just="left", xmod=.5, size = 0.7) +
  tm_legend(legend.outside=TRUE)


# proximity interpolation ----
library(spatstat) 

th <- dirichlet(as.ppp(p)) |> st_as_sfc() |> st_as_sf() # Create a tessellated surface
st_crs(th) <- st_crs(p)
th2 <- st_join(th, p, fn=mean)      # Join the point attributes to the polygons
th.clp   <- st_intersection(th2, w) # Clip the tessellated  surface to the Texas boundaries
tm_shape(th.clp) + 
  tm_polygons(col="Precip_in", palette="RdBu", 
              title="Predicted precipitation \n(in inches)") +
  tm_legend(legend.outside=TRUE)






