# Description ----
# Learn Autocorrelation, topic that I interest now.
# https://mgimond.github.io/Spatial/spatial-autocorrelation-in-r.html


# Data preparation ----
z <- gzcon(url("https://github.com/mgimond/Spatial/raw/main/Data/s_sf.RDS"))
s1 <- readRDS(z)

library(tmap)
tm_shape(s1) + tm_polygons(style="quantile", col = "Income") +
  tm_legend(outside = TRUE, text.size = .8) 


# Define neighborhood ----
library(spdep)
nb <- poly2nb(s1, queen=TRUE) # list all neighboring polygons (contiguous neighbor)
nb[[1]] # neighbor of 1st polygons, 1st polygon near 2nd, 3rd, 4th, 5th polygons
s1$NAME[1]
s1$NAME[c(2,3,4,5)]

lw <- nb2listw(nb, style="W", zero.policy=TRUE) #add equal weight to neighborhoods
lw$weights[2]

Inc.lag <- lag.listw(lw, s1$Income) #compute lag (average neighbor) income 
Inc.lag


# Moran's I Statistics: Manual way ----
library(stats)
M <- lm(Inc.lag ~ s1$Income)                    # Regression model
plot(Inc.lag ~ s1$Income, pch=20, asp=1, las=1)  # Lag income vs own income
abline(M, col='red')
coef(M)[2] # Slope coefficient

## Simulated ----
n <- 599L                 # No. of simulations
I.r <- vector(length=n)   # Create an empty vector

for (i in 1:n){
  x <- sample(s1$Income, replace=FALSE) # Randomly shuffle income values
  x.lag <- lag.listw(lw, x)   # Compute new set of lagged values
  M.r    <- lm(x.lag ~ x)     # Compute the regression slope and store its value
  I.r[i] <- coef(M.r)[2]
}

hist(I.r, main=NULL, xlab="Moran's I", las=1)
abline(v=coef(M)[2], col="red") # Add slope coefficient that we already have

## Computing a pseudo p-value from an MC simulation ----
# Find the no. of simulated Moran’s I values values greater than our observed Moran’s I value.
N.greater <- sum(coef(M)[2] > I.r)

# Pseudo p-value
p <- min(N.greater + 1, n + 1 - N.greater) / (n + 1)
p
# p = 0.025 -> there is 2.5% to be wrong to reject null hypothesis
#   (being wrong in stating that the income values are not clustered at the county level)


# Moran's I Statistics: spdep package ----
# This method aren't MC simulation, but it's analytically approach. 
#     -> Test significant in w/ MC simulation
moran.test(s1$Income,lw)

MC<- moran.mc(s1$Income, lw, nsim=599)
MC
plot(MC, main="", las=1)


# Moran's I Statistics on distance band ----
# Use distance based instead of contiguous neighborhood
coo <- st_centroid(s1)                # extract center of each polygon
S.dist <- dnearneigh(coo, 0, 50000)   # distance in 0 - 50 km (50000 m)
lw <- nb2listw(S.dist, style="W",zero.policy=T)         #define neighborhood
MI  <-  moran.mc(s1$Income, lw, nsim=599,zero.policy=T) #run MC simulation
plot(MI, main="", las=1) 
MI
