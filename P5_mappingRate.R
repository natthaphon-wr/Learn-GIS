# Description ----
# Learn more advance of mapping with using sf's plot instead of tmap package.
# https://mgimond.github.io/Spatial/mapping-rates-in-r.html 

# Initialize ----
library(spdep)
library(classInt)
library(RColorBrewer)
library(sf)
library(sp)

pal1 <- brewer.pal(6,"Greys")
pal2 <- brewer.pal(8,"RdYlGn")
pal3 <- c(brewer.pal(9,"Greys"), "#FF0000")

auckland <- st_read(system.file("shapes/auckland.shp", package="spData")[1])
brks1 <- classIntervals(auckland$M77_85, n = 6, style = "equal")
brks2 <- classIntervals(auckland$M77_85, n = 6, style = "quantile")
plot(auckland["M77_85"], breaks = brks1$brks, pal = pal1, at = round(brks1$brks,2),
     main = "Equal interval breaks", key.pos = 4, las = 1)
plot(auckland["M77_85"], breaks = brks2$brks, pal = pal1, at = brks2$brks,
     main = "Quantile breaks", key.pos = 4, las = 1)


# Raw Rates ----
pop <- auckland$Und5_81 * 9 # whole pop = amount of pop under 5 for a year (1981) * 9 
mor <- auckland$M77_85      # mortality count for 9 years
auckland$raw.rate <- mor / pop * 1000   # infant deaths per 1000 individuals per year
brks1 <- classIntervals(auckland$raw.rate, n = 6, style = "equal")
brks2 <- classIntervals(auckland$raw.rate, n = 6, style = "quantile")
plot(auckland["raw.rate"], breaks = brks1$brks, pal = pal1, at = round(brks1$brks,2),
     main = "Equal interval breaks", key.pos = 4, las = 1)
plot(auckland["raw.rate"], breaks = brks2$brks, pal = pal1, at = round(brks2$brks,2),
     main = "Quantile breaks", key.pos = 4, las = 1)


# Standardized mortality ratios (relative risk) ----
# Popular form of representation in the field of epidemiology
# Map the ratios of the number of deaths to an expected death count. 
auck.rate <- sum(mor) / sum(pop)
mor.exp   <- pop * auck.rate  # Expected count over a nine year period
auckland$rel.rate <- 100 * mor / mor.exp
brks <- classIntervals(auckland$rel.rate, n = 6, style = "fixed", 
                       fixedBreaks = c(0,47, 83, 118, 154, 190, 704))
plot(auckland["rel.rate"], breaks =  brks$brks, at = brks$brks, pal = pal1,
     key.pos = 4, las = 1)


# Dykes and Unwin’s chi-square statistic ----
auckland$chi.squ = (mor - mor.exp) / sqrt(mor.exp)
brks <- classIntervals(auckland$chi.squ, n = 6, style = "fixed", 
                       fixedBreaks = c(-5,-3, -1, -2, 0, 1, 2, 3, 5))
plot(auckland["chi.squ"], breaks = brks$brks, at = brks$brks, pal=rev(pal2),
     key.pos = 4, las = 1)


# Unstable ratios ----
brks <- classIntervals(auckland$Und5_81, n = 6, style = "equal")
plot(auckland["Und5_81"], breaks = brks$brks,  at = brks$brks, pal = pal1,
     key.pos = 4, las = 1)

## Global Empirical Bayes (EB) rate estimate ----
EB.est         <- EBest(auckland$M77_85, auckland$Und5_81 * 9 )
auckland$EBest <- EB.est$estmm * 1000
brks1          <- classIntervals(auckland$EBest, n = 10, style = "quantile")
brks2          <- classIntervals(auckland$raw.rate, n = 10, style = "quantile")
plot(auckland["EBest"], breaks = brks1$brks, at = round(brks1$brks, 2), pal = pal3, 
     main="EB rates", key.pos = 4, las = 1) 
plot(auckland["raw.rate"], breaks = brks2$brks, at = round(brks2$brks, 2), pal = pal3,
     main="Raw Rates", key.pos = 4, las = 1) 

## Local Empirical Bayes (EB) rate estimate ----
nb      <- poly2nb(auckland) 
EBL.est <- EBlocal(auckland$M77_85, 9*auckland$Und5_81, nb)
auckland$EBLest <- EBL.est$est * 1000
brks1           <- classIntervals(auckland$EBLest, n = 10, style = "quantile")
brks2           <- classIntervals(auckland$raw.rate, n = 10, style = "quantile")
plot(auckland["EBLest"], breaks =  brks1$brks, at = round(brks1$brks,2), pal = pal3, 
     main = "Local EB rates", key.pos = 4, las = 1)
plot(auckland["raw.rate"], breaks =  brks2$brks, at = round(brks2$brks,2), pal = pal3, 
     main = "Raw Rates", key.pos = 4, las = 1)

