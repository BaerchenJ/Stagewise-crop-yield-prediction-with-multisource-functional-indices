
rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(sf)
library(spdep)

# Load the shapefile

illinois <- st_read( "illin.shp")

# merge yield data with geometry info

library(rio)

Yielddf <- import("...\\Data\\Yield.xlsx")

library(stringr)

Yielddf$County <- str_to_title(Yielddf$County)


# unify the county names from shape file and the Yield data

map <- data.frame(
  old = c("Dekalb", "Dewitt", "Jo_daviess", "Lasalle", 
          "Mcdonough", "Mchenry", "Mclean", "Rock_island", "St__clair"),
  new = c("DeKalb", "De Witt", "Jo Daviess", "LaSalle",
          "McDonough", "McHenry", "McLean", "Rock Island", "St. Clair")
)

for(i in 1:nrow(map)){
  Yielddf$County[Yielddf$County == map$old[i]] <- map$new[i]
}


# get the mean of 43 years

library(dplyr)

Yielddf_mean <- Yielddf %>%
  group_by(County) %>%
  summarise(Ystar = mean(Ystar, na.rm = TRUE)) %>%
  ungroup()

illinois <- merge(illinois, Yielddf_mean, by.x = "NAME", by.y = "County", all.x = TRUE)

illinois_clean <- illinois[!is.na(illinois$Ystar), ]

illinois_moran <- illinois_clean[, c("NAME", "Ystar", "geometry")]

# Moran's I is scale-independent

nb <- poly2nb(illinois_moran, queen = TRUE)
lw <- nb2listw(nb, style = "W")

x <- illinois_moran$Ystar
moran.test(x, lw)
moran.mc(x, lw, nsim = 999)

