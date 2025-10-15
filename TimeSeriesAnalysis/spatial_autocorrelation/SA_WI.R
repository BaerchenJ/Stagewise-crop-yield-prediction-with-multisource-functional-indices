
rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(rio)
 
WeatherData <- import("...\\Data\\wimean.xlsx")

SUBData <- cbind(WeatherData[,1:2], 'VP' = WeatherData$VP)  # CR, GDD, SR

SUBData <- SUBData[SUBData$Year == 2018, ]

SUBData$Year <- NULL

any(is.na(SUBData))

library(sf)
library(spdep)

# Load the shapefile

illinois <- st_read( "illin.shp")

# merge weather data with geometry info

library(stringr)

SUBData$County <- str_to_title(SUBData$County)

# unify the county names from shape file and the Weather data

map <- data.frame(
  old = c("Dekalb", "Dewitt", "Jo_daviess", "Lasalle", 
          "Mcdonough", "Mchenry", "Mclean", "Rock_island", "St__clair"),
  new = c("DeKalb", "De Witt", "Jo Daviess", "LaSalle",
          "McDonough", "McHenry", "McLean", "Rock Island", "St. Clair")
)

for(i in 1:nrow(map)){
  SUBData$County[SUBData$County == map$old[i]] <- map$new[i]
}

illinois <- merge(illinois, SUBData, by.x = "NAME", by.y = "County", all.x = TRUE)

illinois_clean <- illinois[!is.na(illinois$VP), ]

illinois_moran <- illinois_clean[, c("NAME", "VP", "geometry")]

# Moran's I is scale-independent

nb <- poly2nb(illinois_moran, queen = TRUE)
lw <- nb2listw(nb, style = "W")

x <- illinois_moran$VP
moran.test(x, lw)
moran.mc(x, lw, nsim = 999)

