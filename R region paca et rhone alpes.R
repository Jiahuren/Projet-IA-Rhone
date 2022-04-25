

library(sp)
library(rgdal)
library(readxl)
library(dplyr)


polluants_provence_cote_azur <- read_excel("C:/Users/William/OneDrive/Bureau/Projet IA/Data Provence Alpes Cote d Azur/polluants provence cote azur.xlsx")
polluants_auvergne_rhone_alpes <- read_excel("C:/Users/William/OneDrive/Bureau/Projet IA/Data Auvergne Rhone ALpes/polluants auvergne rhone alpes.xlsx")




View(polluants_auvergne_rhone_alpes)
View(polluants_provence_cote_azur)



polluants_auvergne_rhone_alpes$date<- as.Date(polluants_auvergne_rhone_alpes$date_debut, format = "%Y/%m/%d" )
polluants_provence_cote_azur$date<- as.Date(polluants_provence_cote_azur$date_debut, format = "%Y/%m/%d" )
polluants_provence_cote_azur$x_l93 <- as.numeric(polluants_provence_cote_azur$x_l93)
polluants_provence_cote_azur$y_l93 <- as.numeric(polluants_provence_cote_azur$y_l93)

str(polluants_provence_cote_azur)


xy <- cbind(c(509535.7, 514535.7),c(201098.6, 201098.6)) 
xy <- cbind(polluants_provence_cote_azur$x_l93, polluants_provence_cote_azur$y_l93)



crs <- CRS("+proj=lcc +lat_1=30 +lat_2=60 +lat_0=38 +lon_0=126 +datum=WGS84")
p <- SpatialPoints(xy, proj4string=crs)
g <- spTransform(p, CRS("+proj=longlat +datum=WGS84"))
coordinates(g)


station_zone <- polluants_auvergne_rhone_alpes %>% 
  filter(x_wgs84 > 4.62477 & x_wgs84 < 5.052735 & y_wgs84 > 43.174200 & y_wgs84 < 45.764043)

station_zone$nom_station <- factor(station_zone$nom_station)
levels(station_zone$nom_station)


