#Réunir les différentes bases de données pour chaque Région
library(data.table)

#pays de la loire
library(readxl)
C6H6_pays_de_la_loire <- read_excel("C:/Users/William/OneDrive/Bureau/Projet IA/Data Pays de la Loire/C6H6 pays de la loire.xlsx")
CO_pays_de_la_loire <- read_excel("C:/Users/William/OneDrive/Bureau/Projet IA/Data Pays de la Loire/CO pays de la loire.xlsx")
NO_pays_de_la_loire <- read_excel("C:/Users/William/OneDrive/Bureau/Projet IA/Data Pays de la Loire/NO pays de la loire.xlsx")
NO2_pays_de_la_loire <- read_excel("C:/Users/William/OneDrive/Bureau/Projet IA/Data Pays de la Loire/NO2 pays de la loire.xlsx")
NOx_pays_de_la_loire <- read_excel("C:/Users/William/OneDrive/Bureau/Projet IA/Data Pays de la Loire/NOx pays de la loire.xlsx")
O3_pays_de_la_loire <- read_excel("C:/Users/William/OneDrive/Bureau/Projet IA/Data Pays de la Loire/O3 pays de la loire.xlsx")
PM2.5_pays_de_la_loire <- read_excel("C:/Users/William/OneDrive/Bureau/Projet IA/Data Pays de la Loire/PM2.5 pays de la loire.xlsx")
PM10_pays_de_la_loire <- read_excel("C:/Users/William/OneDrive/Bureau/Projet IA/Data Pays de la Loire/PM10 pays de la loire.xlsx")
SO2_pays_de_la_loire <- read_excel("C:/Users/William/OneDrive/Bureau/Projet IA/Data Pays de la Loire/SO2 pays de la loire.xlsx")



SO2_pays_de_la_loire <- rbind(SO2_pays_de_la_loire, C6H6_pays_de_la_loire)
SO2_pays_de_la_loire <- rbind(SO2_pays_de_la_loire, CO_pays_de_la_loire)
SO2_pays_de_la_loire <- rbind(SO2_pays_de_la_loire, NO2_pays_de_la_loire)
SO2_pays_de_la_loire <- rbind(SO2_pays_de_la_loire, NOx_pays_de_la_loire)
SO2_pays_de_la_loire <- rbind(SO2_pays_de_la_loire, O3_pays_de_la_loire)
SO2_pays_de_la_loire <- rbind(SO2_pays_de_la_loire, PM2.5_pays_de_la_loire)
SO2_pays_de_la_loire <- rbind(SO2_pays_de_la_loire, PM10_pays_de_la_loire)
SO2_pays_de_la_loire <- rbind(SO2_pays_de_la_loire, NO_pays_de_la_loire)




View(SO2_pays_de_la_loire)

write.csv2(SO2_pays_de_la_loire,file= 'polluants pays de la loire.csv')



#auvergne rhone alpes
benzene<- read_excel("C:/Users/William/OneDrive/Bureau/Projet IA/Data Auvergne Rhone Alpes/benzène pollution.xlsx")
co2<- read_excel("C:/Users/William/OneDrive/Bureau/Projet IA/Data Auvergne Rhone Alpes/co2 sur un an par jour.xlsx")
dioxyde_dazote<- read_excel("C:/Users/William/OneDrive/Bureau/Projet IA/Data Auvergne Rhone Alpes/dioxyde d'azote pollution.xlsx")
dioxyde_de_soufre<- read_excel("C:/Users/William/OneDrive/Bureau/Projet IA/Data Auvergne Rhone Alpes/dioxyde de soufre pollution.xlsx")
monoxyde_dazote<- read_excel("C:/Users/William/OneDrive/Bureau/Projet IA/Data Auvergne Rhone Alpes/monoxyde d'azote pollution.xlsx")
oxyde_dazote<- read_excel("C:/Users/William/OneDrive/Bureau/Projet IA/Data Auvergne Rhone Alpes/oxydes d'azote pollution.xlsx")
ozone<- read_excel("C:/Users/William/OneDrive/Bureau/Projet IA/Data Auvergne Rhone Alpes/ozone pollution.xlsx")
pm25<- read_excel("C:/Users/William/OneDrive/Bureau/Projet IA/Data Auvergne Rhone Alpes/pm 2.5 pollution.xlsx")
pm10<- read_excel("C:/Users/William/OneDrive/Bureau/Projet IA/Data Auvergne Rhone Alpes/pm 10 pollution.xlsx")


benzene <- rbind(benzene, co2)
benzene <- rbind(benzene, dioxyde_dazote)
benzene <- rbind(benzene, dioxyde_de_soufre)
benzene <- rbind(benzene, monoxyde_dazote)
benzene <- rbind(benzene, oxyde_dazote)
benzene <- rbind(benzene, ozone)
benzene <- rbind(benzene, pm25)
benzene <- rbind(benzene, pm10)

View(benzene)

write.csv2(benzene,file= 'polluants auvergne rhone alpes.csv')


#nouvelle aquitaine
a <- read_excel("C:/Users/William/OneDrive/Bureau/Projet IA/Data Nouvelle Aquitaine/atmona_mesures_01_2021-04-10_2022-04-09.xlsx")
b <- read_excel("C:/Users/William/OneDrive/Bureau/Projet IA/Data Nouvelle Aquitaine/atmona_mesures_03_2021-04-10_2022-04-09.xlsx")
c <- read_excel("C:/Users/William/OneDrive/Bureau/Projet IA/Data Nouvelle Aquitaine/atmona_mesures_04_2021-04-10_2022-04-09.xlsx")
d <- read_excel("C:/Users/William/OneDrive/Bureau/Projet IA/Data Nouvelle Aquitaine/atmona_mesures_08_2021-04-10_2022-04-09.xlsx")
e <- read_excel("C:/Users/William/OneDrive/Bureau/Projet IA/Data Nouvelle Aquitaine/atmona_mesures_12_2021-04-10_2022-04-09.xlsx")
f <- read_excel("C:/Users/William/OneDrive/Bureau/Projet IA/Data Nouvelle Aquitaine/atmona_mesures_24_2021-04-10_2022-04-09.xlsx")
g <- read_excel("C:/Users/William/OneDrive/Bureau/Projet IA/Data Nouvelle Aquitaine/atmona_mesures_39_2021-04-10_2022-04-09.xlsx")


a <- rbind(a, b)
a <- rbind(a, c)
a <- rbind(a, d)
a <- rbind(a, e)
a <- rbind(a, f)
a <- rbind(a, g)

View(a)

write.csv2(a,file= 'polluants nouvelle aquitaine.csv')


#corse
aa <- read_excel("C:/Users/William/OneDrive/Bureau/Projet IA/Data Corse/mes_corse_journalier_poll_princ.xlsx")
bb <- read_excel("C:/Users/William/OneDrive/Bureau/Projet IA/Data Corse/mes_corse_journalier_poll_second.xlsx")


aa <- rbind(aa, bb)


View(aa)

write.csv2(aa, file='polluants corse.csv')


#scraping récupération des coordonnées des villes de provence cote d'azur
polluants_provence_cote_azur <- read_excel("C:/Users/William/OneDrive/Bureau/Projet IA/Data Provence Alpes Cote d Azur/polluants provence cote azur.xlsx")
View(polluants_provence_cote_azur)
polluants_provence_cote_azur$date<- as.Date(polluants_provence_cote_azur$date_debut, format = "%Y/%m/%d" )
polluants_provence_cote_azur <- polluants_provence_cote_azur %>%
  select(nom_com, nom_station, nom_poll, valeur, unite, date)
polluants_provence_cote_azur$nom_com <- factor(polluants_provence_cote_azur$nom_com)
nom_com_pca <- levels(polluants_provence_cote_azur$nom_com)
nom_com_pca
polluants_provence_cote_azur$nom_station <- factor(polluants_provence_cote_azur$nom_station)
nom_station_pca <- levels(polluants_provence_cote_azur$nom_station)
nom_station_pca
length(nom_station_pca)

station_pca <- cbind(nom_station_pca)
attach(station_pca)



station_pca <- for (i in seq(from=1, to=length(nom_station_pca))){
  station_pca$lien[i] <-  paste("https://fr.wikipedia.org/wiki/",station_pca$nom_station_pca[i])
}

View(station_pca)
