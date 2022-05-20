library(dplyr)
library(tidyverse)
library(ggplot2)
library(readxl)
install.packages("xlsx")
library(xlsx)
install.packages('Hmsic')
library(Hmisc)
install.packages('corrplot')
library(corrplot)

#Importation fichier qualité air 
data = read_excel("/Users/daviddede/Desktop/Projet/Projet-IA-Rhone/cma_filtre.xlsx")
view(data)
# Filtrage et nettoyage de la bdd 
data_filter = data %>%
  filter(nom_com == c("Valence","Lyon","Arles","Pierrelatte"))%>%
  select(nom_com,X,Y,nom_poll,valeur,year) %>%
  group_by(nom_com)
view(data_filter)

#afficher les collonnes et les différents polluants
colnames(data)
levels(factor(data$nom_poll))

#renommage des polluants
data$nom_poll[data$nom_poll == "ozone"] <- "O3"
data$nom_poll[data$nom_poll == "dioxyde d'azote"] <- "NO2"
data$nom_poll[data$nom_poll == "particules PM2,5"] <- "PM2,5"
data$nom_poll[data$nom_poll == "particules PM10"] <- "PM10"
data$nom_poll[data$nom_poll == "particules PM10"] <- "PM10"
data$nom_poll[data$nom_poll == "dioxyde de soufre"] <- "SO4"
data$nom_poll[data$nom_poll == "NOx as NO2"] <- "NO2"
data$nom_poll[data$nom_poll == "monoxyde d'azote"] <- "NO"



############################## TEST DE CORRELATION ############################

##### SECTION OK ###########
tmp <- data %>% filter(nom_com != "Saint-Bauzile") %>% 
  filter(nom_station != "Avignon Semard") %>% 
  select(nom_station, nom_com, nom_poll, valeur, unite, annee) %>%
  group_by(annee)
tmp

data %>% group_by(annee) %>%
  select(nom_station, nom_com, nom_poll, valeur, annee)
data
###########################

#IMPORTATION DU SHEET LYON
lyon <- read_excel("/Users/daviddede/Desktop/Projet-IA-Rhone/air_quality.xlsx", sheet = 'Lyon')
lyon_ <- lyon %>%
  select('NO2', 'O3', 'PM10', 'TMJA T1', 'RatioPL T1', 'Nb PL T1') 


#MATRICE DE CORRELATION
var_ly <- var(lyon_, na.rm = TRUE)
cor_ly <- cor(na.omit(lyon_))

#CORRELOGRAMME
corrplot(cor_ly, type="lower", order="hclust", tl.col="black", tl.srt=45, main= "Corrélogramme Trançon Lyon-Valence") 
  

#OBSERVATIONS
#Naturellement il y a une corrélation entre le nombre de PL et le ratio de PL
#On note une corrélation positive entre le ratio de PL et NO2 mais aussi PM10. Si le nombre de PL augmente alors PM10 et NO2 augmentent aussi


###########################################################################
#IMPORTATION DU SHEET VALENCE
val <- read_excel("/Users/daviddede/Desktop/Projet-IA-Rhone/air_quality.xlsx", sheet = 'Valence')
val_ <- val %>%
  drop_na(NO2, O3, PM10, 'PM2,5', 'RatioPL T2')


#MATRICE DE CORRELATION VALENCE
var <- var(val_, na.rm = TRUE)
cor_val <- cor(val_)
cor_val_ <- cor(na.omit(val))

#CORRELOGRAMME
corrplot(cor_val_, type="lower", order="hclust", tl.col="black", tl.srt=45, main= "Corrélogramme Trançon Valence-Avignon")

#OBSERVATIONS
#Quand le nombre de poid lourd augmente on note une augmentation du ratio de PL
#On note une corélation positive entre le ratio de PL et O3 c'est-à-dire que que l'augmentation de l'un entraine l'augmention de l'autre
#Or il y a aussi une coorélation positive entre Le O3, le PM 2,5, le NO2 et le PM10 ce qui sous entend que l'augmentation de l'un serait un facteur de l'augmentation de la quantité des autres polluants
#En conclusion On peut établir un lien entre l'augmentation du nombre de PL et la pollution sur ce trançon

###########################################################################
#IMPORTATION DU SHEET AVIGNON
avi <- read_excel("/Users/daviddede/Desktop/Projet-IA-Rhone/air_quality.xlsx", sheet = 'Avignon')
avi_ <- avi %>%
  #select('NO2', 'O3', 'PM10', 'PM2,5', 'RatioPL T2') %>%
  drop_na(NO2, O3, PM10, 'PM2,5', 'RatioPL T3')


#MATRICE DE CORRELATION AVIGNON
var_avi <- var(avi_, na.rm = TRUE)
cor_avi <- cor(avi_)
cor_avi_ <- cor(na.omit(avi))

#CORRELOGRAMME
corrplot(cor_avi_, type="lower", order="hclust", tl.col="black", tl.srt=45, main= "Corrélogramme Trançon Avignon-Montélimar")

#OBSERVATIONS
#Ici on remarque une corrélation positive entre le nombre le nombre de PL le O3. Il n'y a rien d'autre à dire 
#Les autres polluants semblent toujours être liés entre eux 
#Le plot montre une corrélation négative entre le nombre de PL et les différents polluants. Juste à titre informatif


