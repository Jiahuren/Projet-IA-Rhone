#Traitement des données du trafic routier moyen journalier annuel

library(readxl)
library(dplyr)


TMJA_2007 <- read_excel("C:/Users/William/OneDrive/Bureau/Projet IA/Base de données trafic routier/TMJA_2007.xlsx")
View(TMJA_2007)
TMJA_2008 <- read_excel("C:/Users/William/OneDrive/Bureau/Projet IA/Base de données trafic routier/TMJA_2008.xlsx")
View(TMJA_2008)
TMJA_2009 <- read_excel("C:/Users/William/OneDrive/Bureau/Projet IA/Base de données trafic routier/TMJA_2009.xlsx")
View(TMJA_2009)
TMJA_2010 <- read_excel("C:/Users/William/OneDrive/Bureau/Projet IA/Base de données trafic routier/TMJA_2010.xlsx")
View(TMJA_2010)
TMJA_2011 <- read_excel("C:/Users/William/OneDrive/Bureau/Projet IA/Base de données trafic routier/TMJA_2011.xlsx")
View(TMJA_2011)
TMJA_2012 <- read_excel("C:/Users/William/OneDrive/Bureau/Projet IA/Base de données trafic routier/TMJA_2012.xlsx")
View(TMJA_2012)
TMJA_2013 <- read_excel("C:/Users/William/OneDrive/Bureau/Projet IA/Base de données trafic routier/TMJA_2013.xlsx")
View(TMJA_2013)
TMJA_2014 <- read_excel("C:/Users/William/OneDrive/Bureau/Projet IA/Base de données trafic routier/TMJA_2014.xlsx")
View(TMJA_2014)
TMJA_2015 <- read_excel("C:/Users/William/OneDrive/Bureau/Projet IA/Base de données trafic routier/TMJA_2015.xlsx")
View(TMJA_2015)
TMJA_2016 <- read_excel("C:/Users/William/OneDrive/Bureau/Projet IA/Base de données trafic routier/TMJA_2016.xlsx")
View(TMJA_2016)
TMJA_2017 <- read_excel("C:/Users/William/OneDrive/Bureau/Projet IA/Base de données trafic routier/TMJA_2017.xlsx")
View(TMJA_2017)
TMJA_2018 <- read_excel("C:/Users/William/OneDrive/Bureau/Projet IA/Base de données trafic routier/TMJA_2018.xlsx")
View(TMJA_2018)
TMJA_2019 <- read_excel("C:/Users/William/OneDrive/Bureau/Projet IA/Base de données trafic routier/TMJA_2019.xlsx")
View(TMJA_2019)

str(TMJA_2013)

TMJA_2007 <- TMJA_2007 %>% select(dateReferentiel, route, longueur, xD, yD, xF, yF, anneeMesureTrafic, TMJA, RatioPL)
TMJA_2008 <- TMJA_2008 %>% select(dateReferentiel, route, longueur, xD, yD, xF, yF, anneeMesureTrafic, TMJA, RatioPL)
TMJA_2009 <- TMJA_2009 %>% select(dateReferentiel, route, longueur, xD, yD, xF, yF, anneeMesureTrafic, TMJA, RatioPL)
TMJA_2010 <- TMJA_2010 %>% select(dateReferentiel, route, longueur, xD, yD, xF, yF, anneeMesureTrafic, TMJA, RatioPL)
TMJA_2011 <- TMJA_2011 %>% select(dateReferentiel, route, longueur, xD, yD, xF, yF, anneeMesureTrafic, TMJA, RatioPL)
TMJA_2012 <- TMJA_2012 %>% select(dateReferentiel, route, longueur, xD, yD, xF, yF, anneeMesureTrafic, TMJA, RatioPL)
TMJA_2013 <- TMJA_2013 %>% select(dateReferentiel, route, longueur, xD, yD, xF, yF, anneeMesureTrafic, TMJA, RatioPL)
TMJA_2014 <- TMJA_2014 %>% select(dateReferentiel, route, longueur, xD, yD, xF, yF, anneeMesureTrafic, TMJA, RatioPL)
TMJA_2015 <- TMJA_2015 %>% select(dateReferentiel, route, longueur, xD, yD, xF, yF, anneeMesureTrafic, TMJA, RatioPL)
TMJA_2016 <- TMJA_2016 %>% select(dateReferentiel, route, longueur, xD, yD, xF, yF, anneeMesureTrafic, TMJA, RatioPL)
TMJA_2017 <- TMJA_2017 %>% select(dateReferentiel, route, longueur, xD, yD, xF, yF, anneeMesureTrafic, TMJA, RatioPL)
TMJA_2018 <- TMJA_2018 %>% select(dateReferentiel, route, longueur, xD, yD, xF, yF, anneeMesureTrafic, TMJA, RatioPL)
TMJA_2019 <- TMJA_2019 %>% select(dateReferentiel, route, longueur, xD, yD, xF, yF, anneeMesureTrafic, TMJA, RatioPL)


#Il manque la base de données de 2015 car il manque la colonne communeID et 2019
TMJA <- rbind(TMJA_2007, TMJA_2008, TMJA_2009, TMJA_2010, TMJA_2011, TMJA_2012,
              TMJA_2013, TMJA_2014, TMJA_2015, TMJA_2016, TMJA_2017, TMJA_2018, 
              TMJA_2019)

View(TMJA)

TMJA_A7 <- TMJA %>%
  filter(route == "A0007")

View(TMJA_A7)

TMJA_A7_2007 <- filter( TMJA_A7, anneeMesureTrafic == "2007")
TMJA_A7_2008 <- filter( TMJA_A7, anneeMesureTrafic == "2008")
TMJA_A7_2009 <- filter( TMJA_A7, anneeMesureTrafic == "2009")
TMJA_A7_2010 <- filter( TMJA_A7, anneeMesureTrafic == "2010")
TMJA_A7_2011 <- filter( TMJA_A7, anneeMesureTrafic == "2011")
TMJA_A7_2012 <- filter( TMJA_A7, anneeMesureTrafic == "2012")
TMJA_A7_2013 <- filter( TMJA_A7, anneeMesureTrafic == "2013")
TMJA_A7_2014 <- filter( TMJA_A7, anneeMesureTrafic == "2014")
TMJA_A7_2015 <- filter( TMJA_A7, anneeMesureTrafic == "2015")
TMJA_A7_2016 <- filter( TMJA_A7, anneeMesureTrafic == "2016")
TMJA_A7_2017 <- filter( TMJA_A7, anneeMesureTrafic == "2017")
TMJA_A7_2018 <- filter( TMJA_A7, anneeMesureTrafic == "2018")
TMJA_A7_2019 <- filter( TMJA_A7, anneeMesureTrafic == "2019")

View(TMJA_A7_2007)
View(TMJA_A7_2008)
View(TMJA_A7_2009)
View(TMJA_A7_2010)
View(TMJA_A7_2011)
View(TMJA_A7_2012)
View(TMJA_A7_2013)
View(TMJA_A7_2014)
View(TMJA_A7_2015)
View(TMJA_A7_2016)
View(TMJA_A7_2017)
View(TMJA_A7_2018)
View(TMJA_A7_2019)


sum(TMJA_A7_2007$longueur)
sum(TMJA_A7_2008$longueur)
sum(TMJA_A7_2009$longueur)
sum(TMJA_A7_2010$longueur)
sum(TMJA_A7_2011$longueur)
sum(TMJA_A7_2012$longueur)
sum(TMJA_A7_2013$longueur)
sum(TMJA_A7_2014$longueur)
sum(TMJA_A7_2015$longueur)
sum(TMJA_A7_2016$longueur)
sum(TMJA_A7_2017$longueur)
sum(TMJA_A7_2018$longueur)
sum(TMJA_A7_2019$longueur)






#traitement de TMJA_A7_2007


TMJA_A7_2007$cumulD 
TMJA_A7_2007$cumulF
TMJA_A7_2007$Moyenne_cumul_D_et_F 
TMJA_A7_2007$Estima_PL




