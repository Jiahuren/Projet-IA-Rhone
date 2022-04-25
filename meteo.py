import requests
from bs4 import BeautifulSoup
import numpy as np
import time
import pandas as pd
# Création d'une liste d'année de 2010 à 2019
annees = np.arange(2010,2020,1).tolist()
# Mettre par pair les villes avec le code du site 
villes = {"/lyon-bron" : "07480" , "/valence" : "000K3" , "/montelimar-ancone" : "07577" , "/avignon" : "07563"}
# Liste des villes
Villes_list = ['Lyon','Valence','Montelimar','Avignon']


temp = []
for ville in villes :
  for annee in range(2010,2020,1):
    # Récupérer les urls par ville par année 
    url_ville = "https://www.infoclimat.fr/climatologie/annee/"+str(annee)+ ville +"/valeurs/" + villes[ville] +".html"
    reponse = requests.get(url_ville)
    soup = BeautifulSoup(reponse.text)
    temperature = soup.findAll('td',{'style' : 'background-color:#045563;color:white;font-family:sans-serif;font-weight:bold;border-bottom:1px solid rgba(255,255,255,0.4);font-size:13px'})[2].find('span').text
    temp.append(temperature)
    time.sleep(2)
    print(temp) 

# Fonction pour diviser une liste en plusieurs 
def split(liste, nb):
  k, m = divmod(len(liste), nb)
  return list((liste[i*k+min(i, m):(i+1)*k+min(i+1, m)] for i in range(nb)))

# Split temp en 4 listes 
tempF = split(temp,4)

# Listes en Dataframe avec les années en nom de colonne 
df = pd.DataFrame(tempF,columns = annees)

# Nom des villes en index
df = df.set_axis(Villes_list)

# Exportation du df en csv
df.to_csv('/Users/remibareille/Desktop/temp.csv',sep="\t")




    