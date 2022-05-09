import pandas as pd
from pyproj import Transformer
import xlsxwriter

# Entrer le fichier
data = pd.read_excel('')
data1 = pd.DataFrame(data)

# Préparation des données
gpsD = []
gpsF = []
transformer = Transformer.from_proj("epsg:2154", "epsg:4326")

# Boucle de changement des données de Lambert à GPS
# Spécifier la colonne contenant x et y
# Spécifier gpsD ou gpsF si tronçon avec point de départ et de fin
for i in range(len(data1)):
    x = data1.iloc[i, 5]
    y = data1.iloc[i, 6]
    gpsF.append(transformer.transform(x, y))
    i += 1


# Si besoin de séparer les X et Y au lieu de tout avoir en une fois
x_coord = []
y_coord = []

x_coord, y_coord = zip(*gpsF)

x_df = pd.DataFrame(x_coord)
y_df = pd.DataFrame(y_coord)

# Export vers fichier Excel
y_df.to_excel('/Users/arthurgrimaldi/Desktop/finY.xlsx')
