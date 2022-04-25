import pandas as pd
import numpy as np

data = pd.read_csv('/Users/remibareille/Downloads/indice_ATMO.csv')
data.head()

data_rhône = data[(data['X'] > 4.62477) & (data['X'] < 5.221920) & (data['Y'] > 43.174200) & (data['Y']<45.764043)]
data_rhône

