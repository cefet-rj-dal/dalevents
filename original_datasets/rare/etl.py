# -*- coding: utf-8 -*-
"""
Created on Sun May 21 10:02:13 2023

@author: janio
"""

import pandas as pd
import os

caminho = "E:\\Users\\janio\\Documents\\Education\\Mestrado e Doutorado\\CEFET\\2. Pesquisa\\DAL_Events\\harbingerNimbus\\data\\RARE"
os.chdir(caminho)

file = 'RARE.csv'
data = pd.read_csv(file, sep = "|", low_memory=False)

data.describe()
data.head()

#Gravar análise em novo arquivo
output = 'rare_load.csv'
data.to_csv(output, sep = ',')


#Dataset variables
variables = list(data.columns)
rare_attributes = pd.DataFrame(data = {"variables": variables})

output = 'rare_attributes.csv'
rare_attributes.to_csv(output, sep = ',')
