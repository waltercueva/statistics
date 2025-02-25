# Análisis Comparativo del Número de Libros Impresos en Hogares Peruanos según Estrato Socioeconómico

### Análisis estadístico detallado utilizando datos de la ENL 2022, abordando todas las fases exploratorias, pruebas estadísticas y visualizaciones.

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from scipy.stats import shapiro, kruskal, mannwhitneyu, probplot

# Cargar datos
file_path = 'datos.csv'  
data = pd.read_csv(file_path, encoding='latin1', delimiter=';')

# Exploración inicial
display(data.info())
display(data.describe())

# Manejo de valores faltantes
p601_median = data['P601'].median()
data['P601'].fillna(p601_median, inplace=True)

# Detección y eliminación de outliers
Q1, Q3 = data['P601'].quantile([0.25, 0.75])
IQR = Q3 - Q1
lower_bound, upper_bound = Q1 - 1.5 * IQR, Q3 + 1.5 * IQR
clean_data = data[(data['P601'] >= lower_bound) & (data['P601'] <= upper_bound)]

# Prueba de normalidad
stat, p = shapiro(clean_data['P601'])
print("Prueba de Shapiro-Wilk:", stat, p)

# Prueba de Kruskal-Wallis
kruskal_test = kruskal(*[group['P601'] for _, group in clean_data.groupby('ESTRATOSOCIO')])
print("Prueba de Kruskal-Wallis:", kruskal_test)

# Análisis post-hoc (Mann-Whitney)
from itertools import combinations
for g1, g2 in combinations(clean_data['ESTRATOSOCIO'].unique(), 2):
    data1 = clean_data[clean_data['ESTRATOSOCIO'] == g1]['P601']
    data2 = clean_data[clean_data['ESTRATOSOCIO'] == g2]['P601']
    stat, p = mannwhitneyu(data1, data2)
    print(f"Comparación {g1} vs {g2}: U={stat}, p={p}")

# Visualizaciones
sns.histplot(clean_data['P601'], kde=True)
plt.title('Histograma de P601')
plt.show()

sns.boxplot(x='ESTRATOSOCIO', y='P601', data=clean_data)
plt.title('Boxplot por Estrato')
plt.show()

probplot(clean_data['P601'], dist="norm", plot=plt)
plt.title('Q–Q Plot')
plt.show()

# Estadísticos descriptivos
tabla_desc = clean_data.groupby('ESTRATOSOCIO')['P601'].describe()
display(tabla_desc)

# Cálculo de η²
N = len(clean_data)
H = kruskal_test.statistic
eta_squared = (H - (len(clean_data['ESTRATOSOCIO'].unique()) - 1)) / (N - 1)
print("Magnitud del efecto (η²):", eta_squared)
