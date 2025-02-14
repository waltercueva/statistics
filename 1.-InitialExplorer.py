import pandas as pd
file_path = 'datos.csv'  
data = pd.read_csv(file_path, encoding='latin1', delimiter=';')

# 1. Revisar valores faltantes
p601_missing = data['P601'].isnull().sum()
estratosocio_missing = data['ESTRATOSOCIO'].isnull().sum()

# 2. Resumen estadístico de P601
p601_summary = data['P601'].describe()

# 3. Distribución de ESTRATOSOCIO
estratosocio_freq = data['ESTRATOSOCIO'].value_counts()

# Mostrar resultados
print("Valores faltantes en P601:", p601_missing)
print("Valores faltantes en ESTRATOSOCIO:", estratosocio_missing)
print("\nResumen estadístico de P601:")
print(p601_summary)
print("\nDistribución de ESTRATOSOCIO:")
print(estratosocio_freq)
