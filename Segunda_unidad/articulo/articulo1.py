import pandas as pd
import numpy as np
import cvxpy as cp

# Carga tu dataset
df = pd.read_excel(r"c:\Users\HP\Downloads\202503-PDE-REP-00115.xlsx", sheet_name="PDE-REP-00115")
df = pd.read_excel(r"c:\Users\HP\Downloads\202503-PDE-REP-00115.xlsx", sheet_name="PDE-REP-00115")
print(df.columns)


# Tomamos solo 3 aeropuertos y 3 meses para ejemplo
sample_df = df[df["AEROPUERTO"].isin(df["AEROPUERTO"].unique()[:3])]
sample_df = sample_df.groupby(["AEROPUERTO", "ANIO", "MES"])["TOTAL PASAJEROS"].sum().reset_index()

# Crear índices
aeropuertos = sample_df["AEROPUERTO"].unique()
fechas = sample_df[["ANIO", "MES"]].drop_duplicates().sort_values(["ANIO", "MES"])
fechas_list = [f"{row['ANIO']}-{int(row['MES']):02d}" for _, row in fechas.iterrows()]


# Demanda
demanda = {}
for _, row in sample_df.iterrows():
    key = (row["AEROPUERTO"], f"{row['ANIO']}-{int(row['MES']):02d}")
    demanda[key] = row["TOTAL PASAJEROS"]

# Variables de decisión
x = {key: cp.Variable(nonneg=True) for key in demanda.keys()}


# Costos y presupuesto
alpha = 1.0
costos = {key: alpha for key in x}
B = 1e6  # presupuesto total

# Función objetivo
objetivo = cp.Minimize(cp.sum([cp.pos(demanda[key] - x[key]) for key in x]))

# Restricciones
restriccion_presupuesto = cp.sum([costos[key] * x[key] for key in x]) <= B

# Problema y solución
problema = cp.Problem(objetivo, [restriccion_presupuesto])
problema.solve()

# Mostrar primeros resultados
for key in list(x.keys())[:10]:
    print(f"Capacidad óptima asignada a {key}: {x[key].value:.2f} pasajeros")
import matplotlib.pyplot as plt

# Elegimos un aeropuerto para mostrar
aeropuerto_objetivo = "AEROPUERTO CORONEL FAP ALFREDO MENDIVIL DUARTE"

# Extraemos datos reales y optimizados
fechas_plot = []
demanda_plot = []
optimizacion_plot = []

for key in x:
    if key[0] == aeropuerto_objetivo:
        fechas_plot.append(key[1])
        demanda_plot.append(demanda[key])
        optimizacion_plot.append(x[key].value)

# Ordenar por fecha
fechas_plot, demanda_plot, optimizacion_plot = zip(*sorted(zip(fechas_plot, demanda_plot, optimizacion_plot)))

# Graficar
plt.figure(figsize=(10, 5))
plt.plot(fechas_plot, demanda_plot, marker='o', label="Demanda Real")
plt.plot(fechas_plot, optimizacion_plot, marker='x', label="Capacidad Optimizada")
plt.title(f"Comparación Demanda vs Optimización - {aeropuerto_objetivo}")
plt.xlabel("Fecha (AÑO-MES)")
plt.ylabel("Pasajeros")
plt.xticks(rotation=45)
plt.legend()
plt.tight_layout()
plt.grid(True)
plt.show()

# Crear columna de fecha tipo string
sample_df["FECHA"] = sample_df.apply(lambda row: f"{row['ANIO']}-{int(row['MES']):02d}", axis=1)

# Agregar capacidad óptima al DataFrame
sample_df["CAP_OPTIMA"] = sample_df.apply(lambda row: x[(row["AEROPUERTO"], row["FECHA"])].value, axis=1)

# Calcular cobertura
sample_df["COBERTURA"] = sample_df["CAP_OPTIMA"] / sample_df["TOTAL PASAJEROS"]

# Tabla resumen por aeropuerto
estadisticas = sample_df.groupby("AEROPUERTO").agg(
    Demanda_Promedio=("TOTAL PASAJEROS", "mean"),
    Demanda_Desviacion=("TOTAL PASAJEROS", "std"),
    Capacidad_Promedio=("CAP_OPTIMA", "mean"),
    Capacidad_Desviacion=("CAP_OPTIMA", "std"),
    Cobertura_Promedio=("COBERTURA", "mean"),
    Cobertura_Minima=("COBERTURA", "min"),
    Cobertura_Maxima=("COBERTURA", "max")
).round(2)

estadisticas.reset_index(inplace=True)
print(estadisticas)
