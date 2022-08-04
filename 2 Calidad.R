#  Se realiza exploracion general de los datos, se identifican los tipos de datos, algunos estadisticos basicos,
#  se identifican valores faltantes y atipicos y en general se revisa la coherencia de los datos.


# Activacion de librerias
library(readr)
library(lubridate)
library(ggplot2)
library(dplyr)
library(forcats)
library(stats)

# 1. Exploracion general 
glimpse(set_covid_original)

# Resultado columnas 23 , filas 6.265.798
# Cada dato informado corresponde a un caso de contagio de covid en Colombia


# 2. Tipos de datos

# Variable1: `fecha reporte web`                    tipo fecha    - numerica    - continua
# Variable2: `ID de caso`                           tipo numero   - numerica    - discreta.
# Variable3: `Fecha de notificación`                tipo fecha    - numerica    - continua.
# Variable4: `Código DIVIPOLA departamento`         tipo numero   - numerica    - discreta.
# Variable5: `Nombre departamento`                  tipo caracter - Categorica  - Nominal.
# Variable6: `Código DIVIPOLA municipio`            tipo numero   - numerica    - discreta.
# Variable7: `Nombre municipio`                     tipo caracter - Categorica  - Nominal.
# Variable8:  Edad                                  tipo numero   - numerica    - discreta
# Variable9: `Unidad de medida de edad`             tipo numero   - numerica    - discreta
# Variable10:  Sexo                                 tipo caracter - categorica  - binomial.
# Variable11: `Tipo de contagio`                    tipo caracter - categorica  - nominal.
# Variable12: `Ubicación del caso`                  tipo caracter - categorica  - nominal.
# Variable13: Estado                                tipo caracter - categorica  - nominal.
# Variable14: `Código ISO del país`                 tipo numero   - numerica    - discreta
# Variable15: `Nombre del país`                     tipo caracter - categorica  - nominal
# Variable16: Recuperado                            tipo caracter - categorica  - nominal
# Variable17: `Fecha de inicio de síntomas`         tipo fecha    - numerica    - continua
# Variable18: `Fecha de muerte`                     tipo fecha    - numerica    - continua
# Variable19: `Fecha de diagnóstico`                tipo fecha    - numerica    - continua
# Variable20: `Fecha de recuperación`               tipo fecha    - numerica    - continua
# Variable21: `Tipo de recuperación`                tipo caracter - categorica  - nominal
# Variable22: `Pertenencia étnica`                  tipo numero   - numerica    - discreto
# Variable23: `Nombre del grupo étnico`             tipo caracter - categorica  - nominal


# 3. Analisis de datos totales y faltantes

set_covid_original %>% 
  summarise(var1_total = sum(!is.na(`fecha reporte web`)),
            var1_falta = sum(is.na(`fecha reporte web`)),
            var2_total = sum(!is.na(`ID de caso`)),
            var2_falta = sum(is.na(`ID de caso`)),
            var3_total = sum(!is.na(`Fecha de notificación`)),
            var3_falta = sum(is.na(`Fecha de notificación`)),
            var4_total = sum(!is.na(`Código DIVIPOLA departamento`)),
            var4_falta = sum(is.na(`Código DIVIPOLA departamento`)),
            var5_total = sum(!is.na(`Nombre departamento`)),
            var5_falta = sum(is.na(`Nombre departamento`)),
            var6_total = sum(!is.na(`Código DIVIPOLA municipio`)),
            var6_falta = sum(is.na(`Código DIVIPOLA municipio`)),
            var7_total = sum(!is.na(`Nombre municipio`)),
            var7_falta = sum(is.na(`Nombre municipio`)),
            var8_total = sum(!is.na(Edad)),
            var8_falta = sum(is.na(Edad)),
            var9_total = sum(!is.na(`Unidad de medida de edad`)),
            var9_falta = sum(is.na(`Unidad de medida de edad`)),
            var10_total = sum(!is.na(Sexo)),
            var10_falta = sum(is.na(Sexo)),
            var11_total = sum(!is.na(`Tipo de contagio`)),
            var11_falta = sum(is.na(`Tipo de contagio`)),
            var12_total = sum(!is.na(`Ubicación del caso`)),
            var12_falta = sum(is.na(`Ubicación del caso`)),
            var13_total = sum(!is.na(Estado)),
            var13_falta = sum(is.na(Estado)),
            var14_total = sum(!is.na(`Código ISO del país`)),
            var14_falta = sum(is.na(`Código ISO del país`)),
            var15_total = sum(!is.na(`Nombre del país`)),
            var15_falta = sum(is.na(`Nombre del país`)),
            var16_total = sum(!is.na(Recuperado)),
            var16_falta = sum(is.na(Recuperado)),
            var17_total = sum(!is.na(`Fecha de inicio de síntomas`)),
            var17_falta = sum(is.na(`Fecha de inicio de síntomas`)),
            var18_total = sum(!is.na(`Fecha de muerte`)),
            var18_falta = sum(is.na(`Fecha de muerte`)),
            var19_total = sum(!is.na(`Fecha de diagnóstico`)),
            var19_falta = sum(is.na(`Fecha de diagnóstico`)),
            var20_total = sum(!is.na(`Fecha de recuperación`)),
            var20_falta = sum(is.na(`Fecha de recuperación`)),
            var21_total = sum(!is.na(`Tipo de recuperación`)),
            var21_falta = sum(is.na(`Tipo de recuperación`)),
            var22_total = sum(!is.na(`Pertenencia étnica`)),
            var22_falta = sum(is.na(`Pertenencia étnica`)),
            var23_total = sum(!is.na(`Nombre del grupo étnico`)),
            var23_falta = sum(is.na(`Nombre del grupo étnico`))) %>% 
  View()

# Resultados encontrados: Se presentan datos faltantes  en la variables:
#                         Variable14: `Código ISO del país`
#                         Variable15: `Nombre del país`
#                         Variable17: `Fecha de inicio de síntomas`
#                         Variable18: `Fecha de muerte`
#                         Variable19: `Fecha de diagnóstico`
#                         Variable20: `Fecha de recuperación`
#                         Variable21: `Tipo de recuperación`
#                         Variable22: `Pertenencia étnica`
#                         Variable23: `Nombre del grupo étnico`



# 3. Revision del contenido de las variables con datos faltantes

# Variable14 `Código ISO del país`
set_covid_original %>% 
  group_by(`Código ISO del país`) %>% 
  summarise(total = sum(!is.na(`Código ISO del país`)),
            faltantes = sum(is.na(`Código ISO del país`))) %>% 
  View()
# Resultados encontrados: 
#                     1. Se debe actualizar el codigo del pais a 170
#                     2. Actualizar el nombre de la columna a codpais

# Variable15 `Nombre del país`
set_covid_original %>% 
  group_by(`Nombre del país`) %>% 
  summarise(total = sum(!is.na(`Nombre del país`)),
            faltantes = sum(is.na(`Nombre del país`))) %>% 
  View()
# Resultados encontrados: 
#                     1. Se debe actualizar el nombre del país a Colombia
#                     2. Actualizar el nombre de la variable a nompais


# Variable17 `Fecha de inicio de síntomas`
set_covid_original %>% 
  group_by(`Fecha de inicio de síntomas`) %>% 
  summarise(total = sum(!is.na(`Fecha de inicio de síntomas`)),
            faltantes = sum(is.na(`Fecha de inicio de síntomas`))) %>% 
  View()

set_covid_original %>% 
  filter(is.na(`Fecha de inicio de síntomas`)) %>% 
  View()

set_covid_original %>% 
  filter(is.na(`Fecha de inicio de síntomas`) & Recuperado !="Recuperado") %>% 
  View()

set_covid_original %>% 
  filter(is.na(`Fecha de inicio de síntomas`) & Recuperado =="Recuperado") %>% 
  View()

# Resultados encontrados: 
#         1. Se debe analizar por qué se presentan 499.892 registros faltantes.
#         2. Se encontraron 3.052 datos faltantes con fecha de muerte y sin fecha de inicio  de sintomas.
#         3. Se encontraron 496.840 datos faltantes con estado Recuperado y sin fecha de inicio de sintomas.
#         4. Supuesto: Actualizar la fecha de inicio de sintomas con la fecha de diagnostico.
#         5. Actualiza el nombre de la variable a fsintomas.


# Variable18 `Fecha de muerte`
set_covid_original %>% 
  group_by(`Fecha de muerte`) %>% 
  summarise(total = sum(!is.na(`Fecha de muerte`)),
            faltantes = sum(is.na(`Fecha de muerte`))) %>% 
  View()

set_covid_original %>% 
  filter(is.na(`Fecha de muerte`)) %>% 
  View()

set_covid_original %>% 
  filter(is.na(`Fecha de muerte`) & Recuperado == "Recuperado") %>% 
  View()

set_covid_original %>% 
  filter(is.na(`Fecha de muerte`) & Recuperado == "Activo") %>% 
  View()

# Resultados encontrados:
#         1. Se encontraron 6.094.277 faltantes
#         2. Se encontraron 6.075.114 registros recuperados y sin fecha de muerte.  Esto es correcto.
#         3. Se encontraron 19.163 casos aun activos y sin fecha de muerte. Esto es correcto.
#         4. Conclusion no se debe realizar ningun ajuste.
#         5. Actualizar el nombre de la variable a fmuerte


# Variable19 `Fecha de diagnóstico`
set_covid_original %>% 
  group_by(`Fecha de diagnóstico`) %>% 
  summarise(total = sum(!is.na(`Fecha de diagnóstico`)),
            faltantes = sum(is.na(`Fecha de diagnóstico`))) %>% 
  View()

set_covid_original %>% 
  filter(is.na(`Fecha de diagnóstico`)) %>% 
  View()

# Resultados encontrados:
#         1. Se encontraron 2.755 registros con datos faltantes.
#         2. No se encuentra ninguna informacion que valide a que se debe los datos faltantes.
#         3. Supuesto: Se puede actualizar la fecha de diagnostico con la fecha de inicio de sintomas.
#         4. Actualizar el nombre de la variable a fdiagn.


# Variable20 `Fecha de recuperación`
set_covid_original %>% 
  group_by(`Fecha de recuperación`) %>% 
  summarise(total = sum(!is.na(`Fecha de recuperación`)),
            faltantes = sum(is.na(`Fecha de recuperación`))) %>% 
  View()

set_covid_original %>% 
  filter(is.na(`Fecha de recuperación`) & Recuperado == "N/A") %>% 
  View()


set_covid_original %>% 
  filter(is.na(`Fecha de recuperación`) & Recuperado == "Activo") %>% 
  View()

# Resultados encontrados:
#         1. Se encontraron 186.170 casos con informacion faltante.
#         2. Se encontraron 140.845 registros con informacion faltante pero que ya estan fallecidos. Esto es correcto.
#         3. Se encontraron 26.162 registros con informacion faltante pero que tienen fecha de muerte. Esto es correcto.
#         4. Se encontraron 19.163 registros con informacion faltante y estado Activo.  Esto es correcto.
#         5. Conclusion: No se debe realizar ningun ajuste a esta variable.
#         6. Actualizar el nombre de la variable a frecup



# Variable21 `Tipo de recuperación`
set_covid_original %>% 
  group_by(`Tipo de recuperación`) %>% 
  summarise(total = sum(!is.na(`Tipo de recuperación`)),
            faltantes = sum(is.na(`Tipo de recuperación`))) %>% 
  View()

set_covid_original %>% 
  filter(is.na(`Tipo de recuperación`)) %>% 
  View()

# Resultados encontrados:
#         1. Se encontraron 186.170 registros con informacion faltante.
#         2. Los datos faltantes corresponden a personas fallecidas o casos activos. Esto es correcto
#         3. Conclusion: No se debe realizar ningun ajuste a esta variable.
#         4. Actualizar el nombre de la variable a trecup




# Variable22 `Pertenencia étnica`
set_covid_original %>% 
  group_by(`Pertenencia étnica`) %>% 
  summarise(total = sum(!is.na(`Pertenencia étnica`)),
            faltantes = sum(is.na(`Pertenencia étnica`))) %>% 
  View()

set_covid_original %>% 
  filter(is.na(`Pertenencia étnica`)) %>% 
  View()

# Resultados encontrados:
#                     1. Se encontraron 18.164 registros con informacion faltante.
#                     2. No hay informacion consistente para comprender a que se debe los NA.
#                     3. Conclusion: no realizar cambios en la variable.
#                     4. Actualizar el nombre de la variable a gpoetnia




# Variable23 `Nombre del grupo étnico`
set_covid_original %>% 
  group_by(`Nombre del grupo étnico`) %>% 
  summarise(total = sum(!is.na(`Nombre del grupo étnico`)),
            faltantes = sum(is.na(`Nombre del grupo étnico`))) %>% 
  View()

# Resultados encontrados: 
#                     1. Se encontraron 6.183.385 registros con informacion faltante.
#                     2. No hay informacion consistente para comprender a que se debe los NA.
#                     3. Conclusion: no realizar cambios en la variable.
#                     4. Actualizar el nombre de la columna a nometnia



# 4 Analisis de contenido y algunos estadisticos descriptivos basicos

# Variable1: `fecha reporte web` 
set_covid_original %>% 
  summarise(minina = min(`fecha reporte web`),
            maxima = max(`fecha reporte web`)) %>% 
  View()

# Resultados encontrados:
#                     1. Actualizar el nombre de la variable a frepor


# Variable2: `ID de caso`
set_covid_original %>% 
  summarise(registros = sum(!is.na(`ID de caso`)),
            minimo = min(`ID de caso`),
            maximo = max(`ID de caso`)) %>% 
  View()


set_covid_original %>% 
  summarise(registros = sum(!is.na(`ID de caso`)),
            maximo = max(`ID de caso`),
            diferencia = maximo  - registros) %>% 
  View()

# Resultados encontrados:
#                     1. El numero total de registros no coincide con el id caso mayor.
#                     2. Se presenta una diferencia de 40 registros entre el mayor codigo y el total de registros.
#                     3. Esto puede ser evidencia de informacion faltante o eliminada.
#                     4. Actualizar el nombre de la variable a idcaso.



# Variable3: `Fecha de notificación`
set_covid_original %>% 
  summarise(minina = min(`Fecha de notificación`),
            maxima = max(`Fecha de notificación`)) %>% 
  View()

# Resultados encontrados:
#                     1. Actualizar el nombre de la variable a fnotif.



# Variable4: `Código DIVIPOLA departamento` y Variable5: `Nombre departamento`
set_covid_original %>% 
  group_by(`Código DIVIPOLA departamento`, `Nombre departamento`) %>% 
  summarise(registros = sum(!is.na(`Nombre departamento`))) %>% 
  arrange(`Nombre departamento`) %>% 
  ungroup() %>%  
  View()

# Resultados encontrados:
#                     1. Se debe actualizar BARRANQUILLA a ATLANTICO.
#                     2. Se debe actualizar Caldas a CALDAS.
#                     3. Se debe actualizar CARTAGENA a BOLIVAR.
#                     4. Se debe actualizar Cundinamarca a CUNDINAMARCA
#                     5. Se debe actualizar Santander a SANTANDER
#                     6. Se debe actualizar STA MARTA D.E. a MAGDALENA
#                     7. Se debe actualizar Santander a SANTANDER
#                     8. Se debe actualizar Tolima a TOLIMA
#                     9. Se debe actualizar el codigo 8001 a 8.
#                     10. Se debe actualizar el codigo 13001 a 13.
#                     11. Se debe actualizar el codigo 47001 a 47.
#                     12. Actualizar el nombre de la columna a coddpto y nomdpto



# Variable6: `Código DIVIPOLA municipio` y Variable7: `Nombre municipio`
set_covid_original %>% 
  group_by(`Código DIVIPOLA municipio`, `Nombre municipio`, `Nombre departamento`) %>% 
  summarise(registros = sum(!is.na(`Nombre municipio`))) %>% 
  arrange(`Nombre municipio`) %>% 
  ungroup() %>% 
  View()

# Resultados encontrados
#                     1. Se debe actualizar Anserma a ANSERMA.
#                     2. Se debe actualizar barrancabermeja a BARRANCABERMEJA
#                     3. Se debe actualizar ENTRERrIOS a ENTRERRIOS
#                     4. Se debe actualizar gachala a GACHALA
#                     5. Se debe actualizar Galapa a GALAPA
#                     6. Se debe actualizar Gameza a GAMEZA
#                     7. Se debe actualizar Guepsa a GUEPSA
#                     8. Se debe actualizar Medellin a MEDELLIN
#                     9. Se debe actualizar MEDELLiN a MEDELLIN
#                     10. Se debe actualizar momil a MOMIL
#                     11. Se debe actualizar Pensilvania a PENSILVANIA
#                     12. Se debe actualizar puerto colombia a PUERTO COLOMBIA
#                     13. Se debe actualizar puerto COLOMBIA a PUERTO COLOMBIA
#                     14. Se debe actualizar Somondoco a SOMONDOCO
#                     15. Actualizar el nombre de la variable a codmpio y nommpio



# Variable8:  Edad
set_covid_original %>% 
  group_by(Edad) %>% 
  summarise(registros = sum(!is.na(Edad))) %>% 
  View() 

# Resultados encontrados
#                     1. Conclusion: No se debe ajustar ningun dato
#                     2. Actualizar el nombre de la variable a edad



# Variable9: `Unidad de medida de edad`
set_covid_original %>% 
  group_by(`Unidad de medida de edad`) %>% 
  summarise(registros = sum(!is.na(`Unidad de medida de edad`))) %>% 
  View() 

# Resultados encontrados
#                     1. Conclusion: No se debe ajustar ningun dato
#                     2, Actualizar el nombre de la variable a uniedad



# Variable10:  Sexo
set_covid_original %>% 
  group_by(Sexo) %>% 
  summarise(registros = sum(!is.na(Sexo))) %>% 
  View() 

# Resultados encontrados
#                     1. Se debe ajustar f a F.
#                     2. Se debe ajustar m a M.
#                     3. Actualizar el nombre de la variable a sexo



# Variable11: `Tipo de contagio`
set_covid_original %>% 
  group_by(`Tipo de contagio`) %>% 
  summarise(registros = sum(!is.na(`Tipo de contagio`))) %>% 
  View() 

# Resultados encontrados
#                     1. Conclusion: No se debe ajustar ningun dato
#                     2. Actualizar el nombre de la variable a tcontg


# Variable12: `Ubicación del caso`
set_covid_original %>% 
  group_by(`Ubicación del caso`) %>% 
  summarise(registros = sum(!is.na(`Ubicación del caso`))) %>% 
  View() 

set_covid_original %>% 
  filter(`Ubicación del caso` == "N/A" & !is.na(`Fecha de muerte`)) %>% 
  View()
  

# Resultados encontrados
#                     1. Se debe actualizar casa a Casa.
#                     2. Se debe actualizar CASA a Casa.
#                     3. Se debe actualizar Hospital UCI a UCI
#                     4. Se debe actualizar Fallecimiento a Cementerio
#                     5. Se tienen 30.676 registros marcados como N/A. 
#                     6. Se debe actualizar a Cementerio 30.676 donde ubicacion es N/A y fecha de muerte es diferente a NA.
#                     7. Actualizar el nombre de la variable a ubicaso



# Variable13: Estado
set_covid_original %>% 
  group_by(Estado) %>% 
  summarise(registros = sum(!is.na(Estado))) %>% 
  View() 

set_covid_original %>% 
  filter(Estado == "N/A") %>% 
  View()

# Resultados encontrados:
#                     1. Actualizar el nombre de la variable a nivel
#                     2. Se debe actualizar leve a Leve
#                     3. Se debe actualizar LEVE a Leve.
#                     4. Se debe actualizar 30.676 casos con valor N/A a Fallecido



# Variable16: Recuperado
set_covid_original %>% 
  group_by(Recuperado) %>% 
  summarise(registros = sum(!is.na(Recuperado))) %>% 
  View() 

set_covid_original %>% 
  filter(Recuperado == "N/A") %>% 
  View()

# Resultados encontrados:
#                     1. Actualizar el nombre de la variable a estado
#                     2. Actualizar fallecido a Fallecido
#                     3. Se encontraron 26.162 registros con registros N/A y con fecha de muerte. Esto no es correcto
#                     4. Se debe actualizar a Fallecido los 26.162 registros con valor N/A
  



