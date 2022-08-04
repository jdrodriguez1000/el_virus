#### Archivo de importacion de los set de datos -------

# Activacion de librerias
library(readr)
library(lubridate)
library(ggplot2)
library(dplyr)

# Carga del set de datos original
ruta_archivo <- file.choose()
set_covid_original <- read_csv(ruta_archivo)