# Transformacion del set de datos hasta obtener el archivo base para los analisis

# Activacion de librerias
library(readr)
library(lubridate)
library(ggplot2)
library(dplyr)
library(forcats)
library(stats)

# Creacion del archivo base
dtf_casos <- set_covid_original

glimpse(dtf_casos)

# Actualizacion nombres de columnas
dtf_casos <- dtf_casos %>% 
  rename(frepor = `fecha reporte web`,
         idcaso = `ID de caso`,
         fnotif = `Fecha de notificación`,
         coddpto = `Código DIVIPOLA departamento`,
         nomdpto = `Nombre departamento`,
         codmpio = `Código DIVIPOLA municipio`,
         nommpio = `Nombre municipio`,
         edad = Edad,
         uniedad = `Unidad de medida de edad`,
         sexo = Sexo,
         tcontg = `Tipo de contagio`,
         ubicaso = `Ubicación del caso`,
         nivel = Estado,
         codpais = `Código ISO del país`,
         nompais = `Nombre del país`,
         estado = Recuperado,
         fsintomas = `Fecha de inicio de síntomas`,
         fmuerte = `Fecha de muerte`,
         fdiagn = `Fecha de diagnóstico`,
         frecup = `Fecha de recuperación`,
         trecup = `Tipo de recuperación`,
         gpoetnia = `Pertenencia étnica`,
         nometnia = `Nombre del grupo étnico`)


# Actualizacion del codigo y nombre del pais
dtf_casos <- dtf_casos %>% 
  mutate(nompais = ifelse(nompais == "TABATINGA", "BRASIL", nompais),
         codpais = ifelse(nompais == "BRASIL" & is.na(codpais), 76, codpais),
         codpais = ifelse(is.na(codpais), 170, codpais),
         nompais = ifelse(is.na(nompais), "COLOMBIA", nompais),
         codpais = ifelse(nompais == "BRASIL" & codpais != 76, 76, codpais),
         nompais = ifelse(nompais == "REPÚBLICA DOCIMINCANA", "REPÚBLICA DOMINICANA", nompais),
         codpais = ifelse(nompais == "FRANCIA" & codpais == 270, 250 , codpais),
         nompais = ifelse(nompais == "MEXICO", "MÉXICO", nompais),
         nompais = ifelse(nompais == "PANAMA", "PANAMÁ", nompais),
         nompais = ifelse(nompais == "PERU", "PERÚ", nompais),
         nompais = ifelse(nompais == "ARABIA SAUDÍ", "ARABIA SAUDITA", nompais),
         nompais = ifelse(nompais == "REINO UNIDO DE GRAN BRETAÑA E IRLANDA DEL NORTE", "REINO UNIDO", nompais),
         nompais = ifelse(nompais == "ESTADOS UNIDOS DE AMÉRICA", "ESTADOS UNIDOS", nompais),
         nompais = ifelse(nompais == "VENEUELA", "VENEZUELA", nompais),
         codpais = ifelse(nompais == "ESTADOS UNIDOS" & codpais == 1249, 840, codpais),
         codpais = ifelse(nompais == "CHILE" & codpais == 1152, 152, codpais),
         codpais = ifelse(nompais == "ECUADOR" & codpais == 1239, 218, codpais),
         codpais = ifelse(nompais == "ITALIA" & codpais == 1380, 380, codpais),
         codpais = ifelse(nompais == "PAÍSES BAJOS" & codpais == 1528, 528, codpais),
         codpais = ifelse(nompais == "PERÚ" & codpais == 1589, 604, codpais),
         codpais = ifelse(nompais == "PANAMÁ" & codpais == 1580, 591, codpais),
         codpais = ifelse(nompais == "ESPAÑA" & codpais == 1724, 724, codpais),
         codpais = ifelse(nompais == "CURAZAO" & codpais == 1000, 531, codpais))


# Actualizacion de codigos y nombres de departamentos
dtf_casos <- dtf_casos %>% 
  mutate(nomdpto = ifelse(nomdpto == "BARRANQUILLA", "ATLANTICO", nomdpto),
         nomdpto = ifelse(nomdpto == "Caldas", "CALDAS", nomdpto),
         nomdpto = ifelse(nomdpto == "CARTAGENA", "BOLIVAR", nomdpto),
         nomdpto = ifelse(nomdpto == "Cundinamarca", "CUNDINAMARCA", nomdpto),
         nomdpto = ifelse(nomdpto == "Santander", "SANTANDER", nomdpto),
         nomdpto = ifelse(nomdpto == "STA MARTA D.E.", "MAGDALENA", nomdpto),
         nomdpto = ifelse(nomdpto == "Tolima", "TOLIMA", nomdpto),
         coddpto = ifelse(coddpto == 8001, 8, coddpto),
         coddpto = ifelse(coddpto == 13001, 13, coddpto),
         coddpto = ifelse(coddpto == 47001, 47, coddpto))



# Actualizacion de nombres de municipios
dtf_casos <- dtf_casos %>% 
  mutate(nommpio = ifelse(nommpio == "Anserma", "ANSERMA", nommpio),
         nommpio = ifelse(nommpio == "barrancabermeja", "BARRANCABERMEJA", nommpio),
         nommpio = ifelse(nommpio == "ENTRERrIOS", "ENTRERRIOS", nommpio),
         nommpio = ifelse(nommpio == "gachala", "GACHALA", nommpio),
         nommpio = ifelse(nommpio == "Galapa", "GALAPA", nommpio),
         nommpio = ifelse(nommpio == "Gameza", "GAMEZA", nommpio),
         nommpio = ifelse(nommpio == "Guepsa", "GUEPSA", nommpio),
         nommpio = ifelse(nommpio == "Medellin", "MEDELLIN", nommpio),
         nommpio = ifelse(nommpio == "MEDELLiN", "MEDELLIN", nommpio),
         nommpio = ifelse(nommpio == "momil", "MOMIL", nommpio),
         nommpio = ifelse(nommpio == "Pensilvania", "PENSILVANIA", nommpio),
         nommpio = ifelse(nommpio == "puerto colombia", "PUERTO COLOMBIA", nommpio),
         nommpio = ifelse(nommpio == "puerto COLOMBIA", "PUERTO COLOMBIA", nommpio),
         nommpio = ifelse(nommpio == "Somondoco", "SOMONDOCO", nommpio))


# Actualizacion de sexo
dtf_casos <- dtf_casos %>% 
  mutate(sexo = ifelse(sexo == "f", "F", sexo),
         sexo = ifelse(sexo == "m", "M", sexo))


# Actualizacion ubicacion del caso
dtf_casos <- dtf_casos %>% 
  mutate(ubicaso = ifelse(ubicaso == "casa", "Casa", ubicaso),
         ubicaso = ifelse(ubicaso == "CASA", "Casa", ubicaso),
         ubicaso = ifelse(ubicaso == "Hospital UCI", "UCI", ubicaso),
         ubicaso = ifelse(ubicaso == "Fallecido", "Cementerio", ubicaso),
         ubicaso = ifelse(ubicaso == "N/A" & !is.na(fmuerte), "Cementerio", ubicaso))


# Actualizacion de la variable nivel
dtf_casos <- dtf_casos %>% 
  mutate(nivel = ifelse(nivel == "leve", "Leve", nivel),
         nivel = ifelse(nivel == "LEVE", "Leve", nivel),
         nivel = ifelse(nivel == "N/A", "Fallecido", nivel),
         nivel = ifelse(nivel == "Fallecido", "Muy grave", nivel))

# Actualizacion de la variable estado
dtf_casos <- dtf_casos %>% 
  mutate(estado = ifelse(estado == "fallecido", "Fallecido", estado),
         estado = ifelse(estado == "N/A", "Fallecido", estado),
         estado = ifelse(estado == "Recuperado" & !is.na(fmuerte), "Fallecido", estado))


# Actualizacion fecha de recuperacion
dtf_casos <- dtf_casos %>% 
  mutate(frecup = ifelse(estado == "Fallecido" & !is.na(frecup), NA, frecup))

# Actualizacion del campo tipo recuperacion
dtf_casos <- dtf_casos %>% 
  mutate(trecup = ifelse(estado == "Fallecido" & !is.na(trecup), NA, trecup))

# Adicion de las columnas nro de semana y dia de semana
dtf_casos <- dtf_casos %>% 
  mutate(diasem = wday(fnotif, label = TRUE),
         nrosem = isoweek(fnotif))


# Actualizacion nrosem de la primera semana de 2022
dtf_casos <- dtf_casos %>% 
  mutate(nrosem = ifelse(year(fnotif) == "2022" &  nrosem == 52, 1, nrosem))

dtf_casos %>% 
  filter(year(fnotif) == "2022" & nrosem == 52) %>% 
  View()

# Actualizacion del tipo de recuperacion
dtf_casos <- dtf_casos %>% 
  mutate(trecup = ifelse(estado == "Activo" & is.na(trecup), "Aún sin recuperacion", trecup))


# Orden del archvo de analisis
dtf_casos <- dtf_casos %>% 
  select(idcaso, frepor, fnotif, nrosem, diasem, codpais, nompais, coddpto, nomdpto, codmpio, nommpio, edad, sexo,
         estado, nivel, ubicaso, tcontg, fsintomas, fdiagn, frecup, trecup, fmuerte)






