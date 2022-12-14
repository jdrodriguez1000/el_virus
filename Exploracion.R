# Activacion de librerias
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(nortest)
library(modeest)
library(tidyr)
library(stringr)




# ----------------------                                ----------------------
# ----------------------                                ----------------------
# ----------------------      ANALISIS UNIVVARIADO      ---------------------- 
# ----------------------                                ----------------------
# ----------------------                                ----------------------



# ----------------------                                ----------------------
# ----------------------          VARIABLE frepor       ----------------------
# ----------------------                                ----------------------

# Tipo de variable: Fecha - Numerica - Continua

# ----------------------          DATOS GENERALES        ----------------------

# Registros totales
dtf_casos %>% 
  summarise(total_registros = sum(!is.na(frepor))) %>% 
  View()

# Registros faltantes
dtf_casos %>% 
  summarise(total_registros = sum(is.na(frepor))) %>% 
  View()

# Ultimos 15 valores registrados
dtf_casos %>% 
  group_by(frepor) %>% 
  summarise(total_registros = sum(!is.na(frepor))) %>% 
  arrange(desc(frepor)) %>% 
  head(15) %>% 
  View()

# 20 Valores maximos
dtf_casos %>% 
  group_by(frepor) %>% 
  summarise(total_registros = sum(!is.na(frepor))) %>% 
  arrange(desc(total_registros)) %>% 
  head(20) %>% 
  View()

# 20 Valores minimos
dtf_casos %>% 
  group_by(frepor) %>% 
  summarise(total_registros = sum(!is.na(frepor))) %>% 
  arrange(total_registros) %>% 
  head(20) %>% 
  View()


# ----------------------    ESTADISTICOS DESCRIPTIVOS    ----------------------

dtf_casos %>% 
  summarise(fecha_minima = min(frepor),
            fecha_maxima = max(frepor),
            Rango = fecha_maxima - fecha_minima,
            media = mean(frepor),
            mediana = median(frepor)) %>% 
  View()



# ----------------------      GRÁFICO HISTOGRAMA          ----------------------

ggplot(dtf_casos, aes(frepor)) +
  geom_histogram(bins = 30, fill = "#ABEBC6", color = "#5D6D7E") +
  theme_bw() +
  labs(title = "Gráfico contactos por fecha de reporte",
       x = "Fecha de reporte",
       y = "Número de casos")+
  theme(axis.text.x = element_text(size = 7, angle = 90)) +
  theme(axis.text.y = element_text(size = 7))



# ----------------------      GRÁFICO BOXPLOT          ----------------------

ggplot(dtf_casos, aes(frepor)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.shape = 10, outlier.size = 1) +
  theme_bw() +
  coord_flip() +
  scale_fill_brewer(palette = "Blues") +
  stat_boxplot(geom = "errorbar", width = 0.25, alpha = 0.5) +
  labs(title = "Gráfico de cjas - Fecha de reporte",
       x = "Fecha de reporte",
       y = "") +
  theme(axis.text.x = element_text(size = 0))


# ----------------------  GRÁFICO HISTOGRAMA POR AÑO    ----------------------
  
ggplot(dtf_casos, aes(frepor)) +
  geom_histogram(bins = 30, fill = "#ABEBC6", color = "#5D6D7E", alpha = 0.5) +
  theme_bw() +
  labs(title = "Gráfico contactos por fecha de reporte",
       x = "Fecha de reporte",
       y = "Número de casos")+
  theme(axis.text.x = element_text(size = 7, angle = 90)) +
  theme(axis.text.y = element_text(size = 7)) +
  facet_grid(cols = vars(year(dtf_casos$frepor)))


# ----------------------  DATA SET DE CASOS REPORTADOS    ----------------------

#Creacion del set
dtf_reportados <- dtf_casos %>% 
  group_by(frepor) %>% 
  summarise(total_registros = sum(!is.na(frepor)))

# Adicion de columna de acumulados
dtf_reportados <- dtf_reportados %>% 
  mutate(registros_acumulados = cumsum(total_registros))

# Cambio de nombre de columna
dtf_reportados <- dtf_reportados %>% 
  rename(fecha_registro = frepor)



# ---------  ESTADISTICOS DESCRIPTIVOS  SET DE DATOS REPORTADOS   --------------

dtf_reportados %>% 
  summarise(minimo = min(total_registros),
            maximo = max(total_registros),
            rango = maximo - minimo,
            media = mean(total_registros),
            mediana = median(total_registros),
            desv_std = sd(total_registros),
            varianza = var(total_registros),
            perc25 = quantile(total_registros, 0.25),
            perc75 = quantile(total_registros, 0.75),
            rangoIC = perc75 - perc25,
            dcil1 = quantile(total_registros, 0.1),
            dcil9 = quantile(total_registros, 0.9)) %>% 
  View()


# ---------------  GRAFICOS SET DE DATOS CASOS REPORTADOS  --------------------

# Grafico contagios reportados diarios
ggplot(dtf_reportados, aes(fecha_registro, total_registros)) +
  geom_col(fill = "#C39BD3", alpha = 0.5) +
  theme_bw() +
  labs(title = "Gráfico de Casos reportados",
       x = "Fechas",
       y = "Casos reportados") +
  theme(axis.text.x = element_text(size = 7, angle = 90)) +
  theme(axis.text.y = element_text(size = 7))

# Grafico contagios reportados diarios tipo linea
ggplot(dtf_reportados, aes(fecha_registro, total_registros)) +
  geom_line(color = "#C39BD3") +
  theme_bw() +
  labs(title = "Gráfico de Casos reportados",
       x = "Fechas",
       y = "Casos reportados") +
  theme(axis.text.x = element_text(size = 7, angle = 90)) +
  theme(axis.text.y = element_text(size = 7))

# Grafico acumulado de contagios diarios 
ggplot(dtf_reportados, aes(fecha_registro, registros_acumulados)) +
  geom_point(color = "#C39BD3", alpha = 0.6) +
  theme_bw() +
  labs(title = "Gráfico de casos acumulados",
       x = "Fechas",
       y = "Casos reportados") +
  theme(axis.text.x = element_text(size = 7, angle = 90)) +
  theme(axis.text.y = element_text(size = 7))

# Grafico boxplot
ggplot(dtf_reportados, aes(total_registros)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.shape = 10, outlier.size = 1) +
  theme_bw() +
  coord_flip() +
  scale_fill_brewer(palette = "Blues") +
  stat_boxplot(geom = "errorbar", width = 0.25, alpha = 0.5) +
  labs(title = "Gráfico de casos reportados",
       x = "Total registros",
       y = "") +
  theme(axis.text.x = element_text(size = 0))





# ----------------------                                ----------------------
# ----------------------          VARIABLE fnotif       ----------------------
# ----------------------                                ----------------------

# Tipo de variable: Fecha - Numerica - Continua

# ----------------------          DATOS GENERALES        ----------------------

# Registros totales
dtf_casos %>% 
  summarise(total_registros = sum(!is.na(fnotif))) %>% 
  View()

# Registros faltantes
dtf_casos %>% 
  summarise(total_registros = sum(is.na(fnotif))) %>% 
  View()

# Ultimos 15 valores registrados
dtf_casos %>% 
  group_by(fnotif) %>% 
  summarise(total_registros = sum(!is.na(fnotif))) %>% 
  arrange(desc(fnotif)) %>% 
  head(15) %>% 
  View()

# 20 Valores maximos
dtf_casos %>% 
  group_by(fnotif) %>% 
  summarise(total_registros = sum(!is.na(fnotif))) %>% 
  arrange(desc(total_registros)) %>% 
  head(20) %>% 
  View()

# 20 Valores minimos
dtf_casos %>% 
  group_by(fnotif) %>% 
  summarise(total_registros = sum(!is.na(fnotif))) %>% 
  arrange(total_registros) %>% 
  head(20) %>% 
  View()


# ----------------------    ESTADISTICOS DESCRIPTIVOS    ----------------------

dtf_casos %>% 
  summarise(fecha_minima = min(fnotif),
            fecha_maxima = max(fnotif),
            Rango = fecha_maxima - fecha_minima,
            media = mean(fnotif),
            mediana = median(fnotif)) %>% 
  View()



# ----------------------      GRÁFICO HISTOGRAMA          ----------------------

ggplot(dtf_casos, aes(fnotif)) +
  geom_histogram(bins = 30, fill = "#ABEBC6", color = "#5D6D7E") +
  theme_bw() +
  labs(title = "Gráfico contactos por fecha de notificacion",
       x = "Fecha de notificacion",
       y = "Número de casos")+
  theme(axis.text.x = element_text(size = 7, angle = 90)) +
  theme(axis.text.y = element_text(size = 7))



# ----------------------      GRÁFICO BOXPLOT          ----------------------

ggplot(dtf_casos, aes(fnotif)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.shape = 10, outlier.size = 1) +
  theme_bw() +
  coord_flip() +
  scale_fill_brewer(palette = "Blues") +
  stat_boxplot(geom = "errorbar", width = 0.25, alpha = 0.5) +
  labs(title = "Gráfico de cajas - Fecha de notificacion",
       x = "Fecha de notificación",
       y = "") +
  theme(axis.text.x = element_text(size = 0))


# ----------------------  GRÁFICO HISTOGRAMA POR AÑO    ----------------------

ggplot(dtf_casos, aes(fnotif)) +
  geom_histogram(bins = 30, fill = "#ABEBC6", color = "#5D6D7E", alpha = 0.5) +
  theme_bw() +
  labs(title = "Gráfico contactos por fecha de notificación",
       x = "Fecha de notificación",
       y = "Número de casos")+
  theme(axis.text.x = element_text(size = 7, angle = 90)) +
  theme(axis.text.y = element_text(size = 7)) +
  facet_grid(cols = vars(year(dtf_casos$fnotif)))


# ----------------------  DATA SET DE CASOS NOTIFICADOS    ----------------------

#Creacion del set
dtf_notificados <- dtf_casos %>% 
  group_by(fnotif) %>% 
  summarise(total_registros = sum(!is.na(fnotif)))

# Adicion de columna de acumulados
dtf_notificados <- dtf_notificados %>% 
  mutate(registros_acumulados = cumsum(total_registros))

# Cambio de nombre de columna
dtf_notificados <- dtf_notificados %>% 
  rename(fecha_notificacion = fnotif)



# --------- ESTADISTICOS DESCRIPTIVOS  SET DE DATOS NOTIFICADOS  ---------------

dtf_notificados %>% 
  summarise(minimo = min(total_registros),
            maximo = max(total_registros),
            rango = maximo - minimo,
            media = mean(total_registros),
            mediana = median(total_registros),
            desv_std = sd(total_registros),
            varianza = var(total_registros),
            perc25 = quantile(total_registros, 0.25),
            perc75 = quantile(total_registros, 0.75),
            rangoIC = perc75 - perc25,
            dcil1 = quantile(total_registros, 0.1),
            dcil9 = quantile(total_registros, 0.9)) %>% 
  View()


# ---------------  GRAFICOS SET DE DATOS CASOS NOTIFICADOS  --------------------

# Grafico contagios notificados diarios
ggplot(dtf_notificados, aes(fecha_notificacion, total_registros)) +
  geom_col(fill = "#C39BD3", alpha = 0.5) +
  theme_bw() +
  labs(title = "Gráfico de Casos notificados",
       x = "Fechas",
       y = "Numero de casos") +
  theme(axis.text.x = element_text(size = 7, angle = 90)) +
  theme(axis.text.y = element_text(size = 7))

# Grafico contagios notificados diarios tipo linea
ggplot(dtf_notificados, aes(fecha_notificacion, total_registros)) +
  geom_line(color = "#C39BD3") +
  theme_bw() +
  labs(title = "Gráfico de Casos notificados",
       x = "Fechas",
       y = "Número de casos") +
  theme(axis.text.x = element_text(size = 7, angle = 90)) +
  theme(axis.text.y = element_text(size = 7))

# Grafico acumulado de contagios diarios 
ggplot(dtf_notificados, aes(fecha_notificacion, registros_acumulados)) +
  geom_point(color = "#C39BD3", alpha = 0.6) +
  theme_bw() +
  labs(title = "Gráfico de casos acumulados",
       x = "Fechas",
       y = "Casos reportados") +
  theme(axis.text.x = element_text(size = 7, angle = 90)) +
  theme(axis.text.y = element_text(size = 7))

# Grafico boxplot
ggplot(dtf_notificados, aes(total_registros)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.shape = 10, outlier.size = 1) +
  theme_bw() +
  coord_flip() +
  scale_fill_brewer(palette = "Blues") +
  stat_boxplot(geom = "errorbar", width = 0.25, alpha = 0.5) +
  labs(title = "Gráfico de casos notificados",
       x = "Total registros",
       y = "") +
  theme(axis.text.x = element_text(size = 0))



# ---------------  ANALISIS DE NORMALIDAD dtf_notificados   --------------------

# Test no grafico: shapiro_wlk (n<50)
shapiro.test(dtf_notificados$total_registros)
# Resultado p-value < 2.2e-16.  Menor a 5% se asume que no corresponde a una distribucion normal

# Test no grafico: kolgomorov smirnov (n>=50)
lillie.test(dtf_notificados$total_registros)
# Resultado p-value < 2.2e-16.  Menor a 5% se asume que no corresponde a una distribucion normal

# Test grafico: cuantil - cuartil
qqnorm(dtf_notificados$total_registros)
qqline(dtf_notificados$total_registros)
# La grafica indica que no se comporta como una distribucion normal (los puntos no se distribuyen sobre la linea)

# Test grafico: crawley
crawley.plot(dtf_notificados$total_registros)
# La grafica resultado no muestra similitud con una distribucion normal




# ----------------------                                ----------------------
# ----------------------          VARIABLE mes          ----------------------
# ----------------------                                ----------------------

# Tipo de variable: numero - Numerica - discreta

# ----------------------          DATOS GENERALES        ----------------------

# Registros totales
dtf_casos %>% 
  summarise(total_registros = sum(!is.na(nromes))) %>% 
  View()

# Registros faltantes
dtf_casos %>% 
  summarise(total_registros = sum(is.na(nromes))) %>% 
  View()


# Registros en los ultimas 15 meses
dtf_casos %>% 
  group_by(año, mes) %>% 
  summarise(total_registros = sum(!is.na(nromes))) %>% 
  arrange(desc(año), desc(nromes)) %>% 
  head(15) %>% 
  View()

# 20 meses con mas registros
dtf_casos %>% 
  group_by(año, nromes) %>% 
  summarise(total_registros = sum(!is.na(nromes))) %>% 
  arrange(desc(total_registros)) %>% 
  head(20) %>% 
  View()


# 20 Semanas con los valores mas bajos
dtf_casos %>% 
  group_by(año, nromes) %>% 
  summarise(total_registros = sum(!is.na(nromes))) %>% 
  arrange(total_registros) %>% 
  head(20) %>% 
  View()


# 6 Meses con mayor numero de contagio
dtf_casos %>% 
  group_by(nromes) %>% 
  summarise(total_registros = sum(!is.na(nromes))) %>% 
  arrange(desc(total_registros)) %>%
  head(6) %>% 
  View()


# 6 Meses con menor numero de contagio
dtf_casos %>% 
  group_by(nromes) %>% 
  summarise(total_registros = sum(!is.na(nromes))) %>% 
  arrange(total_registros) %>%
  head(6) %>% 
  View()


# ---------------------   CREACION SET DATOS POR MES  ----------------------

# Creacion del set
dtf_mes <- dtf_casos %>% 
  select(frepor, fnotif, año, nromes) %>% 
  mutate(nbdia = 1)
dtf_mes <- (unite(dtf_mes, año_mes, año, nromes, nbdia, sep = "-"))
dtf_mes  <- dtf_mes %>% 
  mutate(añomes = as.Date(año_mes)) %>% 
  group_by(añomes) %>% 
  summarise(total_registros = sum(!is.na(añomes)))

# Adicion de columna de acumulados
dtf_mes <- dtf_mes %>% 
  mutate(registros_acumulados = cumsum(total_registros))

# Cambio de nombre de columna
dtf_mes <- dtf_mes %>%
  rename(fecha_notificacion = añomes) 
  


# --------- ESTADISTICOS DESCRIPTIVOS  SET DE DATOS MENSUALES  ---------------

dtf_mes %>% 
  summarise(minimo = min(total_registros),
            maximo = max(total_registros),
            rango = maximo - minimo,
            media = mean(total_registros),
            mediana = median(total_registros),
            desv_std = sd(total_registros),
            varianza = var(total_registros),
            perc25 = quantile(total_registros, 0.25),
            perc75 = quantile(total_registros, 0.75),
            rangoIC = perc75 - perc25,
            dcil1 = quantile(total_registros, 0.1),
            dcil9 = quantile(total_registros, 0.9)) %>% 
  View()



# ---------------  GRAFICOS SET DE DATOS DATOS MENSUALES  --------------------

# Grafico contagios notificados por mes
ggplot(dtf_mes, aes(fecha_notificacion, total_registros)) +
  geom_col(fill = "#C39BD3", alpha = 0.5) +
  theme_bw() +
  labs(title = "Gráfico de Casos notificados por mes",
       x = "Fechas",
       y = "Numero de casos") +
  theme(axis.text.x = element_text(size = 7, angle = 90)) +
  theme(axis.text.y = element_text(size = 7))

# Grafico contagios notificados por mes tipo linea
ggplot(dtf_mes, aes(fecha_notificacion, total_registros)) +
  geom_line(color = "#C39BD3") +
  theme_bw() +
  labs(title = "Gráfico de Casos notificados por mes",
       x = "Fechas",
       y = "Número de casos") +
  theme(axis.text.x = element_text(size = 7, angle = 90)) +
  theme(axis.text.y = element_text(size = 7))

# Grafico acumulado de contagios mensuales 
ggplot(dtf_mes, aes(fecha_notificacion, registros_acumulados)) +
  geom_point(color = "#C39BD3", alpha = 0.6) +
  theme_bw() +
  labs(title = "Gráfico de casos acumulados",
       x = "Fechas",
       y = "Casos reportados") +
  theme(axis.text.x = element_text(size = 7, angle = 90)) +
  theme(axis.text.y = element_text(size = 7))

# Grafico boxplot
ggplot(dtf_mes, aes(total_registros)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.shape = 10, outlier.size = 1) +
  theme_bw() +
  coord_flip() +
  scale_fill_brewer(palette = "Blues") +
  stat_boxplot(geom = "errorbar", width = 0.25, alpha = 0.5) +
  labs(title = "Gráfico de casos mensuales notificados",
       x = "Total registros",
       y = "") +
  theme(axis.text.x = element_text(size = 0))


# ----------  ANALISIS DE NORMALIDAD SET DE DATOS MENSUALES     ----------------

# Test no grafico: shapiro_wlk (n<50)
shapiro.test(dtf_mes$total_registros)
# Resultado p-value = 0.0003777.  Menor a 5% se asume que no corresponde a una distribucion normal

# Test no grafico: kolgomorov smirnov (n>=50)
lillie.test(dtf_mes$total_registros)
# Resultado p-value = 0.005199.  Menor a 5% se asume que no corresponde a una distribucion normal

# Test grafico: cuantil - cuartil
qqnorm(dtf_mes$total_registros)
qqline(dtf_mes$total_registros)
# La grafica indica que no se comporta como una distribucion normal (los puntos no se distribuyen sobre la linea)

# Test grafico: crawley
crawley.plot(dtf_mes$total_registros)
# La grafica resultado no muestra similitud con una distribucion normal



# # ----------------------                                ----------------------
# # ----------------------          VARIABLE nrosem       ----------------------
# # ----------------------                                ----------------------
 
# # ----------------------          DATOS GENERALES        ----------------------

# Tipo de variable: numero - Numerica - discreta

# Registros totales
dtf_casos %>%
  summarise(total_registros = sum(!is.na(nrosem))) %>%
  View()

# Registros faltantes
dtf_casos %>%
  summarise(total_registros = sum(is.na(nrosem))) %>%
  View()

# Registros en las ultimas 15 semanas
dtf_casos %>%
  group_by(año, nrosem) %>%
  summarise(total_registros = sum(!is.na(nrosem))) %>%
  arrange(desc(año), desc(nrosem)) %>%
  head(15) %>%
  ungroup() %>%
  View()

# 20 Semanas con los valores mas altos
dtf_casos %>%
  group_by(año, nrosem) %>%
  summarise(total_registros = sum(!is.na(nrosem))) %>%
  arrange(desc(total_registros)) %>%
  head(20) %>%
  View()


# 20 Semanas con los valores mas bajos
dtf_casos %>%
  group_by(año, nrosem) %>%
  summarise(total_registros = sum(!is.na(nrosem))) %>%
  arrange(total_registros) %>%
  head(20) %>%
  View()


# ---------------------   CREACION SET DATOS POR SEMANA  ----------------------

#Creacion set
dtf_semana <- dtf_casos %>% 
  group_by(año, nrosem) %>% 
  summarise(total_registros = sum(!is.na(nrosem))) %>% 
  mutate(año1 = año,
         dia1 = 0,
         sem1 = nrosem)
dtf_semana <- unite(dtf_semana, año_sem, año, nrosem, sep = "-")
dtf_semana <-unite(dtf_semana, tempsem, dia1, sem1,  sep = "")
dtf_semana <-unite(dtf_semana, año_sem1, año1, tempsem, sep = "-")
dtf_semana <- dtf_semana %>% 
  mutate(longitud = str_length(año_sem1)) %>% 
  mutate(fecha_semana = ifelse(longitud == 7, año_sem1, año_sem)) %>% 
  select(fecha_semana, total_registros)

# Adicion columna acumulados
dtf_semana <- dtf_semana %>% 
  mutate(registros_acumulados = cumsum(total_registros))



# --------- ESTADISTICOS DESCRIPTIVOS  SET DE DATOS SEMANALES  ---------------

dtf_semana %>% 
  summarise(minimo = min(total_registros),
            maximo = max(total_registros),
            rango = maximo - minimo,
            media = mean(total_registros),
            mediana = median(total_registros),
            desv_std = sd(total_registros),
            varianza = var(total_registros),
            perc25 = quantile(total_registros, 0.25),
            perc75 = quantile(total_registros, 0.75),
            rangoIC = perc75 - perc25,
            dcil1 = quantile(total_registros, 0.1),
            dcil9 = quantile(total_registros, 0.9)) %>% 
  View()



# ---------------  GRAFICOS SET DE DATOS DATOS SEMANALES  --------------------

# Grafico contagios notificados por semana
ggplot(dtf_semana, aes(fecha_semana, total_registros)) +
  geom_col(fill = "#C39BD3", alpha = 0.5) +
  theme_bw() +
  labs(title = "Gráfico de Casos notificados por semana",
       x = "Fechas",
       y = "Numero de casos") +
  theme(axis.text.x = element_text(size = 7, angle = 90)) +
  theme(axis.text.y = element_text(size = 7))

# Grafico acumulado de contagios semanales 
ggplot(dtf_semana, aes(fecha_semana, registros_acumulados)) +
  geom_point(color = "#C39BD3", alpha = 0.6) +
  theme_bw() +
  labs(title = "Gráfico de casos acumulados",
       x = "Fechas",
       y = "Casos reportados") +
  theme(axis.text.x = element_text(size = 7, angle = 90)) +
  theme(axis.text.y = element_text(size = 7))

# Grafico boxplot
ggplot(dtf_semana, aes(total_registros)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.shape = 10, outlier.size = 1) +
  theme_bw() +
  coord_flip() +
  scale_fill_brewer(palette = "Blues") +
  stat_boxplot(geom = "errorbar", width = 0.25, alpha = 0.5) +
  labs(title = "Gráfico de casos semanales notificados",
       x = "Total registros",
       y = "") +
  theme(axis.text.x = element_text(size = 0))


# ----------  ANALISIS DE NORMALIDAD SET DE DATOS SEMANALES     ----------------

# Test no grafico: shapiro_wlk (n<50)
shapiro.test(dtf_semana$total_registros)
# Resultado p-value = 1.878e-11.  Menor a 5% se asume que no corresponde a una distribucion normal

# Test no grafico: kolgomorov smirnov (n>=50)
lillie.test(dtf_semana$total_registros)
# Resultado p-value = 3.799e-11.  Menor a 5% se asume que no corresponde a una distribucion normal

# Test grafico: cuantil - cuartil
qqnorm(dtf_semana$total_registros)
qqline(dtf_semana$total_registros)
# La grafica indica que no se comporta como una distribucion normal (los puntos no se distribuyen sobre la linea)

# Test grafico: crawley
crawley.plot(dtf_semana$total_registros)
# La grafica resultado no muestra similitud con una distribucion normal



# # ----------------------                                ----------------------
# # ----------------------          VARIABLE diasem       ----------------------
# # ----------------------                                ----------------------

# # ----------------------          DATOS GENERALES       ----------------------

# Tipo de variable: Caracter - Categorica - Nominal

# Registros totales
dtf_casos %>%
  summarise(total_registros = sum(!is.na(diasem))) %>%
  View()

# Registros faltantes
dtf_casos %>%
  summarise(total_registros = sum(is.na(diasem))) %>%
  View()

# Registros en las ultimas 15 dias
dtf_casos %>%
  group_by(fnotif, diasem) %>%
  summarise(total_registros = sum(!is.na(diasem))) %>%
  arrange(desc(fnotif), desc(diasem)) %>%
  head(15) %>%
  ungroup() %>%
  View()

# 20 dias con los valores mas altos
dtf_casos %>%
  group_by(fnotif, diasem) %>%
  summarise(total_registros = sum(!is.na(diasem))) %>%
  arrange(desc(total_registros)) %>%
  head(20) %>%
  View()

# 20 dias con los valores mas bajos
dtf_casos %>%
  group_by(fnotif, diasem) %>%
  summarise(total_registros = sum(!is.na(diasem))) %>%
  arrange(total_registros) %>%
  head(20) %>%
  View()

# Dia de la semana con mas casas
dtf_casos %>% 
  group_by(diasem) %>% 
  summarise(total_registros = sum(!is.na(diasem))) %>% 
  arrange(desc(total_registros)) %>% 
  View()


# --------------     CREACION SET DATOS POR DIA DE LA SEMANA     ---------------

# Creacion data set
dtf_dia <- dtf_casos %>% 
  group_by(fnotif, diasem) %>% 
  summarise(total_registros = sum(!is.na(diasem)))

# Adicionar la columna acumulados
dtf_dia <- dtf_dia %>% 
  mutate(registros_acumulados = cumsum(total_registros))


# ---------     ESTADISTICOS DESCRIPTIVOS POR DIA DE LA SEMANA      ------------

# Estadistico descritivo para el dia lunes
dtf_dia %>% 
  filter(diasem == "Mon") %>% 
  group_by(diasem) %>% 
  summarise(minimo = min(total_registros),
            maximo = max(total_registros),
            rango = maximo - minimo,
            media = mean(total_registros),
            mediana = median(total_registros),
            #moda = mfv(total_registros),
            desv_std = sd(total_registros),
            varianza = var(total_registros),
            perct25 = quantile(total_registros, 0.25),
            perct75 = quantile(total_registros, 0.75),
            rangoIC = perct75 - perct25,
            decil1 = quantile(total_registros, 0.1),
            decil9 = quantile(total_registros, 0.9)) %>% 
  View()


# Estadistico descritivo para el dia martes
dtf_dia %>% 
  filter(diasem == "Tue") %>% 
  group_by(diasem) %>% 
  summarise(minimo = min(total_registros),
            maximo = max(total_registros),
            rango = maximo - minimo,
            media = mean(total_registros),
            mediana = median(total_registros),
            #moda = mfv(total_registros),
            desv_std = sd(total_registros),
            varianza = var(total_registros),
            perct25 = quantile(total_registros, 0.25),
            perct75 = quantile(total_registros, 0.75),
            rangoIC = perct75 - perct25,
            decil1 = quantile(total_registros, 0.1),
            decil9 = quantile(total_registros, 0.9)) %>% 
  View()

# Estadistico descritivo para el dia miercoles
dtf_dia %>% 
  filter(diasem == "Wed") %>% 
  group_by(diasem) %>% 
  summarise(minimo = min(total_registros),
            maximo = max(total_registros),
            rango = maximo - minimo,
            media = mean(total_registros),
            mediana = median(total_registros),
            #moda = mfv(total_registros),
            desv_std = sd(total_registros),
            varianza = var(total_registros),
            perct25 = quantile(total_registros, 0.25),
            perct75 = quantile(total_registros, 0.75),
            rangoIC = perct75 - perct25,
            decil1 = quantile(total_registros, 0.1),
            decil9 = quantile(total_registros, 0.9)) %>% 
  View()


# Estadistico descritivo para el dia jueves
dtf_dia %>% 
  filter(diasem == "Thu") %>% 
  group_by(diasem) %>% 
  summarise(minimo = min(total_registros),
            maximo = max(total_registros),
            rango = maximo - minimo,
            media = mean(total_registros),
            mediana = median(total_registros),
            #moda = mfv(total_registros),
            desv_std = sd(total_registros),
            varianza = var(total_registros),
            perct25 = quantile(total_registros, 0.25),
            perct75 = quantile(total_registros, 0.75),
            rangoIC = perct75 - perct25,
            decil1 = quantile(total_registros, 0.1),
            decil9 = quantile(total_registros, 0.9)) %>% 
  View()

# Estadistico descritivo para el dia viernes
dtf_dia %>% 
  filter(diasem == "Fri") %>% 
  group_by(diasem) %>% 
  summarise(minimo = min(total_registros),
            maximo = max(total_registros),
            rango = maximo - minimo,
            media = mean(total_registros),
            mediana = median(total_registros),
            #moda = mfv(total_registros),
            desv_std = sd(total_registros),
            varianza = var(total_registros),
            perct25 = quantile(total_registros, 0.25),
            perct75 = quantile(total_registros, 0.75),
            rangoIC = perct75 - perct25,
            decil1 = quantile(total_registros, 0.1),
            decil9 = quantile(total_registros, 0.9)) %>% 
  View()

# Estadistico descritivo para el dia viernes
dtf_dia %>% 
  filter(diasem == "Sat") %>% 
  group_by(diasem) %>% 
  summarise(minimo = min(total_registros),
            maximo = max(total_registros),
            rango = maximo - minimo,
            media = mean(total_registros),
            mediana = median(total_registros),
            #moda = mfv(total_registros),
            desv_std = sd(total_registros),
            varianza = var(total_registros),
            perct25 = quantile(total_registros, 0.25),
            perct75 = quantile(total_registros, 0.75),
            rangoIC = perct75 - perct25,
            decil1 = quantile(total_registros, 0.1),
            decil9 = quantile(total_registros, 0.9)) %>% 
  View()

# Estadistico descritivo para el dia viernes
dtf_dia %>% 
  filter(diasem == "Sun") %>% 
  group_by(diasem) %>% 
  summarise(minimo = min(total_registros),
            maximo = max(total_registros),
            rango = maximo - minimo,
            media = mean(total_registros),
            mediana = median(total_registros),
            #moda = mfv(total_registros),
            desv_std = sd(total_registros),
            varianza = var(total_registros),
            perct25 = quantile(total_registros, 0.25),
            perct75 = quantile(total_registros, 0.75),
            rangoIC = perct75 - perct25,
            decil1 = quantile(total_registros, 0.1),
            decil9 = quantile(total_registros, 0.9)) %>% 
  View()


# ----------------      GRAFICOS POR DIA DE LA SEMANA   -   --------------------

# Grafico contagios notificados por dia de la semana
ggplot(dtf_dia, aes(diasem, total_registros)) +
  geom_col(fill = "#C39BD3", alpha = 0.5) +
  theme_bw() +
  labs(title = "Gráfico de Casos notificados por dia de la semana",
       x = "Dia de la semana",
       y = "Numero de casos") +
  theme(axis.text.x = element_text(size = 7, angle = 90)) +
  theme(axis.text.y = element_text(size = 7))

# Grafico acumulado de contagios semanales 
ggplot(dtf_dia, aes(diasem, registros_acumulados)) +
  geom_point(color = "#C39BD3", alpha = 0.6) +
  theme_bw() +
  labs(title = "Gráfico de casos acumulados",
       x = "Fechas",
       y = "Casos reportados") +
  theme(axis.text.x = element_text(size = 7, angle = 90)) +
  theme(axis.text.y = element_text(size = 7))

# Grafico genral boxplot
ggplot(dtf_dia, aes(total_registros)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.shape = 10, outlier.size = 1) +
  theme_bw() +
  coord_flip() +
  scale_fill_brewer(palette = "Dark2") +
  #stat_boxplot(geom = "errorbar", width = 0.25, alpha = 0.5) +
  labs(title = "Gráfico general de contagios",
       x = "Total registros",
       y = "") +
  theme(axis.text.x = element_text(size = 0))

# Grafico genral boxplot por dia de la semana
ggplot(dtf_dia, aes(total_registros, fill = diasem)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.shape = 10, outlier.size = 1) +
  theme_bw() +
  coord_flip() +
  scale_fill_brewer(palette = "Blues") +
  #stat_boxplot(geom = "errorbar", width = 0.25, alpha = 0.5) +
  labs(title = "Gráfico de contagios por dia de la semana",
       x = "Total registros",
       y = "") +
  theme(axis.text.x = element_text(size = 0))






# # ----------------------            ANALISIS            ----------------------
# # ----------------------            PRIMEROS            ----------------------
# # ----------------------             CASOS              ----------------------



# ------------------     SET DE DATOS CASOS INICIALES       --------------------

# Creacion del set
dtf_casos_iniciales <- dtf_casos %>% 
  filter(fnotif >="2020-03-02" & fnotif <= "2020-03-22") %>% 
  group_by(fnotif) %>% 
  summarise(total_registros = sum(!is.na(fnotif)))

# Adicion de la columna acumulados
dtf_casos_iniciales <- dtf_casos_iniciales %>% 
  mutate(registros_acumulados = cumsum(total_registros))


# ----------     ESTADISTICOS DESCRIPTIVOS CASOS INICIALES       ---------------

dtf_casos_iniciales %>% 
  summarise(fecha_minima = min(fnotif),
            fecha_maxima = max(fnotif),
            dias_registrados = fecha_maxima - fecha_minima,
            nro_registros = sum(!is.na((total_registros))),
            minimo = min(total_registros),
            maximo = max(total_registros),
            rango = maximo - minimo,
            media = mean(total_registros),
            mediana = median(total_registros),
            desv_std = sd(total_registros),
            varianza = var(total_registros),
            perc25 = quantile(total_registros, 0.25),
            perc75 = quantile(total_registros, 0.75),
            rangoIC = perc75 - perc25,
            dcil1 = quantile(total_registros, 0.1),
            dcil9 = quantile(total_registros, 0.9)) %>% 
  View()


# -------------     GRAFICOS SET DE DATOS CASOS INICIALES     ------------------

# Grafico contagios notificados iniciales
ggplot(dtf_casos_iniciales, aes(fnotif, total_registros)) +
  geom_col(fill = "#C39BD3", alpha = 0.5) +
  theme_bw() +
  labs(title = "Gráfico de Casos notificados diarios",
       x = "Dia de la semana",
       y = "Numero de casos") +
  theme(axis.text.x = element_text(size = 7, angle = 90)) +
  theme(axis.text.y = element_text(size = 7))

# Grafico acumulado de contagios iniciales 
ggplot(dtf_casos_iniciales, aes(fnotif, registros_acumulados)) +
  geom_point(color = "#C39BD3", alpha = 0.6) +
  theme_bw() +
  labs(title = "Gráfico de casos acumulados",
       x = "Fechas",
       y = "Casos reportados") +
  theme(axis.text.x = element_text(size = 7, angle = 90)) +
  theme(axis.text.y = element_text(size = 7))
