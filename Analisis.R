# Activacion de librerias
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(nortest)
library(modeest)




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

