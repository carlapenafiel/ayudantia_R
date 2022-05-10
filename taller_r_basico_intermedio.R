####################################################################################################
#                             Limpiar espacio de trabajo
####################################################################################################

rm(list=ls())         # Limpia la lista de objetos o espacio de trabajo
graphics.off()        # Limpia la lista de gráficos

####################################################################################################
#                             Librerías a utilizar y setear directorio
####################################################################################################
library(readxl)
library(tidyr)
library(foreign)
library(dplyr)
library(haven)
library(ggplot2) 
library(gmodels)
library(Hmisc)
library(ggthemes)
library(tidyverse)
library(reshape)
library(reshape2)
library(plyr)
library(dbplyr)
library(kableExtra)
library(knitr)
library("writexl")
library(lubridate)

#Setear tu directorio
dir <- setwd("/Users/58994/Desktop/Ayudantia_R")

####################################################################################################
#                             R principio
####################################################################################################

#Instalar paquetes
#install.packages("nombre_paquete")

#Objeto
n <- 14
n
p <- "Hola!"
p
q <- 5
n+q

#Vector
v <- c(1,2,3,4,5)
v
v >= 4

#Funciones
#Suma de dos numeros 
suma <- function(x, y) {
  resultado <- x + y
  return(resultado)
}

suma(4,5)

#OJO: Es bueno crear esto en un file aparte que contenga todas las funciones y luego correrlo al inicio del código
#Correr un otro file: source("Ruta/functions.R")

####################################################################################################
#                             Trabajo datos de denuncias
####################################################################################################

##Cargamos BD a utilizar
df <- read_xlsx("reg_maltrato_2005_2021.xlsx")

#Miramos los datos
head(df)
summary(df)
#Promedio de menores de edad
prom = mean(df$cantidad_menores, na.rm = TRUE)
print('Promedio cantidad de menores de edad:') 
print(prom)

#Haremos un subset con las variables que ocuparemos
df_subset <- subset(df, select = c("region", "Comuna", "Anho", "Frecuencia"))
#Para los análisis miraremos la RM
df_filter <- filter(df_subset, region == 13)

#Miramos el df a nivel comunal
plot(df_filter$Anho, df_filter$Frecuencia)

#Nos aseguramos que cada dato sea del tipo correcto
df_filter$Anho <- as.numeric(df_filter$Anho)
df_filter$tasa_mil_n <- as.numeric(df_filter$Frecuencia)

#Supongamos que quiero calcular la suma de denuncias para una comuna a lo largo de los años.
suma_denuncias <- aggregate(Frecuencia ~ Comuna, df_filter, sum, na.rm = FALSE)

#Gráficos: GGPLOT
ggplot(df_filter, aes(Anho, Frecuencia)) + geom_point(colour = "blue")
ggplot(df_filter, aes(Anho, Frecuencia)) + geom_point(aes(colour = factor(Comuna)))
ggplot(df_filter, aes(x = Anho, y = Frecuencia, color = Comuna)) + geom_line()

#Gráficos interactivos
library(plotly)
library(gapminder)
library(gganimate)

#Gráfico de puntos de comunas de la RM
plot_ly(data = df_filter, x = ~ Anho, y = ~ Frecuencia, color = ~ Comuna)

#Miramos comunas en particular
#Filtrar: O1
puente_nunoa <- filter(df_filter, Comuna == "PUENTE ALTO" | Comuna == "ÑUÑOA")
#Filtrar: O2 (Una alternativa)
ejemplo <- df_filter[df_filter$Comuna == "PUENTE ALTO",]

#Gráfico interactivo de la frecuencia de denuncias por año según comuna
fig <- plot_ly(puente_nunoa, x = ~Anho, y = ~Frecuencia, color = ~ Comuna)
fig

#Veamos el gráfico con la suma de denuncias por comuna a lo largo de los años
fig_2 <- plot_ly(suma_denuncias, x = ~Comuna, y = ~Frecuencia)
fig_2

####################################################################################################
#                             Trabajo con datos de empleo
####################################################################################################

#Cargamos los datos de empleo por año
desempleo <- read_xlsx("tasa_anual_desempleo_chile.xlsx")

#Miramos la BD
summary(desempleo)

#Cambiamos el nombre de la variable Año para el merge.
desempleo<- dplyr:: rename(desempleo, Anho= Año)
desempleo<- dplyr:: rename(desempleo, tasa_desempleo=`Tasa desempleo`)

#Pegamos el desempleo a la tasa de denuncias de M.
merge_dd <- merge(x = df_filter, y =desempleo, by = c("Anho"), all.x = TRUE, all.y = FALSE)

####################################################################################################
#                      Trabajo con datos de empleo y denuncias
####################################################################################################

#Queremos ver cómo se comporta el desempleo en comparación con la tasa de denuncias 
cor(merge_dd$Frecuencia, merge_dd$tasa_desempleo, use="complete.obs")

#No tomaremos en cuenta los datos anteriores al 2010
dd_filter <- filter(merge_dd, Anho >= 2010)

#Regresión OLS
reg1 <- lm(Frecuencia ~ tasa_desempleo, data = dd_filter)
summary(reg1)

#Regresión de efecto fijo por año
fatal_fe_lm_mod <- lm(Frecuencia ~ tasa_desempleo + Anho - 1, data = dd_filter)
summary(fatal_fe_lm_mod)

#Regresión de efecto fijo por comuna
fixed.dum <-lm(Frecuencia ~ tasa_desempleo + factor(Comuna) - 1, data = dd_filter)
summary(fixed.dum)

#Supongamos que queremos agregar una variable extra en nuestro análisis: Una dummy que indique 1 si el año es mayor o igual al 2017
dd_v2 <- dd_filter %>%
  mutate(
    ley_proteccion = case_when(Anho == 2010 ~ 0,
                    Anho  == 2011 ~ 0,
                    Anho == 2012 ~ 0,
                    Anho  == 2013 ~ 0,
                    Anho  == 2014 ~ 0,
                    Anho  == 2015 ~ 0,
                    Anho == 2016 ~ 0,
                    Anho  == 2017 ~ 1,
                    Anho  == 2018 ~ 1,
                    Anho == 2019 ~ 1,
                    Anho  == 2020 ~ 1,
                    Anho  == 2021 ~ 1)
  )

#Corremos una regresion OLS nuevamente, agregando la nueva variable ley_proteccion
reg2 <- lm(Frecuencia ~ tasa_desempleo + ley_proteccion, data = dd_v2)
summary(reg2)


