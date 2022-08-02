# Loading
library("readxl")
library(tibbletime)
library(dplyr)
library(tidyverse)

# xls files
consumo <- read_excel("Consumo.xlsx")

#Resumen general
summary(consumo)

#### Fecha: Cuantitativa continua
#### Gasolina superior: Cuantitativa continua
#### Gasolina regular: Cuantitativa continua
#### Diesel alto azufre: Cuantitativa continua

#Gráficos
plot(consumo$Fecha)
hist(x = consumo$`Gasolina superior`)
hist(x = consumo$`Gasolina regular`)
hist(x = consumo$`Diesel alto azufre`)

## Correlación de las variables numéricas
df1<-data.frame(consumo$`Gasolina superior`, consumo$`Gasolina regular`, consumo$`Diesel alto azufre`)
corrplot::corrplot(cor(df1))

###  1. ¿Cuáles son las 10 fechas que más consumo registraron de diesel? <br/>

consumo[,'']
consumo[,'']

fecha<-consumo[,'']
diesel<-consumo[,'']
q1<-consumo[,'']
q1<-data.frame()
