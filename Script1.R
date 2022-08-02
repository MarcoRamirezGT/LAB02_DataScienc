
# Loading
library("readxl")
# xls files
importacion <- read_excel("importacion.xlsx")
View(importacion)
str(importacion)
importacion$Fecha<-as.Date(importacion$Fecha, "%Y/%m/%d")


nrow(importacion)
ncol(importacion)


#Que mes fue el que mas tuvo mas importacion 
superior <- importacion[order(importacion$`Gasolina superior`),]
superior<- head(importacion,10)
superior<- 
View(superior)

