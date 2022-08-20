# PROYECTO ESTADISTICA GRUPO 2
# INTEGRANTES: Darien Bustos, Yairis Cordova, Steven Escobar, Jacobo Guerrero, Elkin Ramirez, Andres Toala

# Instalación de paquetes a usar (procurar no instalar dos veces para que no exista problemas de carga)

install.packages("readxl")
install.packages("dplyr")
install.packages("fdth")
install.packages("ggcorrplot")
install.packages("moments")
install.packages("GGally")
install.packages("nortest")

# Carga de librerias.

library("readxl") 		#Excel to dataframes
library("dplyr")  		#Dataframes management
library("fdth")			  #Tablas de frec, hist y poligonos
library("ggcorrplot")	#Correlation matrix on ggplot2
library("moments")		#Kurtosis
library("GGally")		  #Mas funciones para ggplot2
library("nortest")		#Tests de normalidad
library("ggplot2")		#Graficos

###Revisar si existen mejores alternativas.

# Carga inicial de datos a analizar.

data <- read_excel("Datos proyecto.xlsx")
data
str(data)
summary(data)

