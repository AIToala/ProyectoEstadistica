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

raw_data <- read_excel("Base2DatosEstudiantes.xlsx")
str(raw_data)

# Selección de datos a analizar.
data = subset(raw_data, select = c("Matricula","Sexo","Trabaja","Computador_uso_exclusivo","Despierta_mas_de_1vez_durante_noche","Carrera","Promedio","Materias_Promedio_x_Termino","Frecuencia_semanal_actividad_fisica","Horas_promedio_diarias_sueño","Horas_promedio_diarias_estudio","Horas_promedio_diarias_en_redes") )
# Estructura de datos con problemas.
str(data)

# Reestructuración de datos cualitativos
data[,c("Trabaja","Computador_uso_exclusivo","Despierta_mas_de_1vez_durante_noche","Frecuencia_semanal_actividad_fisica")]<-lapply(data[,c("Trabaja","Computador_uso_exclusivo","Despierta_mas_de_1vez_durante_noche","Frecuencia_semanal_actividad_fisica")] , as.character)

# Estructura de datos solucionado.
str(data)

# Solucion a datos cualitativos con valores no respondidos durante encuesta.
data$Trabaja<-ifelse(data$Trabaja=="0", "NO INFO", data$Trabaja)
data$Despierta_mas_de_1vez_durante_noche<-ifelse(data$Despierta_mas_de_1vez_durante_noche=="0", "NO INFO", data$Despierta_mas_de_1vez_durante_noche)
data$Carrera<-chartr("ÁÉÍÓÚ", "AEIOU", toupper(data$Carrera))
data$Carrera<-ifelse(data$Carrera=="TELECO","TELECOMUNICACIONES",data$Carrera)
data$Frecuencia_semanal_actividad_fisica<-ifelse(data$Frecuencia_semanal_actividad_fisica=="5","3",data$Frecuencia_semanal_actividad_fisica)

# Estadistica descriptiva univariante
# Variables Cualitativas
# Sexo
sex_bd<- ggplot(data) + aes(x = Sexo, fill = Sexo) +
  geom_bar() + 
  labs(title ="Diagrama de Barra de Sexo\n", x="Sexo", y="Frecuencia") +
  geom_text(stat='count', aes(label=..count..), vjust=1)

sex_bd

# Trabaja
works_bd<- ggplot(data) + aes(x = Trabaja, fill = Trabaja) +
  geom_bar() + 
  labs(title ="Diagrama de Barra de Estudiantes que trabajan\n", x="Trabaja", y="Frecuencia") +
  geom_text(stat='count', aes(label=..count..), vjust=1)

works_bd

# Computador USO EXCLUSIVO
computador_bd<- ggplot(data) + aes(x = Computador_uso_exclusivo, fill = Computador_uso_exclusivo) +
  geom_bar() + 
  labs(title ="Diagrama de Barra de Estudiantes con computador de uso exclusivo\n", x="Computador uso exclusivo", y="Frecuencia") +
  geom_text(stat='count', aes(label=..count..), vjust=1)

computador_bd

# Despierta mas de 1 vez durante noche
despNoche_bd<- ggplot(data) + aes(x = Despierta_mas_de_1vez_durante_noche, fill = Despierta_mas_de_1vez_durante_noche) +
  geom_bar() + 
  labs(title ="Diagrama de Barra de Estudiantes que despiertan mas de 1 vez durante la noche\n", x="Despierta mas de 1 vez durante noche", y="Frecuencia") +
  geom_text(stat='count', aes(label=..count..), vjust=1)

despNoche_bd

# Carrera

carrera_bd<- ggplot(data) + aes(x = Carrera, fill = Carrera) +
  geom_bar() + 
  labs(title ="Diagrama de Barra de Carreras seguidas por Estudiantes\n", x="Carreras", y="Frecuencia") +
  geom_text(stat='count', aes(label=..count..), vjust=1)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank() 
  )

carrera_bd

# Frecuencia Semanal Actividad Fisica

frec_act_fis_bd<- ggplot(data) + aes(x = Frecuencia_semanal_actividad_fisica, fill = Frecuencia_semanal_actividad_fisica) +
  geom_bar() + 
  labs(title ="Diagrama de Barra de Frecuencia Semanal de Actividad Fisica de Estudiantes\n", x="Frecuencia semanal de actividad fisica", y="Frecuencia") +
  geom_text(stat='count', aes(label=..count..), vjust=1)

frec_act_fis_bd

#Variables Cuantitativas
# Promedio
#Medidas de promedio
summary(data$Promedio)
sd(data$Promedio)
kurtosis(data$Promedio)
skewness(data$Promedio)

#Tabla de frecuencia de promedio
promedio <- fdt(data$Promedio,
    breaks=c('Sturges'),
    right=FALSE,
    na.rm=FALSE)

promedio_table<- promedio$table
as_tibble(promedio_table)
colnames(promedio_table)<-c('Clase','Frec. Abs.','Frec. Rel.','Frec. Rel. Porc.','Frec. Abs. Acum.','Frec. Abs. Acum. Porc.')

j1 <- c(3:4, 6)
promedio_table[j1] <- lapply(promedio_table[j1], function(x) sub(".", ",", sprintf("%.2f", x), fixed = TRUE))

promedio_table

# Materias_Promedio X TERMINO
#Medidas de Materias_Promedio X TERMINO
summary(data$Materias_Promedio_x_Termino)
sd(data$Materias_Promedio_x_Termino)
kurtosis(data$Materias_Promedio_x_Termino)
skewness(data$Materias_Promedio_x_Termino)

#Tabla de frecuencia de promedio
mat_promedio_term <- factor(data$Materias_Promedio_x_Termino)

mat_promedio_term_table<- mat_promedio_term$table
as_tibble(mat_promedio_term_table)
colnames(mat_promedio_term_table)<-c('Clase','Frec. Abs.','Frec. Rel.','Frec. Rel. Porc.','Frec. Abs. Acum.','Frec. Abs. Acum. Porc.')

mat_promedio_term_table[j1] <- lapply(mat_promedio_term_table[j1], function(x) sub(".", ",", sprintf("%.2f", x), fixed = TRUE))

mat_promedio_term_table
















