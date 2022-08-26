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
install.packages("plotly")
install.packages("summarytools")

# Carga de librerias.

library("readxl") 		#Excel to dataframes
library("dplyr")  		#Dataframes management
library("fdth")			  #Tablas de frec, hist y poligonos
library("ggcorrplot")	#Correlation matrix on ggplot2
library("moments")		#Kurtosis
library("GGally")		  #Mas funciones para ggplot2
library("nortest")		#Tests de normalidad
library("ggplot2")		#Graficos
library("plotly")


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

ggplotly(sex_bd)


# Trabaja
works_bd<- ggplot(data) + aes(x = Trabaja, fill = Trabaja) +
  geom_bar() + 
  labs(title ="Diagrama de Barra de Estudiantes que trabajan\n", x="Trabaja", y="Frecuencia") +
  geom_text(stat='count', aes(label=..count..), vjust=1)

ggplotly(works_bd)

# Computador USO EXCLUSIVO
computador_bd<- ggplot(data) + aes(x = Computador_uso_exclusivo, fill = Computador_uso_exclusivo) +
  geom_bar() + 
  labs(title ="Diagrama de Barra de Estudiantes con computador de uso exclusivo\n", x="Computador uso exclusivo", y="Frecuencia") +
  geom_text(stat='count', aes(label=..count..), vjust=1)

ggplotly(computador_bd)

# Despierta mas de 1 vez durante noche
despNoche_bd<- ggplot(data) + aes(x = Despierta_mas_de_1vez_durante_noche, fill = Despierta_mas_de_1vez_durante_noche) +
  geom_bar() + 
  labs(title ="Diagrama de Barra de Estudiantes que despiertan mas de 1 vez durante la noche\n", x="Despierta mas de 1 vez durante noche", y="Frecuencia") +
  geom_text(stat='count', aes(label=..count..), vjust=1)

ggplotly(despNoche_bd)

# Carrera

carrera_bd<- ggplot(data) + aes(x = Carrera, fill = Carrera) +
  geom_bar() + 
  labs(title ="Diagrama de Barra de Carreras seguidas por Estudiantes\n", x="Carreras", y="Frecuencia") +
  geom_text(stat='count', aes(label=..count..), vjust=1)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank() 
  )

ggplotly(carrera_bd)

# Frecuencia Semanal Actividad Fisica

frec_act_fis_bd<- ggplot(data) + aes(x = Frecuencia_semanal_actividad_fisica, fill = Frecuencia_semanal_actividad_fisica) +
  geom_bar() + 
  labs(title ="Diagrama de Barra de Frecuencia Semanal de Actividad Fisica de Estudiantes\n", x="Frecuencia semanal de actividad fisica", y="Frecuencia") +
  geom_text(stat='count', aes(label=..count..), vjust=1)

ggplotly(frec_act_fis_bd)

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
promedio_table <- promedio$table
#Histograma de promedio
prom_hist<-ggplot(promedio_table, aes(factor(`Class limits`), `f`, fill = `Class limits`)) + 
  geom_col()+
  labs(title ="Histograma de Promedios\n", x="Promedios", y="Frecuencia") +
  geom_text(aes(label = `f`), vjust = 1)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
ggplotly(prom_hist)
#Diagrama de cajas de promedio
prom_boxplot<-ggplot(data, aes(x="",y=Promedio))+
  geom_boxplot(fill = 2)+
  labs(title ="Diagrama de cajas de Promedios\n")+
  stat_boxplot(geom = "errorbar",
               width = 0.15)
prom_boxplot
ggplotly(prom_boxplot)

# Materias_Promedio X TERMINO
#Medidas de Materias_Promedio X TERMINO
summary(data$Materias_Promedio_x_Termino)
sd(data$Materias_Promedio_x_Termino)
kurtosis(data$Materias_Promedio_x_Termino)
skewness(data$Materias_Promedio_x_Termino)

#Tabla de frecuencia de promedio
mat_promedio_term_table <- as.data.frame(table(data$Materias_Promedio_x_Termino))

mat_promedio_term_table$'Frec. Rel.'<-mat_promedio_term_table$Freq/sum(mat_promedio_term_table$Freq)
mat_promedio_term_table$'Frec. Rel. Porc'<-mat_promedio_term_table$'Frec. Rel.'*100
mat_promedio_term_table$'Frec. Abs. Acum.'<-cumsum(mat_promedio_term_table$Freq)
mat_promedio_term_table$'Frec. Rel. Acum. Porc.'<-cumsum(mat_promedio_term_table$`Frec. Rel. Porc`)

colnames(mat_promedio_term_table)<-c('Mat. Promedio','Frec. Abs.','Frec. Rel.','Frec. Rel. Porc.','Frec. Abs. Acum.','Frec. Rel. Acum. Porc.')
mat_promedio_term_table <- as.data.frame(mat_promedio_term_table)

j1 <- c(3:4, 6)

mat_promedio_term_table[j1] <- lapply(mat_promedio_term_table[j1], function(x) sub(".", ",", sprintf("%.2f", x), fixed = TRUE))

mat_promedio_term_table

#Histograma de Materias promedio por termino
mat_prom_hist<-ggplot(data, aes(x=Materias_Promedio_x_Termino)) + 
  geom_histogram(bins=n_distinct(data$Materias_Promedio_x_Termino))+
  labs(title ="Histograma de Materias promedio por termino\n", x="Materias Promedio", y="Frecuencia")+
  geom_text(stat='count', aes(label=..count..), vjust=1)
  
ggplotly(mat_prom_hist)

#Diagrama de cajas de Materias promedio por termino
mat_prom_boxplot<-ggplot(data, aes(x="",y=Materias_Promedio_x_Termino))+
  geom_boxplot(fill = 2)+
  labs(title ="Diagrama de cajas de Materias Promedio por termino\n")+
  stat_boxplot(geom = "errorbar",
               width = 0.15)
mat_prom_boxplot
ggplotly(mat_prom_boxplot)

#Horas_promedio_diarias_sueño      
#Medidas de Horas_promedio_diarias_sueño
summary(data$Horas_promedio_diarias_sueño)
sd(data$Horas_promedio_diarias_sueño)
kurtosis(data$Horas_promedio_diarias_sueño)
skewness(data$Horas_promedio_diarias_sueño)

#Tabla de frecuencia de Horas_promedio_diarias_sueño
horas_sueño_table <- as.data.frame(table(data$Horas_promedio_diarias_sueño))

horas_sueño_table$'Frec. Rel.'<-horas_sueño_table$Freq/sum(horas_sueño_table$Freq)
horas_sueño_table$'Frec. Rel. Porc'<-horas_sueño_table$'Frec. Rel.'*100
horas_sueño_table$'Frec. Abs. Acum.'<-cumsum(horas_sueño_table$Freq)
horas_sueño_table$'Frec. Rel. Acum. Porc.'<-cumsum(horas_sueño_table$`Frec. Rel. Porc`)

colnames(horas_sueño_table)<-c('Horas Prom. Sueño','Frec. Abs.','Frec. Rel.','Frec. Rel. Porc.','Frec. Abs. Acum.','Frec. Rel. Acum. Porc.')
horas_sueño_table <- as.data.frame(horas_sueño_table)

j1 <- c(3:4, 6)

horas_sueño_table[j1] <- lapply(horas_sueño_table[j1], function(x) sub(".", ",", sprintf("%.2f", x), fixed = TRUE))

horas_sueño_table

#Histograma de Horas_promedio_diarias_sueño
sueño_prom_hist<-ggplot(data, aes(x=Horas_promedio_diarias_sueño)) + 
  geom_histogram(bins=n_distinct(data$Horas_promedio_diarias_sueño))+
  labs(title ="Histograma de Horas promedio de sueño\n", x="Horas Promedio", y="Frecuencia")+
  geom_text(stat='count', aes(label=..count..), vjust=1)

ggplotly(sueño_prom_hist)

#Diagrama de cajas de Horas_promedio_diarias_sueño
sueño_prom_boxplot<-ggplot(data, aes(x="",y=Horas_promedio_diarias_sueño))+
  geom_boxplot(fill = 2)+
  labs(title ="Diagrama de cajas de Horas Promedio de sueño\n",y="Horas promedio")+
  stat_boxplot(geom = "errorbar",
               width = 0.15)
sueño_prom_boxplot
ggplotly(sueño_prom_boxplot)

#Horas_promedio_diarias_estudio      
#Medidas de Horas_promedio_diarias_estudio
summary(data$Horas_promedio_diarias_estudio)
sd(data$Horas_promedio_diarias_estudio)
kurtosis(data$Horas_promedio_diarias_estudio)
skewness(data$Horas_promedio_diarias_estudio)

#Tabla de frecuencia de Horas_promedio_diarias_estudio
horas_estudio_table <- as.data.frame(table(data$Horas_promedio_diarias_estudio))

horas_estudio_table$'Frec. Rel.'<-horas_estudio_table$Freq/sum(horas_estudio_table$Freq)
horas_estudio_table$'Frec. Rel. Porc'<-horas_estudio_table$'Frec. Rel.'*100
horas_estudio_table$'Frec. Abs. Acum.'<-cumsum(horas_estudio_table$Freq)
horas_estudio_table$'Frec. Rel. Acum. Porc.'<-cumsum(horas_estudio_table$`Frec. Rel. Porc`)

colnames(horas_estudio_table)<-c('Horas Prom. Sueño','Frec. Abs.','Frec. Rel.','Frec. Rel. Porc.','Frec. Abs. Acum.','Frec. Rel. Acum. Porc.')
horas_estudio_table <- as.data.frame(horas_estudio_table)

j1 <- c(3:4, 6)

horas_estudio_table[j1] <- lapply(horas_estudio_table[j1], function(x) sub(".", ",", sprintf("%.2f", x), fixed = TRUE))

horas_estudio_table

#Histograma de Horas_promedio_diarias_estudio
estudio_prom_hist<-ggplot(data, aes(x=Horas_promedio_diarias_estudio)) + 
  geom_histogram(bins=n_distinct(data$Horas_promedio_diarias_estudio),binwidth = 1)+
  labs(title ="Histograma de Horas promedio de estudio\n", x="Horas Promedio", y="Frecuencia")

ggplotly(estudio_prom_hist)

#Diagrama de cajas de Horas_promedio_diarias_estudio
estudio_prom_boxplot<-ggplot(data, aes(x="",y=Horas_promedio_diarias_estudio))+
  geom_boxplot(fill = 2)+
  labs(title ="Diagrama de cajas de Horas Promedio de estudio\n",y="Horas promedio")+
  stat_boxplot(geom = "errorbar",
               width = 0.15)
estudio_prom_boxplot
ggplotly(estudio_prom_boxplot)

#Horas_promedio_diarias_en_redes      
#Medidas de Horas_promedio_diarias_en_redes
summary(data$Horas_promedio_diarias_en_redes)
sd(data$Horas_promedio_diarias_en_redes)
kurtosis(data$Horas_promedio_diarias_en_redes)
skewness(data$Horas_promedio_diarias_en_redes)

#Tabla de frecuencia de Horas_promedio_diarias_en_redes
horas_redes_table <- as.data.frame(table(data$Horas_promedio_diarias_en_redes))

horas_redes_table$'Frec. Rel.'<-horas_redes_table$Freq/sum(horas_redes_table$Freq)
horas_redes_table$'Frec. Rel. Porc'<-horas_redes_table$'Frec. Rel.'*100
horas_redes_table$'Frec. Abs. Acum.'<-cumsum(horas_redes_table$Freq)
horas_redes_table$'Frec. Rel. Acum. Porc.'<-cumsum(horas_redes_table$`Frec. Rel. Porc`)

colnames(horas_redes_table)<-c('Horas Prom. Sueño','Frec. Abs.','Frec. Rel.','Frec. Rel. Porc.','Frec. Abs. Acum.','Frec. Rel. Acum. Porc.')
horas_redes_table <- as.data.frame(horas_redes_table)

j1 <- c(3:4, 6)

horas_redes_table[j1] <- lapply(horas_redes_table[j1], function(x) sub(".", ",", sprintf("%.2f", x), fixed = TRUE))

horas_redes_table

#Histograma de Horas_promedio_diarias_en_redes
redes_prom_hist<-ggplot(data, aes(x=Horas_promedio_diarias_en_redes)) + 
  geom_histogram(bins=n_distinct(data$Horas_promedio_diarias_en_redes),binwidth = 1)+
  labs(title ="Histograma de Horas promedio en redes\n", x="Horas Promedio", y="Frecuencia")

ggplotly(redes_prom_hist)

#Diagrama de cajas de Horas_promedio_diarias_en_redes
redes_prom_boxplot<-ggplot(data, aes(x="",y=Horas_promedio_diarias_en_redes))+
  geom_boxplot(fill = 2)+
  labs(title ="Diagrama de cajas de Horas Promedio en redes\n",y="Horas promedio")+
  stat_boxplot(geom = "errorbar",
               width = 0.15)
redes_prom_boxplot
ggplotly(redes_prom_boxplot)

# Estadistica descriptiva univariante
#Variables Cualitativas
#PROMEDIO
#Sexo vs Promedio
sexo_prom_boxplot<-ggplot(data, aes(x=Sexo,y=Promedio))+
  geom_boxplot(fill = 2)+
  labs(title ="Diagrama de cajas de Sexo - Promedio\n",y="Horas promedio")+
  stat_boxplot(geom = "errorbar",
               width = 0.15)
ggplotly(sexo_prom_boxplot)
#Trabaja y Promedio
trab_prom_boxplot<-ggplot(data, aes(x=Trabaja,y=Promedio))+
  geom_boxplot(fill = 2)+
  labs(title ="Diagrama de cajas de Trabaja - Promedio\n",y="Horas promedio")+
  stat_boxplot(geom = "errorbar",
               width = 0.15)
ggplotly(trab_prom_boxplot)
#Computador Exclusivo y Promedio
comp_prom_boxplot<-ggplot(data, aes(x=Computador_uso_exclusivo,y=Promedio))+
  geom_boxplot(fill = 2)+
  labs(title ="Diagrama de cajas de Uso exclusivo de computador - Promedio\n",y="Horas promedio")+
  stat_boxplot(geom = "errorbar",
               width = 0.15)
ggplotly(comp_prom_boxplot)
#Despierta_mas_de_1vez_durante_noche y Promedio
despi_prom_boxplot<-ggplot(data, aes(x=Despierta_mas_de_1vez_durante_noche,y=Promedio))+
  geom_boxplot(fill=2)+
  labs(title ="Diagrama de cajas de Despertares Nocturnos - Promedio\n",y="Horas promedio")+
  stat_boxplot(geom = "errorbar",
               width = 0.15)
ggplotly(despi_prom_boxplot)
#Carrera y promedio
carr_prom_boxplot<-ggplot(data, aes(x=Carrera,y=Promedio,fill=Carrera))+
  geom_boxplot()+
  labs(title ="Diagrama de cajas de Carrera - Promedio\n",y="Horas promedio")+
  stat_boxplot(geom = "errorbar",
               width = 0.15)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank() 
  )
ggplotly(carr_prom_boxplot)
#Frecuencia_semanal_actividad_fisica y promedio
frec_prom_boxplot<-ggplot(data, aes(x=Frecuencia_semanal_actividad_fisica,y=Promedio))+
  geom_boxplot(fill=2)+
  labs(title ="Diagrama de cajas de Frecuencia semanal de actividad fisica - Promedio\n",y="Horas promedio")+
  stat_boxplot(geom = "errorbar",
               width = 0.15)
ggplotly(frec_prom_boxplot)

#HORAS SUEÑO
#Horas_promedio_diarias_sueño y Trabaja
trab_sueño_boxplot<-ggplot(data, aes(x=Trabaja,y=Horas_promedio_diarias_sueño))+
  geom_boxplot(fill=2)+
  labs(title ="Diagrama de cajas de Horas promedio de sueño - Trabaja\n",y="Horas promedio")+
  stat_boxplot(geom = "errorbar",
               width = 0.15)
#HORAS ESTUDIO
#Horas_promedio_diarias_estudio y Trabaja
trab_estudio_boxplot<-ggplot(data, aes(x=Trabaja,y=Horas_promedio_diarias_estudio))+
  geom_boxplot(fill=2)+
  labs(title ="Diagrama de cajas de Horas promedio de estudio - Trabaja\n",y="Horas promedio")+
  stat_boxplot(geom = "errorbar",
               width = 0.15)
#HORAS EN REDES
#Horas_promedio_diarias_en_redes y Trabaja
trab_redes_boxplot<-ggplot(data, aes(x=Trabaja,y=Horas_promedio_diarias_en_redes))+
  geom_boxplot(fill=2)+
  labs(title ="Diagrama de cajas de Horas promedio en redes - Trabaja\n",y="Horas promedio")+
  stat_boxplot(geom = "errorbar",
               width = 0.15)
#
ggplotly(trab_sueño_boxplot)
#HORAS ESTUDIO
#Horas_promedio_diarias_estudio y 
# Variables Cuantitativas
#Matriz de correlacion
cor_matrix_cuanti<-cor(data[, c('Promedio','Materias_Promedio_x_Termino','Horas_promedio_diarias_en_redes','Horas_promedio_diarias_estudio','Horas_promedio_diarias_sueño')])
#Matriz de Covarianzas
cov_matrix_cuanti<-cov(data[, c('Promedio','Materias_Promedio_x_Termino','Horas_promedio_diarias_en_redes','Horas_promedio_diarias_estudio','Horas_promedio_diarias_sueño')])

#Matriz grafica de correlacion - Cuantitativas
ggcorrplot(cor_matrix_cuanti,
           hc.order = TRUE,
           type = "lower",
           lab = TRUE)


#Intervalos de confianza
hombres<-data[data$Sexo=="M", ]
mujeres<-data[data$Sexo=="F", ]

prom_hombres_8<-hombres[hombres$Promedio>8,]

var(hombres$Promedio)
var(mujeres$Promedio)
shapiro.test(hombres$Promedio)
shapiro.test(mujeres$Promedio)

# F test to compare two variances
var.test(x=hombres$Promedio, y=mujeres$Promedio,
      conf.level = 0.95)
# T Test to compare differences of means
t.test(x=mujeres$Promedio, y=hombres$Promedio,
       paired=FALSE, var.equal=FALSE,
       conf.level = 0.95)
t.test(x=hombres$Promedio, y=mujeres$Promedio,
       paired=FALSE, var.equal=FALSE,
       conf.level = 0.95)
# Test to compare proportions
prop.test(x=10,n=77,conf.level=0.95)


var_cuantitativas <- subset(raw_data, select = c("Promedio","Materias_Promedio_x_Termino","Horas_promedio_diarias_sueño","Horas_promedio_diarias_estudio","Horas_promedio_diarias_en_redes") )

#Prueba Lilliefors (Kolmogorov-Smirnov) con la función lillie.test del paquete nortest.
#Prueba Pearson chi-square con la función pearson.test del paquete nortest.  
#chisq.test(data$Promedio)

shapiro.test(data$Horas_promedio_diarias_estudio)
shapiro.test(data$Promedio)
lillie.test(data$Horas_promedio_diarias_estudio)
lillie.test(data$Promedio)

#plot(data$Promedio, data$Horas_promedio_diarias_estudio, xlab='Promedio', ylab='horas estudio')
#abline(regresion)


regresion <- lm(Promedio ~ Peso, data = raw_data, na.action=na.omit)
summary(regresion)
grafregresion1<-ggplot(data=regresion, aes(x=Promedio,y=Peso))+
  geom_point()+
  geom_smooth(method='lm',formula=y~x, se=FALSE, col='dodgerblue1')+
  theme_light()
grafregresion1
