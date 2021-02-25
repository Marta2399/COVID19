library(readxl)
library(tidyverse)
library(tsibble)

##Gráfica PIB
PIB <- read_excel("C:/Users/JoseGallardo/Documents/Ajolotec/PAP/BD_INDICADORES_MACROECONOMICOS.xlsx", sheet = "PIB")
#Filtrar del dataset solo los datos que sean de Jalisco
PIB <- PIB[PIB$`Nombre entidad`=="Jalisco",]
#Renombrar las columnas para facilitar los calculos
colnames(PIB) <- c("Clave","Estado","Year","Clave_Sector","Nombre_Sector","Actividad","PIB")
#Cambiar el tipo de dato de la columna "Year" para poder filtrar 
###PIB$Year <- factor(PIB$Year)
#Definir los datos que vamos a necesitar para filtar
a <- ggplot(data=PIB,aes(x=Year,y=PIB))
#Gráfico boxplot con Jitter
a + geom_jitter(color=I(3)) + geom_boxplot(alpha=I(0.5)) +
  facet_grid(Actividad~.,scales="free")+
  ggtitle("Gráficas de PIB por año, separadas por tipo de actividad")
#Grafíco de barras dividiendo por actividad
a + geom_col(aes(fill=Actividad)) +
  ggtitle("Gráfica de PIB por año, señalando tipo de actividad")

a + geom_line()


PIB_Tsibble <- PIB %>% as_tsibble(index=Year,key=c(Estado,Nombre_Sector))
PIB_Tsibble %>% filter(Estado=="Jalisco") %>% distinct(Nombre_Sector)




##Gráfica de Visitantes extranjeros por entrada aérea, por nacionalidad
Visitantes<- read_excel("C:/Users/JoseGallardo/Documents/Ajolotec/PAP/BD_INDICADORES_MACROECONOMICOS.xlsx", sheet = "Visitantes")
#Filtrar del dataset solo los datos que sean de Jalisco
Visitantes <- Visitantes[(Visitantes$Aeropuerto=='Puerto Vallarta, Jal.')|(Visitantes$Aeropuerto=='Guadalajara, Jal.'),] 
#Cambiar el tipo de dato de la columna "Año" para poder filtrar 
Visitantes$Año <- factor(Visitantes$Año)
##Definir los datos que vamos a necesitar para filtar
#Filtrando y eliminando ciertas regiones para tener un análisis más limpio
b <- ggplot(data=Visitantes[(Visitantes$Regiones!="América del Norte"&
                               Visitantes$Regiones!="Europa"&
                               Visitantes$Regiones!="Apátrida"&
                               Visitantes$Regiones!="Islas del Caribe"&
                               Visitantes$Regiones!="África"),],
            aes(x=Regiones,y=Entradas))
##Grafica de Barras dividiendo entre los dos aeropuertos y separando entre sexo
b + geom_col(aes(fill=Sexo)) + 
  facet_grid(Año~Aeropuerto,scales="free") +
  ggtitle("Graficas de visitantes extranjeros divididos por area y por aeropuerto")


#Grafica IMSS
library(reshape)
library(tidyr)
#Leer del excel los datos y pasarlos al dataframe IMMSS
IMSS <- read_excel("C:/Users/JoseGallardo/Documents/Ajolotec/PAP/BD_INDICADORES_MACROECONOMICOS.xlsx", sheet = "IMSS")
#Cambiar el nombre de las columnas para facilitar los calculos
colnames(IMSS) <- c("División","Grupo","Fracción","Fecha","Trabajadores_Asegurados","Trabajadores_Permanentes","Trabajadores Eventuales")
#Filtrar las fechas para que solo nos apareca el año (eliminar mes y día)
IMSS$Fecha <- substring(IMSS$Fecha,1,4)
#Mdificar la tabla para poder graficar los datos más sencillo
IMSS_Filtro <- IMSS %>% gather(Trabajador, Asegurados, c("Trabajadores_Asegurados",
                                                         "Trabajadores_Permanentes","Trabajadores Eventuales"))
#Definir los parámetros de la gráfica
Graf1 <- ggplot(data=IMSS_Filtro[IMSS_Filtro$Fecha!=2021,],aes(x=Fecha,y=Asegurados))
#Gráficar
Graf1 + geom_col(aes(fill=Trabajador)) + 
  ggtitle("Gráfica de trabajadores asegurados por IMSS, señalando el tipo de trabajador")



#Patrones
Patrones<- read_excel("C:/Users/JoseGallardo/Documents/Ajolotec/PAP/BD_INDICADORES_MACROECONOMICOS.xlsx",
                      sheet = "Patrones")
#Filtrar la fecha para que solo lea por año
Patrones$Fecha <- substring(Patrones$Fecha,1,4)
#Definir los parámetros de la gráfica
Graf2 <- ggplot(data=Patrones[Patrones$Fecha!=2021,],aes(x=Fecha,y=Patrones))
#Gráficar
Graf2 + geom_jitter(aes(color=cve2)) +
  coord_cartesian(ylim=c(0,40000)) +
  theme(axis.text.x = element_text(angle = 45)) + 
  ggtitle("Gráfica de Paotrones asegurados en IMMS por año, separado por tipo de trabajo")



#Inversión Extranjera Directa por tipo de inversión
#Definir el Dataset
IED <-  read_excel("C:/Users/JoseGallardo/Documents/Ajolotec/PAP/BD_INDICADORES_MACROECONOMICOS.xlsx",
                   sheet = "IED1")
#Filtrar la fecha para solo tomar los años
IED$Periodo <- substring(IED$Periodo,7,10)
#Cambiar nombre de columnas
colnames(IED) <- c("Fecha","Tipo","Cifras Preliminares","Cifras Revisadas","C")
#Reformular el dataset para facilitar la graficación
IED_Filtro <- IED %>% gather( Tipo_de_cifra,Cifras, c("Cifras Preliminares","Cifras Revisadas"))
#Hcarer factor la fecha para facilitar modelado
IED_Filtro$Fecha <- factor(IED_Filtro$Fecha)
#Definir valores de gráfica
Graf4 <- ggplot(data=IED_Filtro[IED_Filtro$Cifras>0,],aes(x=Fecha,y=Cifras))
#Graficar
Graf4 +  geom_col(aes(fill=Tipo_de_cifra)) + 
  ggtitle("Inversión Extranjera Directa por tipo de inversión")




#Características de la población ocupada según la Encuesta Nacional de Ocupación y Empleo
ENOE <- read_excel("C:/Users/JoseGallardo/Documents/Ajolotec/PAP/BD_INDICADORES_MACROECONOMICOS.xlsx",
                   sheet = "ENOE")
colnames(ENOE)<-c("Entidad","Posicion","Salud","Actividad","Ocupación","Ingreso","Formal","Personas")
ENOE <- ENOE[ENOE$Entidad=="Jalisco",]

Graf3 <- ggplot(data=ENOE[ENOE$Actividad!="NA",],aes(x=Actividad,y=Personas))
Graf3 + geom_col(aes(fill=Formal)) +
  theme(axis.text.x=element_text(angle=45)) + 
  ggtitle("Cantidad de personas en secotres, señalados por tipo de empleo")
