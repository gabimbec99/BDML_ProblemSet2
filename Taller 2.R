install.packages("tidyverse")
install.packages("here")
install.packages("dplyr")

library("here")

remove.packages(c("ggplot2", "lifecycle"))
  install.packages('Rcpp', dependencies = TRUE)
  install.packages('ggplot2', dependencies = TRUE)
  install.packages('lifecycle', dependencies = TRUE)


train_hogares<-readRDS(here("C:/Users/danie/Documents/taller 2 BD/data/train_hogares.Rds"))
train_personas<-readRDS(here("C:/Users/danie/Documents/taller 2 BD/data/train_personas.Rds"))

test_hogares<-readRDS(here("C:/Users/danie/Documents/taller 2 BD/data/test_hogares.Rds"))
test_personas<-readRDS(here("C:/Users/danie/Documents/taller 2 BD/data/test_personas.Rds"))

# extraer las varaibles que tengo en train y test: 

# merge pobreza a toda la base de train 

colnames_personas_test <- colnames(test_personas)
X_personas = train_personas[,colnames_personas_test]

pobre<-train_hogares$Pobre
pobre <- as.data.frame(pobre)
X_personas <- merge(x=train_personas, y=pobre, all=TRUE)



# obteniendo otras bases

colnames_hogares_test <- colnames(test_hogares) 
X_hogares = train_hogares[,colnames_hogares_test]
pobre_hogares = train_hogares[,c("Pobre")]
ingreso_hogares = train_hogares[,c("Ingpcug")]

X_ingreso_hogares =cbind(X_hogares,ingreso_hogares)
X_ingreso_hogares= data.frame(X_ingreso_hogares)

X_pobre_hogares =cbind(X_hogares,pobre_hogares)
X_pobre_hogares= data.frame(X_pobre_hogares)



# Estadisticas descriptivas
install.packages("ggplot2")
library(ggplot2)

#Histograma simple
ggplot(X_ingreso_hogares, aes(x=ingreso_hogares)) +
  geom_histogram(position="identity", , bins=200)+
  scale_x_continuous(n.breaks=10,labels=scales::dollar, limits=c(0, 15000000))+labs(title= "Histograma del Ingreso per cápita del hogar", subtitle = "Colombia", caption="Fuente: GEIH 2018")+  
  #Añadir los labels de los ejes
  xlab("Ingresos per capita del hogar") + ylab("Densidad") + theme_bw() 

# grafico de waffle 
#Se llaman los siguientes paquetes y librerias para los gráficos:
install.packages("ggplot2")
install.packages("waffle")
library(ggplot2)
library(waffle)

#Variables factores. 
library(haven)
X_pobre_hogares$Pobre = haven::as_factor(X_pobre_hogares$Pobre)
class(train_hogares$Pobre)


int<-table(X_pobre_hogares$Pobre)
int2<-table(train_personas$Estrato1)

# Gráfico de waffle


test <- c(`No Pobre`= 131936, `Pobre`= 33024)
waffle(test/10000, rows=2, size=0.8, title="Hogares pobres", 
       xlab="1 cuadrado = 10 000 personas",colors = c(RColorBrewer::brewer.pal(3, "Set2")[1:2]))

waffle(int2/1000, rows=10, size=0.8, title="Estrato de los individuos", 
       xlab="1 cuadrado = 1000 personas")


ggplot(train_personas, aes(x=Estrato1, y=)) + geom_jitter()

summary(X_pobre_hogares$Pobre)

# seleccion de variables. 


# identificar la variable objetivo: 
# es pobreza e ingpcug 
# seleccionar variables. 



# hacer la seleccion de vaiables: 







# elegir las variables de personas que hay en el test. 
colnames_personas_test <- colnames(test_personas) 
print(colnames_personas_test)

# obtener la pobreza 
sum_ingresos<-train_personas %>% group_by(id) %>% summarize(Ingtot_hogar=sum(Ingtot,na.rm = TRUE)) 


# seleccionar las variables del datafame
X= train_personas[,colnames_personas_test]

train_hogares$Pobre