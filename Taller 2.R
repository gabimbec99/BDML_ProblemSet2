install.packages("installr")
library(installr)
updateR()

install.packages("tidyverse")
install.packages("here")
install.packages("dplyr")

library("here")

remove.packages(c("ggplot2", "lifecycle"))
install.packages('Rcpp', dependencies = TRUE)
install.packages('ggplot2', dependencies = TRUE)
install.packages('lifecycle', dependencies = TRUE)


train_hogares<-readRDS(here("C:/Users/mrozo/OneDrive - Universidad de los Andes/Maestría/Big Data/data/train_hogares.Rds"))
train_personas<-readRDS(here("C:/Users/mrozo/OneDrive - Universidad de los Andes/Maestría/Big Data/data/train_personas.Rds"))

test_hogares<-readRDS(here("C:/Users/mrozo/OneDrive - Universidad de los Andes/Maestría/Big Data/data/test_hogares.Rds"))
test_personas<-readRDS(here("C:/Users/mrozo/OneDrive - Universidad de los Andes/Maestría/Big Data/data/test_personas.Rds"))

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

# Emparejamiento de la base de entrenamiento y prueba

#Paquetes de instalación
install.packages("pacman")
require("pacman")
p_load(tidyverse,knitr,kableExtra,here,jtools,ggstance,broom,broom.mixed,skimr)

install.packages("ggplot2")

install.packages("vctrs", type = "binary", dependencies = TRUE, repos = "https://cloud.r-project.org")

install.packages("ggplot2",
                 type = "binary",
                 dependencies = TRUE,
                 repos = "https://cloud.r-project.org")

remove.packages("rlang")

install.packages("rlang")

install.packages("caret")
library("caret")


#Selección de variables
# Emparejamiento de la base de entrenamiento y prueba

#Paquetes de instalación
install.packages("pacman")
require("pacman")
p_load(tidyverse,knitr,kableExtra,here,jtools,ggstance,broom,broom.mixed,skimr)

install.packages("ggplot2")

install.packages("vctrs", type = "binary", dependencies = TRUE, repos = "https://cloud.r-project.org")

install.packages("ggplot2",
                 type = "binary",
                 dependencies = TRUE,
                 repos = "https://cloud.r-project.org")

install.packages("caret")
library("caret")

gc()
#Selección de variables
skim(X_pobre_hogares)
#Cambiar variables a categóricas
X_pobre_hogares <- X_pobre_hogares[, !names(X_pobre_hogares) %in% c("Clase","Dominio","Fex_c","Fex_dpto")]
test_hogares <- test_hogares[, !names(test_hogares) %in% c("Clase","Dominio","Fex_c","Fex_dpto")]

X_pobre_hogares[, "Depto"] <- as.factor(X_pobre_hogares[, "Depto", drop = T])


X_pobre_hogares <- data.frame(X_pobre_hogares[, !names(X_pobre_hogares) %in% "Depto"],model.matrix(~ Depto-1,X_pobre_hogares))

test_hogares[, "Depto"] <- as.factor(test_hogares[, "Depto", drop = T])


test_hogares <- data.frame(test_hogares[, !names(test_hogares) %in% "Depto"],model.matrix(~ Depto-1,test_hogares))


# quitando los na 
sum(is.na(X_pobre_hogares$Pobre))

colSums(is.na(X_pobre_hogares))

## quitar las varuabes que estorban 

X_pobre_hogares <- X_pobre_hogares[, !names(X_pobre_hogares) %in% c("P5100","P5130","P5140")]
test_hogares <- test_hogares[, !names(test_hogares) %in% c("P5100","P5130","P5140")]

X_pobre_hogares$D <- 1
test_hogares$D <- 0

install.packages("plyr")
library("plyr")

data_hogares <- rbind.fill(X_pobre_hogares,test_hogares)

data_hogares <- data_hogares[, !names(data_hogares) %in% c("Pobre","Depto11")]

rm(test_hogares,train_hogares,X_pobre_hogares ,pobre,pobre_hogares,test_personas,train_personas,X_hogares,X_ingreso_hogares,X_personas)
#Soporte común
d_lm <- lm(D ~ .,  
               data = data_hogares, na.action=na.exclude) 

tidy(d_logit)


datos_pscore %>% 
  ggplot(aes(x = pscore, color = D)) +
  geom_density() +
  labs(x = "Probabilidad de pertencer a train",
       y = "Densidad") +
  scale_color_discrete(name = element_blank(), 
                       labels = c("Train", "Test")) +
  theme_bw() +
  theme(legend.position = "bottom")


##Ya contamos con probabilidades predichas de la participación al programa. Estas serán nuestro insumo para la identificar a los “clones”.

### Definición del Soporte Común
##La idea del soporte común es poder contar, para cada probabilidad, con casos de participantes y de no participantes. Esto nos permitirá descartar a quienes no podrán emparejarse. 



datos_pscore %>% 
  group_by(D) %>% 
  summarise(min_pscore = min(pscore),
            max_pscore = max(pscore)
  ) %>% distinct(D, min_pscore, max_pscore)
min_T <- datos_pscore[which(datos_pscore$D), "pscore"] %>% min()
max_C <- datos_pscore[which(!datos_pscore$D),"pscore"] %>% max()



n_datos_pscore <- datos_pscore %>% 
  filter(D & pscore <= max_C | !D & pscore >= min_T)


##Esto resulta en un pequeño cambio

count(datos_pscore, D) %>% mutate(datos = "antes") %>% 
  bind_rows(count(n_datos_pscore, D) %>% mutate(datos = "después")) %>% 
  pivot_wider(names_from = "D", values_from = "n")




#Regularización
lambda <- 10^seq(-2, 3, length = 100)
lasso <- train(
  Pobre ~ . ,  data = X_pobre_hogares, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(alpha = 1, lambda=lambda), preProcess = c("center", "scale"))
lasso

#Prueba que funciona
data(swiss) 
set.seed(123) #set the seed for replication purposes
str(swiss) 

ridge <- train(
  Fertility ~., data = swiss, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(alpha = 0, lambda=lambda), preProcess = c("center", "scale")
)
ridge
