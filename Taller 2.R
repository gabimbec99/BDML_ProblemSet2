## sancocho de paquetes 

# create notin operator
`%notin%` <- Negate(`%in%`)

# Download packages if not available
pckgs <- c("tidyverse", "data.table", "rlang", "readxl")
if (any(pckgs %notin% rownames(installed.packages())==TRUE)){
  install.packages(pckgs, repos = c(CRAN = "http://cloud.r-project.org"))}

# load packages
invisible(sapply(pckgs, FUN = require, character.only = TRUE))

#### estos son para otras cosas 
install.packages("here")
install.packages("dplyr")
install.packages("readxl")

library(readxl)
library(dplyr)  
library(here)
library(tidyr)

install.packages("caret")
library("caret")

## leyendo los datos

train_hogares<-readRDS(here("D:/noveno semestre/big data/BDML_ProblemSet2/data/train_hogares.Rds"))
train_personas<-readRDS(here("D:/noveno semestre/big data/BDML_ProblemSet2/data/train_personas.Rds"))

test_hogares<-readRDS(here("D:/noveno semestre/big data/BDML_ProblemSet2/data/test_hogares.Rds"))
test_personas<-readRDS(here("D:/noveno semestre/big data/BDML_ProblemSet2/data/test_personas.Rds"))

## creando el subset de las variables:

colnames_hogares_test <- colnames(test_hogares) 
pobre_hogares = train_hogares[,c("Pobre")]
ingreso_hogares = train_hogares[,c("Ingpcug")]
train_hogares = train_hogares[,colnames_hogares_test]
train_hogares =cbind(train_hogares,ingreso_hogares,pobre_hogares)
train_hogares= data.frame(train_hogares)

### limpiar la de personas

colnames_personas_test <- colnames(test_personas) 
ingreso_personas = train_personas[,c("Ingtot")]
train_personas= train_personas[,colnames_personas_test]
train_personas=cbind(train_personas,ingreso_personas)
train_personas= data.frame(train_personas)

# extraer las varaibles que son relevantes para el objeto de estudio:
# limpieza de las variables. 

############# hay que cambiar que es haven ###############
library(haven)

train_personas = haven::as_factor(train_personas)

# import the codebook 
key <- read_excel("D:/noveno semestre/big data/BDML_ProblemSet2/data/recode_vals.xlsx") %>%
  select(c("var_name"),contains("value")) 

variables_cambio <- key[["var_name"]]


## Functions

## split and unlist character values (separated by commas) into a numeric vector with n elements

#vulist <- function(x){suppressWarnings(as.numeric(unlist(strsplit(as.character(x), split = ",")))) }

## split and unlist string (separated by commas) into a character vector with n elements

#vlulist <- function(x){suppressWarnings(unlist(strsplit(as.character(x), split = ",")))}

## Option 5: recode values function (deframe) 
# (rlang, tibble, and dplyr)

#recode_the_vals <- function(x){	
  
 # x_name <- quo_name(enquo(x))
 # key.sub <- key %>% filter(var_name %in% x_name)
#  label.keys <- tibble(values = vlulist(key.sub$values), labels = vlulist(key.sub$value.labels))
#  recode(x, !!!(deframe(label.keys)))
#}

# recode values
#train_personas %>% 
  #mutate(across(.cols = variables_cambio, ~ recode_the_vals(.x)))



# tras hacer la recodificacion se seleccionan las variables mas relevantes:
X1=train_personas[,variables_cambio]
X2= train_personas[,c("id","P6040","Orden","Clase","Depto","P6040","P6430","P6800","P6870","Pet","Oc","Des","Ina")]
Ingtot= train_personas[,"Ingtot"]
train_personas =cbind(Ingtot,X2,X1)
train_personas= data.frame(train_personas)

#### encontrando el numero de missings

# ver los missing values 
is.na(train_personas)
colSums(is.na(train_personas))

## reemplazar categoricas 

variables_categoricas <- c("Depto")
for (v in variables_categoricas) {
  train_personas[, v] <- as.factor(train_personas[, v, drop = T])
}

train_personas <- train_personas %>% mutate_at(vars("Ingtot"), ~replace_na(.,0))
train_personas <- train_personas %>% mutate_at(vars("P6800"), ~replace_na(.,0))

library(forcats)
# reemplazando estas variables por no 

asignar_no_col <- c("P6510", "P6545", "P6580", "P6590", "P6610","P7040","P7090","P7110", "P7495", "P7510s3", "P7510s5", "P6920")
variables_factor <- names(select_if(train_personas, is.factor))

for (v in asignar_no_col) {
  print(class(v))
  print(v)
  train_personas[, v] <- fct_explicit_na(train_personas[, v], "No")
}

# reemplazando por no sabe

train_personas$P6090 <- fct_explicit_na(train_personas$P6090, "No sabe, no informa")
train_personas$Ina <- fct_explicit_na(train_personas$Ina, "No info")
train_personas$Oc <- fct_explicit_na(train_personas$Oc, "No info")
train_personas$Des <- fct_explicit_na(train_personas$Des, "No info")
train_personas$Pet <- fct_explicit_na(train_personas$Pet, "No Pet")
train_personas$P6870 <- fct_explicit_na(train_personas$P6870, "No tamano")
train_personas$P6430 <- fct_explicit_na(train_personas$P6430, "No evidencia")
train_personas$P6210 <- fct_explicit_na(train_personas$P6210, "No sabe,no informa")
train_personas$P6240 <- fct_explicit_na(train_personas$P6240, "Otra actividad")



install.packages("tidytable")
library(tidytable)

save(train_personas, file="D:/noveno semestre/big data/BDML_ProblemSet2/data/train_hogares.RData")


variables_factor <- names(select_if(train_personas, is.factor))
train_personas_dummy <- train_personas[,variables_factor]


train_personas_dummy <- train_personas_dummy %>%
  get_dummies.()

variable_dummy <- names(select_if(train_personas_dummy, is.numeric))
train_personas_dummy <- as.data.frame(train_personas_dummy)

train_personas_dummy <- train_personas_dummy[,variable_dummy]


variables_numeric <- names(select_if(train_personas, is.numeric))
variables_character <- names(select_if(train_personas, is.character))

train_personas_id <- train_personas[,variables_character]
train_personas_num <- train_personas[,variables_numeric]


train_personas <- cbind(train_personas_num, train_personas_dummy )


# reemplazar el ingtot de na por 0. 
train_personas <- train_personas %>% mutate_at(vars("Ingtot"), ~replace_na(.,0))


# Installing Packages
install.packages("dplyr")
install.packages("glmnet")
install.packages("ggplot2")
install.packages("caret")


rm(train_personas_num, train_personas_id, train_personas_dummy, X,Y,X1,X2,key)
rm(Ingtot)
rm(test_hogares, test_personas)
rm(pobre_hogares, ingreso_hogares, ingreso_personas)


library(caret)

X <- train_personas[,"P6040"]
Y <- train_personas[,"Ingtot"]


sum(is.na(train_personas$Ingtot))


# X and Y datasets
Y <- train_personas %>% 
  select(Ingtot) %>% 
  scale(center = TRUE, scale = FALSE) %>% 
  as.matrix()

X <- train_personas %>% 
  select(-Ingtot)%>%   as.matrix()

class(Y)

X <- train_personas %>% 
  select(-Ingtot)%>% 
  select(-id) %>%  as.matrix()


# Model Building : Elastic Net Regression
control <- trainControl(method = "repeatedcv",
                        number = 1,
                        repeats = 1,
                        search = "random",
                        verboseIter = TRUE)

# Training ELastic Net Regression model
elastic_model <- train(Ingtot ~ .,
                       data = cbind(Y, X),
                       method = "glmnet",
                       preProcess = c("center", "scale"),
                       tuneLength = 2,
                       trControl = control)

elastic_model

plot(elastic_model, main = "Elastic Net Regression")




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

remotes::update_packages("ggplot2")

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
library(rlang)

install.packages("caret")
library("caret")


#Selección de variables
# Emparejamiento de la base de entrenamiento y prueba


############################## creando variables agregadas #####################

for (c in variables_dummy){ 
train_hogares <- train_personas %>% group_by(id) %>% summarize(c_prom=mean(c,na.rm = TRUE)) 




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
d_lm <- lm(Ingtot ~ P6040,data = train_personas, na.action=na.exclude) 

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
library(glmnet)
cv_model <- cv.glmnet(X, Y, alpha = 1)

best_lambda <- cv_model$lambda.min
best_lambda

plot(cv_model) 


best_model <- glmnet(X, Y, alpha = 1, lambda = best_lambda)
coef(best_model)

lambda <- 10^seq(-2, 3, length = 100)
lasso <- train(
  Y ~ . ,  data = cbind(X, Y), method = "glmnet",
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
