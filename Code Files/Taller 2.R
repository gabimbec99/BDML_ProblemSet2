########################## PROBLEM SET 2 BDML 2022-2 - PREDICCIÓN DE LA POBREZA ####################################
## Daniel Lasso
## Matteo Rozo
## Gabriela Mejía


######### Consideraciones ##############

# Crear operador notin
`%notin%` <- Negate(`%in%`)

## En adelante es necesario cambiar el path de trabajo propio ##

# Descargar paquetes si no se encuentran disponibles
pckgs <- c("tidyverse", "data.table", "rlang", "readxl")
if (any(pckgs %notin% rownames(installed.packages())==TRUE)){
  install.packages(pckgs, repos = c(CRAN = "http://cloud.r-project.org"))}
install.packages("caret")

# Descargar y cargar algunos paquetes
invisible(sapply(pckgs, FUN = require, character.only = TRUE))
install.packages("vtable")
library(vtable)
install.packages("here")
install.packages("dplyr")
install.packages("readxl")
library(readxl)
library(dplyr)  
library(here)
library(tidyr)
library("caret")

## Lectura de los datos (Cambio de path)

train_hogares<-readRDS(("D:/noveno semestre/big data/BDML_ProblemSet2/data/train_hogares.Rds"))
train_personas<-readRDS(("D:/noveno semestre/big data/BDML_ProblemSet2/data/train_personas.Rds"))
test_hogares<-readRDS(("D:/noveno semestre/big data/BDML_ProblemSet2/data/test_hogares.Rds"))
test_personas<-readRDS(("D:/noveno semestre/big data/BDML_ProblemSet2/data/test_personas.Rds"))

#Directorios GM
train_hogares<-readRDS(("/Users/gabrielamejia/Documents/GitHub/BDML_ProblemSet2/data/train_hogares.Rds"))
train_personas<-readRDS(("/Users/gabrielamejia/Documents/GitHub/BDML_ProblemSet2/data/train_personas.Rds"))
test_hogares<-readRDS(("/Users/gabrielamejia/Documents/GitHub/BDML_ProblemSet2/data/test_hogares.Rds"))
test_personas<-readRDS(("/Users/gabrielamejia/Documents/GitHub/BDML_ProblemSet2/data/test_personas.Rds"))

################ DATA CLEANING ##################

colnames_personas_test <- colnames(test_personas) 
ingreso_personas = train_personas[,c("Ingtot")]
train_personas= train_personas[,colnames_personas_test]
train_personas=cbind(train_personas,ingreso_personas)
train_personas= data.frame(train_personas)
#### Data cleaning para la base train personas
## Es necesario recodificar las  variables y su formato
library(haven)
train_personas = haven::as_factor(train_personas)

# Importamos los códigos de las variables categóricas (Cambiar path)
key <- read_excel("D:/noveno semestre/big data/BDML_ProblemSet2/data/recode_vals.xlsx") %>%
  select(c("var_name"),contains("value"))

#Con directorio GM
key <- read_excel("/Users/gabrielamejia/Documents/GitHub/BDML_ProblemSet2/data/recode_vals.xlsx") %>%
  select(c("var_name"),contains("value"))

variables_cambio <- key[["var_name"]]

# Tras hacer la recodificacion se seleccionan las variables mas relevantes, y las variables que son consistentes a lo largo del test:
X1=train_personas[,variables_cambio]
X2= train_personas[,c("id","P6040","Orden","Clase","Depto","P6040","P6430","P6800","P6870","Pet","Oc","Des","Ina")]
Ingtot= train_personas[,"Ingtot"]
train_personas =cbind(Ingtot,X2,X1)
train_personas= data.frame(train_personas)

#### Encontrando el numero de missings

# ver los missing values 
is.na(train_personas)
colSums(is.na(train_personas))

##reemplazar categoricas 

variables_categoricas <- c("Depto")
for (v in variables_categoricas) {
  train_personas[, v] <- as.factor(train_personas[, v, drop = T])
}

train_personas <- train_personas %>% mutate_at(vars("Ingtot"), ~replace_na(.,0))
train_personas <- train_personas %>% mutate_at(vars("P6800"), ~replace_na(.,0))

library(forcats)
# Reemplazando estas variables por "No" 

asignar_no_col <- c("P6510", "P6545", "P6580", "P6590", "P6610","P7040","P7090","P7110", "P7495", "P7510s3", "P7510s5", "P6920")
variables_factor <- names(select_if(train_personas, is.factor))

for (v in asignar_no_col) {
  print(class(v))
  print(v)
  train_personas[, v] <- fct_explicit_na(train_personas[, v], "No")
}

# Reemplazando por "No sabe"

train_personas$P6090 <- fct_explicit_na(train_personas$P6090, "No sabe, no informa")
train_personas$Ina <- fct_explicit_na(train_personas$Ina, "No info")
train_personas$Oc <- fct_explicit_na(train_personas$Oc, "No info")
train_personas$Des <- fct_explicit_na(train_personas$Des, "No info")
train_personas$Pet <- fct_explicit_na(train_personas$Pet, "No Pet")
train_personas$P6870 <- fct_explicit_na(train_personas$P6870, "No tamano")
train_personas$P6430 <- fct_explicit_na(train_personas$P6430, "No evidencia")
train_personas$P6210 <- fct_explicit_na(train_personas$P6210, "No sabe,no informa")
train_personas$P6240 <- fct_explicit_na(train_personas$P6240, "Otra actividad")
train_personas$logIngtot <- log(train_personas$Ingtot+1)

# Creamos las dummys de las variables factores a partir de model matrix

variables_factor <- names(select_if(train_personas, is.factor))
train_personas_dummy <- train_personas[,variables_factor]
train_personas_dummy <- model.matrix( ~.-1, data=train_personas_dummy)

## Matriz del modelo

train_personas_dummy <- as.data.frame(train_personas_dummy)
variables_numeric <- names(select_if(train_personas, is.numeric))
variables_character <- names(select_if(train_personas, is.character))
train_personas_id <- train_personas[,variables_character]
train_personas_num <- train_personas[,variables_numeric]
train_personas <- cbind(train_personas_num, train_personas_dummy, train_personas_id )

#### Data cleaning para la base test personas - Este código se repite de la sección inmediatamente anterior, 
#por lo que lo incluimos todo de corrido sin comentarios. Para mayor detalle referirse a la sección anterior

test_personas = haven::as_factor(test_personas)

key <- read_excel("D:/noveno semestre/big data/BDML_ProblemSet2/data/recode_vals.xlsx") %>%
  select(c("var_name"),contains("value")) 

variables_cambio <- key[["var_name"]]

X1=test_personas[,variables_cambio]
X2= test_personas[,c("id","P6040","Orden","Clase","Depto","P6040","P6430","P6800","P6870","Pet","Oc","Des","Ina")]
test_personas =cbind(X2,X1)
test_personas= data.frame(test_personas)

is.na(test_personas)
colSums(is.na(test_personas))

variables_categoricas <- c("Depto")
for (v in variables_categoricas) {
  test_personas[, v] <- as.factor(test_personas[, v, drop = T])
}

test_personas <- test_personas %>% mutate_at(vars("P6800"), ~replace_na(.,0))

asignar_no_col <- c("P6510", "P6545", "P6580", "P6590", "P6610","P7040","P7090","P7110", "P7495", "P7510s3", "P7510s5", "P6920")
variables_factor <- names(select_if(test_personas, is.factor))

for (v in asignar_no_col) {
  print(class(v))
  print(v)
  test_personas[, v] <- fct_explicit_na(test_personas[, v], "No")
}

test_personas$P6090 <- fct_explicit_na(test_personas$P6090, "No sabe, no informa")
test_personas$Ina <- fct_explicit_na(test_personas$Ina, "No info")
test_personas$Oc <- fct_explicit_na(test_personas$Oc, "No info")
test_personas$Des <- fct_explicit_na(test_personas$Des, "No info")
test_personas$Pet <- fct_explicit_na(test_personas$Pet, "No Pet")
test_personas$P6870 <- fct_explicit_na(test_personas$P6870, "No tamano")
test_personas$P6430 <- fct_explicit_na(test_personas$P6430, "No evidencia")
test_personas$P6210 <- fct_explicit_na(test_personas$P6210, "No sabe,no informa")
test_personas$P6240 <- fct_explicit_na(test_personas$P6240, "Otra actividad")

variables_factor <- names(select_if(test_personas, is.factor))
test_personas_dummy <- test_personas[,variables_factor]

test_personas_dummy <- model.matrix( ~.-1, data=test_personas_dummy)

test_personas_dummy <- as.data.frame(test_personas_dummy)

variables_numeric <- names(select_if(test_personas, is.numeric))
variables_character <- names(select_if(test_personas, is.character))

test_personas_id <- test_personas[,variables_character]
test_personas_num <- test_personas[,variables_numeric]

test_personas <- cbind(test_personas_num, test_personas_dummy, test_personas_id )


########## DATA MERGE #################
## Aquí el objetivo es agregar a nivel de hogar algunas variables de los individuos para poder tener más información de los hogare y clasificarlos de manera correcta
## Este procedimiento, por facilidad y cuestiones de timepo decidiimos realizarlo en Stata

### Merge de train
#Lo pasamos a stata, a través de un CSV, la transformación se encuentra en el dofile "Merge_hogares_stata"
#Cambiar el path
write.csv(train_hogares, file = "D:/noveno semestre/big data/BDML_ProblemSet2/data/train_hogares.csv")
write.csv(train_personas, file = "D:/noveno semestre/big data/BDML_ProblemSet2/data/train_personas.csv")

## Con la base exportada de stata volvemos al script de R
## Cambiar el path
train_hogares<-read_csv(("D:/noveno semestre/big data/BDML_ProblemSet2/data/train_hogares_final.csv"))

## Revisar missing values 
colSums(is.na(train_hogares))

## Crear una variable del valor del arriendo, pues creemos que es buen proxy de calidad y estrato de la vivienda
## Lo cual jos puede acercar a la medición del ingreso de la unidad de gasto
train_hogares <- train_hogares %>% mutate_at(vars("p5130","p5140"), ~replace_na(.,0))
train_hogares$valor_arriendo = rowSums(train_hogares[,c("p5130", "p5140")])
train_hogares <- subset(train_hogares, select = -c(p5130,p5140,p5100, clase, dominio, indigente, npobres, nindigentes, fex_c, depto, fex_dpto) )
colSums(is.na(train_hogares))

### Merge de test-  Este código se repite de la sección inmediatamente anterior, 
#por lo que lo incluimos todo de corrido sin comentarios. Para mayor detalle referirse a la sección anterior

#Cambiar el path

write.csv(test_hogares, file = "D:/noveno semestre/big data/BDML_ProblemSet2/data/test_hogares.csv")
write.csv(test_personas, file = "D:/noveno semestre/big data/BDML_ProblemSet2/data/test_personas.csv")
test_hogares<-read_csv(("D:/noveno semestre/big data/BDML_ProblemSet2/data/test_hogares_final.csv"))
colSums(is.na(test_hogares))
test_hogares <- test_hogares %>% mutate_at(vars("p5130","p5140"), ~replace_na(.,0))
test_hogares$valor_arriendo = rowSums(test_hogares[,c("p5130", "p5140")])
test_hogares <- subset(test_hogares, select = -c(p5130,p5140,p5100, clase, dominio, fex_c, depto, fex_dpto) )
colSums(is.na(test_hogares))

#Ahora debemos revisar que las bases tenga a misma estructura y cheqeuar que haya quedado correctamente el merge
setdiff(colnames(train_hogares), colnames(test_hogares))
head(train_hogares$v71)
skimr::skim(train_hogares$depto11)

## No tienen las mismas columnas entonces toca arreglarlo

train_hogares_X <- subset(train_hogares, select = -c(ingtotug, ingtotugarr, ingpcug ,depto11 ,v71, p60401, li, npersug) )
test_hogares <- subset(test_hogares, select = -c(v68,p60401, li, npersug) )

setdiff(colnames(train_hogares_X), colnames(test_hogares))

################ DATA MATCHING PARA EL TESTEO ##################
## La evaluación de este problem set se dará según la capacidad de predicción de los modelos dentro de una muestra
## de test específica. Por esta razón, ya ante el desconocimiento que tenemos de los valores de pobreza de
## dentro de la base que tenemos de testeo, es necesario crear nuestro propio testeo a través de una estrategia de 
## Propensity Score Matching que permita a partir de observables construir un subtest y un subtrain que nos de 
## unos valores cercanos al test verdadero sobre el cual se realizará la evaluación

set.seed(1)

# Usamos 70% de los datos como un training set y 30% como test set
sample <- sample(c(TRUE, FALSE), nrow(train_hogares_X), replace=TRUE, prob=c(0.7,0.3))
sub_train_hogares  <- train_hogares_X[sample, ]
sub_test_hogares   <- train_hogares_X[!sample, ]
sub_y_train <- subset(sub_train_hogares, select = c(pobre) )
sub_y_test <- subset(sub_test_hogares, select = c(pobre) )
## eliminar pobre y ID de las X 
sub_train_hogares <- subset(sub_train_hogares, select = -c(pobre, id) )
sub_test_hogares <- subset(sub_test_hogares, select = -c(pobre, id) )

## Para realizar el matching, podemos realizar distintos modelos para obtener la mejor predicción sobre 
## si la observación pertenece a train o test

# Implementamos oversampling
library(pacman)
library(haven)

p_load(AER, tidyverse, caret, MLmetrics, tidymodels, themis)

sub_y_train$pobre <- as.factor(sub_y_train$pobre)
train_hogares2 <- recipe(pobre ~ .,data = cbind(sub_train_hogares, sub_y_train)) %>%
  themis::step_smote(pobre, over_ratio = 1) %>%
  prep() %>%
  bake(new_data = NULL)

prop.table(table(train_hogares2$pobre))


##LASSO PROBABILISTICO 

library(glmnet)
library(kableExtra)

train_hogares2_X <- subset(train_hogares2, select = -c(pobre) )
train_hogares2_X <- as.matrix(train_hogares2_X)
train_hogares2_y <- train_hogares2[,"pobre"]

glmmod <- cv.glmnet(train_hogares2_X, as.factor(train_hogares2_y$pobre), alpha = 1, family="binomial")

train_hogares2$pobre <- as.numeric(train_hogares2$pobre)
probs_insample2 <- predict(glmmod, train_hogares2_X)
probs_insample2[probs_insample2 < 0] <- 0
probs_insample2[probs_insample2 > 1] <- 1
probs_outsample2 <- predict(glmmod, as.matrix(sub_test_hogares))
probs_outsample2[probs_outsample2 < 0] <- 0
probs_outsample2[probs_outsample2 > 1] <- 1

# Convertimos la probabilidad en una predicción
y_hat_insample2 <- as.numeric(probs_insample2 > 0.5)
y_hat_outsample2 <- as.numeric(probs_outsample2 > 0.5)

acc_insample2 <- Accuracy(y_pred = y_hat_insample2, y_true = train_hogares2$pobre)
acc_outsample2 <- Accuracy(y_pred = y_hat_outsample2, y_true = sub_y_test)

pre_insample2 <- Precision(y_pred = y_hat_insample2, y_true = train_hogares2$pobre, positive = 1)
pre_outsample2 <- Precision(y_pred = y_hat_outsample2, y_true = sub_y_test$pobre, positive = 1)

rec_insample2 <- Recall(y_pred = y_hat_insample2, y_true = train_hogares2$pobre, positive = 1)
rec_outsample2 <- Recall(y_pred = y_hat_outsample2, y_true = sub_y_test$pobre, positive = 1)

f1_insample2 <- F1_Score(y_pred = y_hat_insample2, y_true = train_hogares2$pobre, positive = 1)
f1_outsample2 <- F1_Score(y_pred = y_hat_outsample2, y_true = sub_y_test$pobre, positive = 1)

metricas_insample2 <- data.frame(Modelo = "Lasso", 
                                 "Muestreo" = "SMOTE - Oversampling", 
                                 "Evaluación" = "Dentro de muestra",
                                 "Accuracy" = acc_insample2,
                                 "Precision" = pre_insample2,
                                 "Recall" = rec_insample2,
                                 "F1" = f1_insample2)

metricas_outsample2 <- data.frame(Modelo = "Lasso", 
                                  "Muestreo" = "SMOTE - Oversampling", 
                                  "Evaluación" = "Fuera de muestra",
                                  "Accuracy" = acc_outsample2,
                                  "Precision" = pre_outsample2,
                                  "Recall" = rec_outsample2,
                                  "F1" = f1_outsample2)


metricas2 <- bind_rows(metricas_insample2, metricas_outsample2)
metricas <- bind_rows(metricas2)

metricas %>%
  kbl(digits = 2)  %>%
  kable_styling(full_width = T)


# Implementamos undersampling
train_hogares3 <- recipe(pobre ~ .,data = cbind(sub_train_hogares, sub_y_train)) %>%
  themis::step_downsample(pobre) %>%
  prep() %>%
  bake(new_data = NULL)

prop.table(table(train_hogares3$pobre))

train_hogares3_X <- subset(train_hogares3, select = -c(pobre) )
train_hogares3_X <- as.matrix(train_hogares3_X)
train_hogares3_y <- train_hogares3[,"pobre"]

glmmod3 <- cv.glmnet(train_hogares3_X, as.factor(train_hogares3_y$pobre), alpha = 1, family="binomial")

train_hogares3$pobre <- as.numeric(train_hogares3$pobre)
probs_insample3 <- predict(glmmod3, train_hogares3_X)
probs_insample3[probs_insample3 < 0] <- 0
probs_insample3[probs_insample3 > 1] <- 1
probs_outsample3 <- predict(glmmod3, as.matrix(sub_test_hogares))
probs_outsample3[probs_outsample3 < 0] <- 0
probs_outsample3[probs_outsample3 > 1] <- 1

# Convertimos la probabilidad en una predicción
y_hat_insample3 <- as.numeric(probs_insample3 > 0.5)
y_hat_outsample3 <- as.numeric(probs_outsample3 > 0.5)

acc_insample3 <- Accuracy(y_pred = y_hat_insample3, y_true = train_hogares3$pobre)
acc_outsample3 <- Accuracy(y_pred = y_hat_outsample3, y_true = sub_y_test$pobre)

pre_insample3 <- Precision(y_pred = y_hat_insample3, y_true = train_hogares3$pobre, positive = 1)
pre_outsample3 <- Precision(y_pred = y_hat_outsample3, y_true = sub_y_test$pobre, positive = 1)

rec_insample3 <- Recall(y_pred = y_hat_insample3, y_true = train_hogares3$pobre, positive = 1)
rec_outsample3 <- Recall(y_pred = y_hat_outsample3, y_true = sub_y_test$pobre, positive = 1)

f1_insample3 <- F1_Score(y_pred = y_hat_insample3, y_true = train_hogares3$pobre, positive = 1)
f1_outsample3 <- F1_Score(y_pred = y_hat_outsample3, y_true = sub_y_test$pobre, positive = 1)

sensitivity_insample3 <- specificity(train_hogares3$pobre, y_hat_insample3)


metricas_insample3 <- data.frame(Modelo = "Lasso", 
                                 "Muestreo" = "SMOTE - Undersampling", 
                                 "Evaluación" = "Dentro de muestra",
                                 "Accuracy" = acc_insample3,
                                 "Precision" = pre_insample3,
                                 "Recall" = rec_insample3,
                                 "F1" = f1_insample3)

metricas_outsample3 <- data.frame(Modelo = "Lasso", 
                                  "Muestreo" = "SMOTE - UnderSampling", 
                                  "Evaluación" = "Fuera de muestra",
                                  "Accuracy" = acc_outsample3,
                                  "Precision" = pre_outsample3,
                                  "Recall" = rec_outsample3,
                                  "F1" = f1_outsample3)


metricas3 <- bind_rows(metricas_insample3, metricas_outsample3)
metricas <- bind_rows(metricas2, metricas3)

metricas %>%
  kbl(digits = 2)  %>%
  kable_styling(full_width = T)

## RIDGE

glmmod_ridge <- cv.glmnet(train_hogares2_X, as.factor(train_hogares2_y$pobre), alpha = 0, family="binomial")

train_hogares2$pobre <- as.numeric(train_hogares2$pobre)
probs_insample4 <- predict(glmmod_ridge, train_hogares2_X)
probs_insample4[probs_insample4 < 0] <- 0
probs_insample4[probs_insample4 > 1] <- 1
probs_outsample4 <- predict(glmmod_ridge, as.matrix(sub_test_hogares))
probs_outsample4[probs_outsample4 < 0] <- 0
probs_outsample4[probs_outsample4 > 1] <- 1

# Convertimos la probabilidad en una predicción
y_hat_insample4 <- as.numeric(probs_insample4 > 0.5)
y_hat_outsample4 <- as.numeric(probs_outsample4 > 0.5)

acc_insample4 <- Accuracy(y_pred = y_hat_insample4, y_true = train_hogares2$pobre)
acc_outsample4 <- Accuracy(y_pred = y_hat_outsample4, y_true = sub_y_test)

pre_insample4 <- Precision(y_pred = y_hat_insample4, y_true = train_hogares2$pobre, positive = 1)
pre_outsample4 <- Precision(y_pred = y_hat_outsample4, y_true = sub_y_test$pobre, positive = 1)

rec_insample4 <- Recall(y_pred = y_hat_insample4, y_true = train_hogares2$pobre, positive = 1)
rec_outsample4 <- Recall(y_pred = y_hat_outsample4, y_true = sub_y_test$pobre, positive = 1)

f1_insample4 <- F1_Score(y_pred = y_hat_insample4, y_true = train_hogares2$pobre, positive = 1)
f1_outsample4 <- F1_Score(y_pred = y_hat_outsample4, y_true = sub_y_test$pobre, positive = 1)

metricas_insample4 <- data.frame(Modelo = "Ridge", 
                                 "Muestreo" = "SMOTE - Oversampling", 
                                 "Evaluación" = "Dentro de muestra",
                                 "Accuracy" = acc_insample4,
                                 "Precision" = pre_insample4,
                                 "Recall" = rec_insample4,
                                 "F1" = f1_insample4)

metricas_outsample4 <- data.frame(Modelo = "Ridge", 
                                  "Muestreo" = "SMOTE - Oversampling", 
                                  "Evaluación" = "Fuera de muestra",
                                  "Accuracy" = acc_outsample4,
                                  "Precision" = pre_outsample4,
                                  "Recall" = rec_outsample4,
                                  "F1" = f1_outsample4)


metricas4 <- bind_rows(metricas_insample4, metricas_outsample4)
metricas <- bind_rows(metricas2, metricas3, metricas4)

metricas %>%
  kbl(digits = 2)  %>%
  kable_styling(full_width = T)

## Undersampling
train_hogares3$pobre <- as.numeric(train_hogares3$pobre)
probs_insample5 <- predict(glmmod_ridge, train_hogares3_X)
probs_insample5[probs_insample5 < 0] <- 0
probs_insample5[probs_insample5 > 1] <- 1
probs_outsample5 <- predict(glmmod_ridge, as.matrix(sub_test_hogares))
probs_outsample5[probs_outsample5 < 0] <- 0
probs_outsample5[probs_outsample5 > 1] <- 1

# Convertimos la probabilidad en una predicción
y_hat_insample5 <- as.numeric(probs_insample5 > 0.5)
y_hat_outsample5 <- as.numeric(probs_outsample5 > 0.5)


acc_insample5 <- Accuracy(y_pred = y_hat_insample5, y_true = train_hogares3$pobre)
acc_outsample5 <- Accuracy(y_pred = y_hat_outsample5, y_true = sub_y_test)

pre_insample5 <- Precision(y_pred = y_hat_insample5, y_true = train_hogares3$pobre, positive = 1)
pre_outsample5 <- Precision(y_pred = y_hat_outsample5, y_true = sub_y_test$pobre, positive = 1)

rec_insample5 <- Recall(y_pred = y_hat_insample5, y_true = train_hogares3$pobre, positive = 1)
rec_outsample5 <- Recall(y_pred = y_hat_outsample5, y_true = sub_y_test$pobre, positive = 1)

f1_insample5 <- F1_Score(y_pred = y_hat_insample5, y_true = train_hogares3$pobre, positive = 1)
f1_outsample5 <- F1_Score(y_pred = y_hat_outsample5, y_true = sub_y_test$pobre, positive = 1)

metricas_insample5 <- data.frame(Modelo = "Ridge", 
                                 "Muestreo" = "SMOTE - Undersampling", 
                                 "Evaluación" = "Dentro de muestra",
                                 "Accuracy" = acc_insample5,
                                 "Precision" = pre_insample5,
                                 "Recall" = rec_insample5,
                                 "F1" = f1_insample5)

metricas_outsample5 <- data.frame(Modelo = "Ridge", 
                                  "Muestreo" = "SMOTE - Undersampling", 
                                  "Evaluación" = "Fuera de muestra",
                                  "Accuracy" = acc_outsample5,
                                  "Precision" = pre_outsample5,
                                  "Recall" = rec_outsample5,
                                  "F1" = f1_outsample5)


metricas5 <- bind_rows(metricas_insample5, metricas_outsample5)
metricas <- bind_rows(metricas2, metricas3, metricas4, metricas5)

metricas %>%
  kbl(digits = 2)  %>%
  kable_styling(full_width = T)

## Random Forest 

install.packages("randomForest")
library(randomForest)
fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))

train_hogares2$pobre <- as.numeric(train_hogares2$pobre)

fitControl <- trainControl(
  method = 'cv', 
  summaryFunction = fiveStats,     # k-fold cross validation
  number = 5,                      # number of folds
  savePredictions = 'final',       # saves predictions for optimal tuning parameter
  classProbs = T,                  # should class probabilities be returned  # results summary function
) 

train_hogares2$pobre<- ifelse(train_hogares2$pobre==0,"No","Yes")
train_hogares2$pobre <- as.factor(train_hogares2$pobre)

model_rf = train(pobre ~ ., data=train_hogares2, method='rf', tuneLength=5, trControl = fitControl, metric="Sens")
model_rf

varImp(model_rf,scale=TRUE)
varImpPlot(model_rf, scale=TRUE)


probs_insample6 <- predict(model_rf, train_hogares2_X)
probs_outsample6 <- predict(model_rf, as.matrix(sub_test_hogares))

# Convertimos la probabilidad en una predicción
y_hat_insample6 <- as.numeric(probs_insample6) - 1
y_hat_outsample6 <- as.numeric(probs_outsample6) -1


acc_insample6 <- Accuracy(y_pred = y_hat_insample6, y_true = train_hogares2_y$pobre)
acc_outsample6 <- Accuracy(y_pred = y_hat_outsample6, y_true = sub_y_test)

pre_insample6 <- Precision(y_pred = y_hat_insample6, y_true = train_hogares2_y$pobre, positive = 1)
pre_outsample6 <- Precision(y_pred = y_hat_outsample6, y_true = sub_y_test$pobre, positive = 1)

rec_insample6 <- Recall(y_pred = y_hat_insample6, y_true = train_hogares2_y$pobre, positive = 1)
rec_outsample6 <- Recall(y_pred = y_hat_outsample6, y_true = sub_y_test$pobre, positive = 1)

f1_insample6 <- F1_Score(y_pred = y_hat_insample6, y_true = train_hogares2_y$pobre, positive = 1)
f1_outsample6 <- F1_Score(y_pred = y_hat_outsample6, y_true = sub_y_test$pobre, positive = 1)

metricas_insample6 <- data.frame(Modelo = "Random Forest", 
                                 "Muestreo" = "SMOTE - Oversampling", 
                                 "Evaluación" = "Dentro de muestra",
                                 "Accuracy" = acc_insample6,
                                 "Precision" = pre_insample6,
                                 "Recall" = rec_insample6,
                                 "F1" = f1_insample6)

metricas_outsample6 <- data.frame(Modelo = "Random Forest", 
                                  "Muestreo" = "SMOTE - Oversampling", 
                                  "Evaluación" = "Fuera de muestra",
                                  "Accuracy" = acc_outsample6,
                                  "Precision" = pre_outsample6,
                                  "Recall" = rec_outsample6,
                                  "F1" = f1_outsample6)


metricas6 <- bind_rows(metricas_insample6, metricas_outsample6)
metricas <- bind_rows(metricas2, metricas3, metricas4, metricas5 ,metricas6)

metricas %>%
  kbl(digits = 2)  %>%
  kable_styling(full_width = T)

## PSCORE

test_hogares2<-subset(test_hogares,select=-c(id))
train_hogares4<-subset(train_hogares2,select=-c(pobre))
train_hogares4$D<-"No"
test_hogares2$D<-"Yes"

train_test<-as.data.frame(rbind(train_hogares4,test_hogares2))

model_rf3 = train(D~p6920no+p6870notamano+valor_arriendo+ocnoinfo+p7495no+p6430noevidencia+p6800+p6090nosabenoinforma+petnopet+p6430trabajadorporcuentapropia+p5090+p5000+lp+p6240oficiosdelhogar+p6210básicaprimaria1o5o+p6040+nper+p6870101omáspersonas+orden+p6240otraactividad, data=train_test, method='rf', tuneLength=5, trControl = fitControl, metric="Sens")
varImp(model_rf3,scale=TRUE)


sc_obs<-as.data.frame(model_rf3$pred$obs)
sc_pred<-as.data.frame(model_rf3$pred$pred)

sc_total<-cbind(sc_obs,sc_pred)
colnames(sc_total) <- c('Muestra','Predicho')

skimr::skim(sc_total)
sc_total_export<-sc_total[sc_total$Muestra %in% c('No'),]
sc_total_export2<-sc_total[sc_total$Muestra %in% c('Yes'),]


##Base de soporte común

train_hogares2sc<-cbind(train_hogares2,sc_total_export)
train_hogares2sc<-train_hogares2sc[train_hogares2sc$Predicho %in% c('No'),]
write.csv(train_hogares2sc,file="train_hogares_sc.csv")

######### ESTADÍSTICAS DESCRIPTIVAS ##########
## Aquí se sacan las tablas que usamos en la descripción de los datos en formato latex, histograma del ingreso y gráfico de waffle (Sobre la train y test originales)

st(train_personas, out='latex')
st(test_personas, out='latex')

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


# Gráfico de waffle
test <- c(`No Pobre`= 131936, `Pobre`= 33024)
waffle(test/10000, rows=2, size=0.8, title="Hogares pobres", 
       xlab="1 cuadrado = 10 000 personas",colors = c(RColorBrewer::brewer.pal(3, "Set2")[1:2]))

waffle(int2/1000, rows=10, size=0.8, title="Estrato de los individuos", 
       xlab="1 cuadrado = 1000 personas")


ggplot(train_personas, aes(x=Estrato1, y=)) + geom_jitter()

summary(X_pobre_hogares$Pobre)

# Emparejamiento de la base de entrenamiento y prueba

#Selección de variables
# Emparejamiento de la base de entrenamiento y prueba