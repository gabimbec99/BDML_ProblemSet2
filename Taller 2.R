## sancocho de paquetes 
install.packages("vtable")
library(vtable)
## sancocho de paquetes 

# create notin operator
`%notin%` <- Negate(`%in%`)

#### consideraciones 

##############

#### cambia el path de trabajo #######

# Download packages if not available
pckgs <- c("tidyverse", "data.table", "rlang", "readxl")
if (any(pckgs %notin% rownames(installed.packages())==TRUE)){
  install.packages(pckgs, repos = c(CRAN = "http://cloud.r-project.org"))}
install.packages("caret")


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

library("caret")

## leyendo los datos

train_hogares<-readRDS(("D:/noveno semestre/big data/BDML_ProblemSet2/data/train_hogares.Rds"))
train_personas<-readRDS(("D:/noveno semestre/big data/BDML_ProblemSet2/data/train_personas.Rds"))

test_hogares<-readRDS(("D:/noveno semestre/big data/BDML_ProblemSet2/data/test_hogares.Rds"))
test_personas<-readRDS(("D:/noveno semestre/big data/BDML_ProblemSet2/data/test_personas.Rds"))

#Directorios GM
train_hogares<-readRDS(("/Users/gabrielamejia/Documents/GitHub/BDML_ProblemSet2/data/train_hogares.Rds"))
train_personas<-readRDS(("/Users/gabrielamejia/Documents/GitHub/BDML_ProblemSet2/data/train_personas.Rds"))

test_hogares<-readRDS(("/Users/gabrielamejia/Documents/GitHub/BDML_ProblemSet2/data/test_hogares.Rds"))
test_personas<-readRDS(("/Users/gabrielamejia/Documents/GitHub/BDML_ProblemSet2/data/test_personas.Rds"))



############ limpieza de base de entrenamiento  
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

#Directorio GM
key <- read_excel("/Users/gabrielamejia/Documents/GitHub/BDML_ProblemSet2/data/recode_vals.xlsx") %>%
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
train_personas$logIngtot <- log(train_personas$Ingtot+1)

######################## dummys 

variables_factor <- names(select_if(train_personas, is.factor))
train_personas_dummy <- train_personas[,variables_factor]


train_personas_dummy <- model.matrix( ~.-1, data=train_personas_dummy)

## matriz del modelo. 

train_personas_dummy <- as.data.frame(train_personas_dummy)


variables_numeric <- names(select_if(train_personas, is.numeric))
variables_character <- names(select_if(train_personas, is.character))

train_personas_id <- train_personas[,variables_character]
train_personas_num <- train_personas[,variables_numeric]


train_personas <- cbind(train_personas_num, train_personas_dummy, train_personas_id )

########## limpiar test personas

test_personas = haven::as_factor(test_personas)

# import the codebook 
key <- read_excel("/Users/gabrielamejia/Documents/GitHub/BDML_ProblemSet2/data/recode_vals.xlsx") %>%
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
#test_personas %>% 
#mutate(across(.cols = variables_cambio, ~ recode_the_vals(.x)))



# tras hacer la recodificacion se seleccionan las variables mas relevantes:
X1=test_personas[,variables_cambio]
X2= test_personas[,c("id","P6040","Orden","Clase","Depto","P6040","P6430","P6800","P6870","Pet","Oc","Des","Ina")]
test_personas =cbind(X2,X1)
test_personas= data.frame(test_personas)

#### encontrando el numero de missings

# ver los missing values 
is.na(test_personas)
colSums(is.na(test_personas))

## reemplazar categoricas 

variables_categoricas <- c("Depto")
for (v in variables_categoricas) {
  test_personas[, v] <- as.factor(test_personas[, v, drop = T])
}

test_personas <- test_personas %>% mutate_at(vars("P6800"), ~replace_na(.,0))

# reemplazando estas variables por no 

asignar_no_col <- c("P6510", "P6545", "P6580", "P6590", "P6610","P7040","P7090","P7110", "P7495", "P7510s3", "P7510s5", "P6920")
variables_factor <- names(select_if(test_personas, is.factor))

for (v in asignar_no_col) {
  print(class(v))
  print(v)
  test_personas[, v] <- fct_explicit_na(test_personas[, v], "No")
}

# reemplazando por no sabe

test_personas$P6090 <- fct_explicit_na(test_personas$P6090, "No sabe, no informa")
test_personas$Ina <- fct_explicit_na(test_personas$Ina, "No info")
test_personas$Oc <- fct_explicit_na(test_personas$Oc, "No info")
test_personas$Des <- fct_explicit_na(test_personas$Des, "No info")
test_personas$Pet <- fct_explicit_na(test_personas$Pet, "No Pet")
test_personas$P6870 <- fct_explicit_na(test_personas$P6870, "No tamano")
test_personas$P6430 <- fct_explicit_na(test_personas$P6430, "No evidencia")
test_personas$P6210 <- fct_explicit_na(test_personas$P6210, "No sabe,no informa")
test_personas$P6240 <- fct_explicit_na(test_personas$P6240, "Otra actividad")

######################## Estadísticas descriptivas de las bases entrenamiento y testeo para personas
st(train_personas, out='latex')
st(test_personas, out='latex')
######################## dummys 

variables_factor <- names(select_if(test_personas, is.factor))
test_personas_dummy <- test_personas[,variables_factor]


test_personas_dummy <- model.matrix( ~.-1, data=test_personas_dummy)

## matriz del modelo. 

test_personas_dummy <- as.data.frame(test_personas_dummy)


variables_numeric <- names(select_if(test_personas, is.numeric))
variables_character <- names(select_if(test_personas, is.character))

test_personas_id <- test_personas[,variables_character]
test_personas_num <- test_personas[,variables_numeric]


test_personas <- cbind(test_personas_num, test_personas_dummy, test_personas_id )


########## merge de base de hogar con personas #################
# colapsar las variables entre los adultos en el hogar #

#### vamos a colapsar la base en stata ###### 


write.csv(train_hogares, file = "D:/noveno semestre/big data/BDML_ProblemSet2/data/train_hogares.csv")
write.csv(train_personas, file = "D:/noveno semestre/big data/BDML_ProblemSet2/data/train_personas.csv")


##### volvemos al R. 


train_hogares<-read_csv(("D:/noveno semestre/big data/BDML_ProblemSet2/data/train_hogares_final.csv"))


#### revisar missing values 
colSums(is.na(train_hogares))

### crear una variable del valor del arriendo. 

train_hogares <- train_hogares %>% mutate_at(vars("p5130","p5140"), ~replace_na(.,0))
train_hogares$valor_arriendo = rowSums(train_hogares[,c("p5130", "p5140")])
train_hogares <- subset(train_hogares, select = -c(p5130,p5140,p5100, clase, dominio, indigente, npobres, nindigentes, fex_c, depto, fex_dpto) )
colSums(is.na(train_hogares))

# Implementamos oversampling
library(pacman)
library(haven)

p_load(AER, tidyverse, caret, MLmetrics, tidymodels, themis)


train_hogares$pobre = haven::as_factor(train_hogares$pobre)

train_hogares$pobre <- as.factor(train_hogares$pobre)

glimpse(train_hogares$pobre)

train_hogares$pobre <- factor(train_hogares$pobre)


train_hogares_sin_id <- subset(train_hogares, select = -c(id) )



train_hogares2 <- recipe(pobre ~ .,data = train_hogares_sin_id) %>%
  themis::step_smote(pobre, over_ratio = 1) %>%
  prep() %>%
  bake(new_data = NULL)

prop.table(table(train_hogares2$pobre))

########################### PROBANDO TODO ########################
library(kableExtra)
train_hogares2$pobre <- as.numeric(train_hogares2$pobre) - 1
modelo2 <- lm(formula = "pobre ~ .", data = train_hogares2)
probs_insample2 <- predict(modelo2, train_hogares2)
probs_insample2[probs_insample2 < 0] <- 0
probs_insample2[probs_insample2 > 1] <- 1

# Convertimos la probabilidad en una predicción
y_hat_insample2 <- as.numeric(probs_insample2 > 0.5)

acc_insample2 <- Accuracy(y_pred = y_hat_insample2, y_true = train_hogares2$pobre)

pre_insample2 <- Precision(y_pred = y_hat_insample2, y_true = train_hogares2$pobre, positive = 1)

rec_insample2 <- Recall(y_pred = y_hat_insample2, y_true = train_hogares2$pobre, positive = 1)

f1_insample2 <- F1_Score(y_pred = y_hat_insample2, y_true = train_hogares2$pobre, positive = 1)

metricas_insample2 <- data.frame(Modelo = "Regresión lineal", 
                                 "Muestreo" = "SMOTE - Oversampling", 
                                 "Evaluación" = "Dentro de muestra",
                                 "Accuracy" = acc_insample2,
                                 "Precision" = pre_insample2,
                                 "Recall" = rec_insample2,
                                 "F1" = f1_insample2)


metricas2 <- bind_rows(metricas_insample2)
metricas <- bind_rows(metricas2)

metricas %>%
  kbl(digits = 2)  %>%
  kable_styling(full_width = T)
#################################################### UNDERSAMPLING 

train_hogares3 <- recipe(pobre ~ .,data = train_hogares_sin_id) %>%
  themis::step_downsample(pobre) %>%
  prep() %>%
  bake(new_data = NULL)

train_hogares3$pobre <- as.numeric(train_hogares3$pobre) - 1
modelo3 <- lm(formula = "pobre ~ .", data = train_hogares3)
probs_insample3 <- predict(modelo3, train_hogares3)
probs_insample3[probs_insample3 < 0] <- 0
probs_insample3[probs_insample3 > 1] <- 1

# Convertimos la probabilidad en una predicción
y_hat_insample3 <- as.numeric(probs_insample3 > 0.5)

acc_insample3 <- Accuracy(y_pred = y_hat_insample3, y_true = train_hogares3$pobre)

pre_insample3 <- Precision(y_pred = y_hat_insample3, y_true = train_hogares3$pobre, positive = 1)

rec_insample3 <- Recall(y_pred = y_hat_insample3, y_true = train_hogares3$pobre, positive = 1)

f1_insample3 <- F1_Score(y_pred = y_hat_insample3, y_true = train_hogares3$pobre, positive = 1)

metricas_insample3 <- data.frame(Modelo = "Regresión lineal", 
                                 "Muestreo" = "SMOTE - Undersampling", 
                                 "Evaluación" = "Dentro de muestra",
                                 "Accuracy" = acc_insample3,
                                 "Precision" = pre_insample3,
                                 "Recall" = rec_insample3,
                                 "F1" = f1_insample3)

metricas3 <- bind_rows(metricas_insample3)
metricas <- bind_rows(metricas2, metricas3)

metricas %>%
  kbl(digits = 2)  %>%
  kable_styling(full_width = T)



###################################### UMBRAL ###########################
##### proximos pasos: Construir una tabla con oversampling regresion lineal, logit, probit, elastic net, lasso, ridge y random forest. Lo mismo para undersampling

# Esto no se debería hacer sobre la base de testeo pero se hace solo a modo ilustrativo
thresholds <- seq(0.1, 0.9, length.out = 100)
opt_t <- data.frame()
for (t in thresholds) {
  y_pred_t <- as.numeric(probs_outsample1 > t)
  f1_t <- F1_Score(y_true = test$infielTRUE, y_pred = y_pred_t,
                   positive = 1)
  fila <- data.frame(t = t, F1 = f1_t)
  opt_t <- bind_rows(opt_t, fila)
}

mejor_t <-  opt_t$t[which(opt_t$F1 == max(opt_t$F1, na.rm = T))]

ggplot(opt_t, aes(x = t, y = F1)) +
  geom_point(size = 0.7) +
  geom_line() +
  theme_bw() +
  geom_vline(xintercept = mejor_t, linetype = "dashed", 
             color = "red") +
  labs(x = "Threshold")

##################################################
########### HACIENDO LO MISMO PARA TEST################
##############################################

########## merge de base de hogar con personas #################
# colapsar las variables entre los adultos en el hogar #

#### vamos a colapsar la base en stata ###### 


write.csv(test_hogares, file = "D:/noveno semestre/big data/BDML_ProblemSet2/data/test_hogares.csv")
write.csv(test_personas, file = "D:/noveno semestre/big data/BDML_ProblemSet2/data/test_personas.csv")


##### volvemos al R. 


test_hogares<-read_csv(("D:/noveno semestre/big data/BDML_ProblemSet2/data/test_hogares_final.csv"))


#### revisar missing values 
colSums(is.na(test_hogares))

### crear una variable del valor del arriendo. 

test_hogares <- test_hogares %>% mutate_at(vars("p5130","p5140"), ~replace_na(.,0))
test_hogares$valor_arriendo = rowSums(test_hogares[,c("p5130", "p5140")])
test_hogares <- subset(test_hogares, select = -c(p5130,p5140,p5100, clase, dominio, fex_c, depto, fex_dpto) )

colSums(is.na(test_hogares))


##############################################
####################### revisar que tengan la misma estructura 
#############################################

setdiff(colnames(train_hogares), colnames(test_hogares))
head(train_hogares$v71)


##############################
############## NO tienen las mismas columnas entonces toca arreglarlo
###################################


train_hogares_X <- subset(train_hogares, select = -c(ingtotug, ingtotugarr, ingpcug, pobre ,depto11 ,v71) )
test_hogares <- subset(test_hogares, select = -c(v68) )

setdiff(colnames(train_hogares_X), colnames(test_hogares))



# preparando las dicotomas para el soporte comun
test_hogares$D <- 1
train_hogares$D <- 0

### estandarizando el train y test para las mismas variables. 
colnames_test_hogares <- colnames(test_hogares) 
train_hogares_X= train_hogares[,colnames_test_hogares]
train_hogares_X= data.frame(train_hogares_sc)




# reemplazar el ingtot de na por 0. 
train_personas <- train_personas %>% mutate_at(vars("Ingtot"), ~replace_na(.,0))


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


############################## creando variables agregadas #####################

##Ya contamos con probabilidades predichas de la participación al programa. Estas serán nuestro insumo para la identificar a los “clones”.

### Definición del Soporte Común
##La idea del soporte común es poder contar, para cada probabilidad, con casos de participantes y de no participantes. Esto nos permitirá descartar a quienes no podrán emparejarse. 

##Esto resulta en un pequeño cambio
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