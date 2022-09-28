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


########## limpiar test personas

test_personas = haven::as_factor(test_personas)

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