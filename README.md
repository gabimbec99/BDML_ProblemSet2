# BDML_ProblemSet2
El propósito de este repositorio es generar la replicabilidad de la solución para el Problem Set 2 de Big Data and Machine Learning for Applied Economics.
 
 Grupo: [Daniel Lasso](https://github.com/daniell419), [Matteo Rozo](https://github.com/MatteoRozo) y [Gabriela Mejía](https://github.com/gabimbec99).
 
 Profesor: [Ignacio Sarmiento](https://github.com/ignaciomsarmiento)

 El espíritu de este Problem Set, busca predecir de manera acertada la pobreza dentro de una muestra de la Gran Encuesta Integrada de Hogares de 2018 para Colombia. 

 Dentro de este repositorio usted encontrará:
 
 Un reporte corto sobre el desarrollo de modelos de los clasificación de la pobreza de los ingresos y de predicción del ingreso, que encontrará en el siguiente enlace OverLeaf:(https://www.overleaf.com/8567919536rcvkfqcvpyvg)

 ## Data Files

Los archivos de datos: 'train_hogares.Rds', 'train_personas.Rds', 'test_hogares.Rds',
y 'test_personas.Rds', correspondientes a las bases de dato de entrenamiento y testeo de la GEIH, a partir de las cuales se constuyeron los modelos. El archivo 'recode_vals.xlxs' que contiene los valores en texto a los cuales corresponden las variables categóricas; y la base 'train_hogares.Rdata' que consolida la base final.

 ## Code Files 
 
El archivo Taller2.R, donde encontrará 4 grandes secciones:

 - Data Cleaning
 
 Consiste en los procesos de unión de las bases de hogares y personas, la selección de variables que serán usadas para la evaluación, la recodificiación de factores. 

 - Data Merge

 Consiste en el merge (durante el cual se migra a stata y cuyo código puede encontrarse en 'Merge_hogares_stata.do') entre las bases de personas y hogares tanto en train como en test. El objetivo del merge es poder agregar a nivel de hogares características observables de los individuos que nos puedan ayudar a generar predictores para la clasificación de los hogares en situación de pobreza.

 - Data Matching
 Consiste en el Matching de la base de testeo original con un subset de nuestro train original en el cual podamos a partir de observables intentar acercarnos a cuáles son los hogares del train original y podemos a partir de modelos de predicción realizar un propensity score matching. El objetivo es poder ajustar nuestros modelos a estos hogares de testeo, atentiendo a los incentivos de la evaluación de este Problem Set. 

- Estadísticas Descriptivas
- Modelos finales
 
 
