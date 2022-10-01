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

 - Data Merging and Data Cleaning
 
 Consiste en los procesos de unión de las bases de hogares y personas, la selección de variables que serán usadas para la evaluación, y la construcción de otras variables relevantes que agregan características de personas a nivel hogar para una predicción precisa. 
 
 