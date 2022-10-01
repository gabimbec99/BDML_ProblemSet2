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
 
 En el archivo Scrapping.R, se pueden encontrar 4 grandes secciones, correspondientes a cada punto del Problem Set 1 Big Data y Machine Learning for Applied Economics, los cuales son:
 - Data Scrapping and Data Cleaning
 
 Consiste en los procesos de limpieza y selección de las variables de la base scrappeado del GEIH del año 2018. Así mismo, se incluyen una serie de gráficos y tablas descriptivas de las mismas.
 
 - Age-earnings profiles
 
 Tiene como objetivo ser una primera aproximación hacia los modelos de predicción de ingresos a través de la estimación de un modelo de regresión linear a partir del polinomio de segundo grado de la edad. También, se exploran las implicaciones de los resultados encontrados en términos de sus medidas de ajuste y se discute el peak age, siendo aquella edad que se alcanza un pico de ganancias según la literatura económica.
 
 - The gender earnings GAP
  
  Estudia las desigualdades en ingresos que pueden existir entre hombres y mujeres como una variable explicativa de la predicción de los ingresos en base a ciertas características de los individuos y en términos generales. En razón de ello, se implementan varias especificaciones de modelos lineales que buscan estudiar este fenómeno y cómo entenderlo aporta a la predicción de los ingresos. Como en la sección anterior, todavía se hace una discusión sobre el peak ages.
 
 - Predicting earnings
  
  Prueba el poder predictivo de cada uno de los modelos usando como función de pérdida la raíz de los errores medios al cuadrado aplicando diversas técnicaas de validación, con lo cual se buscará encontrar y analizar aquellos con mayor poder predictivo.
 