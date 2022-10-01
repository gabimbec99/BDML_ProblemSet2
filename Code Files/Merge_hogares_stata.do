
*Tratamiento de datos: Formas alternativas en Stata

cd "D:\noveno semestre\big data\BDML_ProblemSet2\data"

*Se importan las bases de r a Stata, se utilizan los CSV del código de R


import delimited "D:\noveno semestre\big data\BDML_ProblemSet2\data\train_personas.csv", clear

drop v1

ds,has(type byte) 
global vars `r(varlist)'

ds,has(type int) 
global int `r(varlist)'


collapse (mean) $vars $int, by(id)


save "personas_collapse", replace  

* ahora vamos a merge la info

import delimited "D:\noveno semestre\big data\BDML_ProblemSet2\data\train_hogares.csv", clear 
drop v1

save "train_hogares", replace  

merge 1:1 id using "personas_collapse" 

drop _merge

export delimited "train_hogares_final.csv" , replace

*******************************************
***************************** para testeo
*******************************************

*Tratamiento de datos: Formas alternativas en Stata

cd "D:\noveno semestre\big data\BDML_ProblemSet2\data"

*Se importan las bases de r a Stata, se utilizan los CSV del código de R


import delimited "D:\noveno semestre\big data\BDML_ProblemSet2\data\test_personas.csv", clear

drop v1

ds,has(type byte) 
global vars `r(varlist)'

ds,has(type int) 
global int `r(varlist)'


collapse (mean) $vars $int, by(id)


save "personas_collapse_test", replace  

* ahora vamos a merge la info

import delimited "D:\noveno semestre\big data\BDML_ProblemSet2\data\test_hogares.csv", clear 
drop v1

save "test_hogares", replace  

merge 1:1 id using "personas_collapse_test" 

drop _merge

export delimited "test_hogares_final.csv" , replace 


