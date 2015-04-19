### Scripts para análisis de datos con R

Pablo O Rodriguez


##Funciones.R

argumentos: (x,y)==>x=datos, y=define grupo, ambas variables deben tener la misma logitud

* mediaSD: muestra media+-SD
* medianaIQR: muestra mediana (p25-p75)
* shapiroGrupo: muestra P de Shapiro test por grupo
* es.normal.por.grupo: evalua (normal o no normal) por grupo para variables numéricas
* p.Fisher: P de test de Fisher entre grupos
* p.Chisq: P de Chisq entre grupos
* p.Ttest: P de t.test para muestras no pareadas con igual varianza
* p.Wilcox: P de test de Wilcoxon para 2 muestras
* p.ANOVA: P de ANOVA para 3 o mas grupos
* p.Friedman: P de test de Friedman para 3 o mas grupos


##Tablas y análisis univariado


Agrega la función creaTabla, con 2 argumentos (datos,grupo)

datos: una variable, tabla o data.frame con datos cuantitativos o cualitativos
grupo: una variable de agrupamiento de la misma longitud que datos



##Reporte de tablas y análisis univariado.
Markdown con la tabla de salida de creaTabla


