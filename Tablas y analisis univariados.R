creaTabla<-function (datos, grupo){
  
############################################################################
####funciones#####
  
source('~/R/Scripts/Funciones.R', echo=FALSE)
  
#################Procesamiento de variables de agrupamiento###########
  
####Identificar variables cuantitativas y cualitativas: escribir el numero de columna####

var.analizar<-1:length (datos)


###convierte variables character a factor########
for (a in 1:length (datos)){
  if (is.character(datos[,a])){
    datos[,a]<-as.factor(datos[,a])
  }  
}

tabla <-c()
filas<-1

for (k in 1:length (datos)){
  if (is.numeric(datos[,k])||is.integer(datos[,k])){
    if (length(levels(as.factor(grupo)))==2)
    {
      
      #######variables cuantitatitvas grupo = 2 levels #####################
        if(es.normal.por.grupo(as.numeric(datos[,k]),grupo)=="normal")
        {
          linea<-c(tapply (as.numeric(datos[,k]),grupo, mediaSD),
                   mediaSD (as.numeric(datos[,k])),
                   es.normal.por.grupo(as.numeric(datos[,k]),grupo),
                   sum (is.na(datos[,k])),
                   p.Ttest(datos[,k],grupo))
          tabla<-rbind(tabla,linea)
        }
        else
        {	
          linea<-c(tapply (as.numeric(datos[,k]),grupo, medianaIQR),
                   medianaIQR (as.numeric(datos[,k])),
                   es.normal.por.grupo(as.numeric(datos[,k]),grupo),
                   sum (is.na(datos[,k])),
                   p.Wilcox(datos[,k],grupo))	
          tabla<-rbind(tabla,linea)
        }
        rownames(tabla)[filas]<-names(datos[var.analizar[k]])
        filas<-filas+1    
    }
    if (length(levels(as.factor(grupo)))>2)
    {
      #######variables cuantitatitvas grupo > 2 levels#####################
        if(es.normal.por.grupo(as.numeric(datos[,k]),grupo)=="normal")
        {
          linea<-c(tapply (as.numeric(datos[,k]),grupo, mediaSD),
                   mediaSD (as.numeric(datos[,k])),
                   es.normal.por.grupo(as.numeric(datos[,k]),grupo),
                   sum (is.na(datos[,k])),
                   p.Anova (datos[,k],grupo))	
          tabla<-rbind(tabla,linea)
        }else
        {	
          linea<-c(tapply (as.numeric(datos[,k]),grupo, medianaIQR),
                   medianaIQR (as.numeric(datos[,k])),
                   es.normal.por.grupo(as.numeric(datos[,k]),grupo),
                   sum (is.na(datos[,k])),
                   p.Friedman(datos[,k],grupo))	
          tabla<-rbind(tabla,linea)
        }
        rownames(tabla)[filas]<-names(datos[var.analizar[k]])
        filas<-filas+1
           
    }
    
  }else{
    if (length(levels(as.factor(grupo)))==2)
    {
     
      #######variables cualitatitvas#####################  
      #####analisis de variables TRUE y FALSE#####
        if (is.logical(datos[,k])==TRUE)
        {  
          linea<-c(
            paste (tapply (na.omit(datos[,k]),grupo[!is.na(datos[,k])], sum),
                   " (",
                   round(
                     tapply (na.omit(datos[,k]),grupo[!is.na(datos[,k])], sum)*100/
                       tapply (na.omit(datos[,k]), grupo[!is.na(datos[,k])], length)
                     ,2),
                   "%)", sep=""),
            paste (sum (na.omit (datos[,k])), " (",
                   round (
                     sum (na.omit (datos[,k]))*100/length(na.omit(datos[,k])),2),
                   "%)", sep=""),        
            "no normal",
            sum (is.na(datos[,k])),
            p.Fisher (datos[,k],grupo))
          tabla<-rbind(tabla,linea)
          rownames(tabla)[filas]<-names(datos[var.analizar[k]])
          filas<-filas+1
        }
        #####analisis de variables cualitativas no logical####
        else
        {
          for (l in 1:length (levels(as.factor(datos[,k]))))
          {
            nivel<-levels(as.factor(datos[,k]))[l]
            linea<-c(
              paste (tapply (na.omit(datos[,k])==nivel,grupo[!is.na(datos[,k])], sum),
                     " (",
                     round(
                       tapply (na.omit(datos[,k])==nivel,grupo[!is.na(datos[,k])], sum)*100/
                         tapply (na.omit(datos[,k]), grupo[!is.na(datos[,k])], length)
                       ,2),
                     "%)", sep=""),
              paste (sum (na.omit (datos[,k])==nivel), " (",
                     round (
                       sum (na.omit (datos[,k])==nivel)*100/length(na.omit(datos[,k])),2),
                     "%)", sep=""), 
              "no normal",
              sum (is.na(datos[,k])),
              p.Chisq (datos[,k],grupo))
            tabla<-rbind(tabla,linea)
            rownames(tabla)[filas]<-paste(names(datos[var.analizar[k]]),nivel)
            filas<-filas+1
          }
        }	
         
    }
    if (length(levels(as.factor(grupo)))>2)
    {
      #######variables cualitatitvas#####################	
      #####analisis de variables TRUE y FALSE#####
        if (is.logical(datos[,k])==TRUE)
        {	
          linea<-c(
            paste (tapply (na.omit(datos[,k]),grupo[!is.na(datos[,k])], sum),
                   " (",
                   round(
                     tapply (na.omit(datos[,k]),grupo[!is.na(datos[,k])], sum)*100/
                       tapply (na.omit(datos[,k]), grupo[!is.na(datos[,k])], length),
                     2),
                   "%)", sep=""),
            paste (sum (na.omit (datos[,k])), " (",
                   round (
                     sum (na.omit (datos[,k]))*100/length(na.omit(datos[,k])),2),
                   "%)", sep=""),
            "no normal",
            sum (is.na(datos[,k])),
            p.Chisq (datos[,k],grupo))
          tabla<-rbind(tabla,linea)
          rownames(tabla)[filas]<-names(datos[var.analizar[k]])
          filas<-filas+1
        }
        #####analisis de variables cualitativas no logical####    
        else
        {
          for (l in 1:length(levels(as.factor(datos[,k])))){
            nivel<-levels(as.factor(datos[,k]))[l]
            linea<-c(
              paste (tapply (na.omit(datos[,k])==nivel,grupo[!is.na(datos[,k])], sum),
                     " (",
                     round(
                       tapply (na.omit(datos[,k])==nivel,grupo[!is.na(datos[,k])], sum)*100/
                         tapply (na.omit(datos[,k]), grupo[!is.na(datos[,k])], length),
                       2),
                     "%)", sep=""),
              paste (sum (na.omit (datos[,k])==nivel), " (",
                     round (
                       sum (na.omit (datos[,k])==nivel)*100/length(na.omit(datos[,k])),2),
                     "%)", sep=""),
              "no normal",
              sum (is.na(datos[,k])),
              p.Chisq (datos[,k],grupo))
            tabla<-rbind(tabla,linea)
            rownames(tabla)[filas]<-paste(names(datos[var.analizar[k]]),nivel)
            filas<-filas+1
          }
        }	      
    }
    
  } 
}


####asigna nombres a las columnas####
for (i in 1:length (levels(as.factor(grupo))))
  {
  colnames(tabla)[i]<-paste(colnames(tabla)[i], 
                            " (n=",
                            summary (as.factor(grupo))[i],
                            ")", sep="")
}

colnames (tabla)[i+1]<-"Todos"
colnames (tabla)[i+2]<-"Distribucion"
colnames (tabla)[i+3]<-"Datos faltantes"
colnames (tabla)[i+4]<-"P"

####Imprime y copia al clipboard la tabla######
tabla
}