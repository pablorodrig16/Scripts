
################################################################################################
####funciones#####
mediaSD<-function (x)
{
  paste(round(mean(x, na.rm=T),2),"+-",round(sd(x, na.rm=T),2),sep="")
}

medianaIQR<-function (x)
{
  paste(round (median(x, na.rm=T),2)," (",round(quantile(x, na.rm=T)[[2]],2),
        "-",round(quantile(x, na.rm=T)[[4]],2),")", sep="")
}

shapiroGrupo<-function(x,y)
{
  by (x,y,shapiro.test)
}

es.normal.por.grupo<-function(x,y)
{
  k<-numeric()
  for (i in 1:length(levels(y)))
  {
    k<-c(k,tryCatch(by (x,y,shapiro.test)[[i]][[2]], error=function(e){(0.01)})>0.05)
  }
  ifelse(sum(k)==length (levels(y)), "normal", "no normal")
}

p.Fisher<-function (x,y){
tryCatch (
  ifelse(fisher.test(na.omit(x),y[!is.na(x)])[[1]]<0.001,
       "<0.001",
       round(fisher.test(na.omit(x),y[!is.na(x)])[[1]],3)),
  error=function(e){"error"})
}
p.Chisq<-function (x,y){
  tryCatch (
    ifelse(chisq.test(na.omit(x),y[!is.na(x)])[[3]]<0.001,
           "<0.001",
           round(chisq.test(na.omit(x),y[!is.na(x)])[[3]],3)),
    error=function(e) {"error"},
    warning = function(w) {
      ifelse(chisq.test(na.omit(x),y[!is.na(x)],simulate.p.value = TRUE)[[3]]<0.001,
             "<0.001",
             round(chisq.test(na.omit(x),y[!is.na(x)],simulate.p.value = TRUE)[[3]],3))
      })
}
p.Ttest<-function (x,y){
  tryCatch (
    ifelse(t.test(na.omit(x)~y[!is.na(x)])[[3]]<0.001,
           "<0.001",
           round(t.test(na.omit(x)~y[!is.na(x)])[[3]],3)),
    error=function(e){"error"})
}
p.Wilcox<-function (x,y){
  tryCatch (
    ifelse(wilcox.test(na.omit(x)~y[!is.na(x)])$p.value<0.001,
           "<0.001",
           round(wilcox.test(na.omit(x)~y[!is.na(x)])$p.value,3)),
    error=function(e){"error"})
}
p.Anova<-function (x,y){
  tryCatch (
    ifelse(anova(aov(na.omit(x)~y[!is.na(x)]))[[5]][1]<0.001,
           "<0.001",
           round(anova(aov(na.omit(x)~y[!is.na(x)]))[[5]][1],3)),
    error=function(e){"error"})
}
p.Friedman<-function (x,y){
  tryCatch (
    ifelse(friedman.test(table(na.omit(x),y[!is.na(x)]))$p.value<0.001,
           "<0.001",
           round(friedman.test(table(na.omit(x),y[!is.na(x)]))$p.value,3)),
    error=function(e){"error"})
}

