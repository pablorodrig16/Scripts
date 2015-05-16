randomTreatment<-function(treatments, probabilities = NULL) {
        set.seed (1)
        treatment<-function (n) sample(treatments,size = n,replace = TRUE, prob = probabilities)
        return (treatment)
}

treatments<-scan(what="character")

select<-randomTreatment(treatments)

n<-1
while (n<20){
print (table(select(n = 1000)))

n<-n+1}