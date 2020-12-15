# Probability_Rstudio

set.seed(13)

# 1)a)
paq<-sample(1:640,5,replace = TRUE)

#1)b)
figstot<-c(1:640)
figspaq<-5
genpaq<-function(figstot,figspaq){
  paq<-sample(figstot,figspaq,replace = TRUE)
  paq
}

#1)c) 
cuantospaqs<-function(figstot,figspaq){
  paqs<-0
  albm<-c()
  while(length(albm)<length(figstot)){
    paqs<-paqs+1
    albm<-unique(c(albm,genpaq(figstot,figspaq)))
  }
  return(paqs)
}


#2)a)



N<-replicate(100,{
  cuantospaqs(figstot,figspaq)
})
x<-table(N)/100
x
barplot(x,xlab='paqs necesarios',ylab='probabilidad',main = 'Func de proba de N')


C<-N*20
y<-table(C)/100
y
barplot(y,xlab='plata necesaria',ylab='probabilidad',main = 'Func de proba de C')


T<-c()
for(i in 1:100){
  X<-rexp(N[i]-1,1)
  T[i]<-sum(X)
}
plot(density(T),main = 'Func de densidad de T')

#2)b)

mean(N)
mean(C)
mean(T)

#2)c)



Nrep<-1000
N<-replicate(Nrep,{
  cuantospaqs(figstot,figspaq)
})
N
x<-table(N)/Nrep
x
barplot(x,xlab='paqs necesarios',ylab='probabilidad',main = 'Func de proba de N')

C<-N*20
y<-table(C)/Nrep
y
barplot(y,xlab='plata necesaria',ylab='probabilidad',main = 'Func de proba de C')

T<-c()
for(i in 1:Nrep){
  X<-rexp(N[i]-1,1)
  T[i]<-sum(X)
}
plot(density(T),main = 'Funcion de densidad de T')

mean(N)
mean(C)
mean(T)

