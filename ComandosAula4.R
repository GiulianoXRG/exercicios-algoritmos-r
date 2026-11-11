
setwd("C:/...")

getwd()

help(mean)

# criando uma Lista

lista1=list(dados1,A2,x2,f)
lista1

lista1[[3]]
rm(x2)
x2 <- lista1[[3]]
x2

# funções rbind e cbind

A2=matrix(c(2,3,5,1,1,0.5,5,6,7),byrow=FALSE,ncol=3)
A2
A2.1=c(2,3,5)
A2.2=c(1,1,0.5)
A2.3=c(5,6,7)

A2.lixo = cbind(A2.1,A2.2,A2.3)
A2 - A2.lixo

A2.lixo2 = rbind(A2.1,A2.2,A2.3)
A2.lixo2

attach(dados1) 

dados1$Sexo=factor(dados1$Sexo,levels=c('M','F'),
                   labels=c('Masculino','Feminino'))
table(Sexo)
table(dados1$Idade)
table(Idade)
table(Altura)
mean(dados1$Altura)
var(Altura)
sd(Altura)

detach(dados1)

quantile(dados1$Idade,c(0.25,0.5,0.75))
median(dados1$Idade)

FxIdade=cut(dados1$Idade,c(0,27.5,33,37.5,max(dados1$Idade)))

dados1$Fxidade = FxIdade
dados1[,-5]
dados1.lixo = data.frame(dados1[,-5],FxIdade)
rm(dados1.lixo,A2.lixo,A2.lixo2)

table(FxIdade)

# dados1$Sexo=factor(dados1$Sexo)
# is.factor(dados1$Sexo)

is.matrix(dados1)
is.data.frame(dados1)

apply(dados1[,1:3],2,sd)
apply(dados1[,1:3],2,mean)

win.graph()
plot(dados1$Altura,dados1$Peso)
abline(h=80)
abline(v=1.72)
