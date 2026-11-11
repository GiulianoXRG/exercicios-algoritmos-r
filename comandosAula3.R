getwd()

setwd("C:/Users/Flavio/OneDrive/Cursos/Algorítmo para Ciências Atuariais - R/Adistancia")


# Usando a funcao para criar matriz. Funcao "matrix"

A2=matrix(c(2,3,5,1,1,0.5,5,6,7),byrow=FALSE,ncol=3)
A2

z=1:10
z

y=seq(1,20,2)
y

rep("a",5)

rep(1:4,2)
rep(1:4,each=3)
rep(c("Tres","Dois","Sete","Quatro"),c(3,2,7,4))
z=c("Tres","Dois","Sete","Quatro")
z

# comecando com  entrada de dados com importacao de arquivos

dados1=read.table('dados1.csv',sep=';',dec=',',header=T)

is.data.frame(dados1)
mean(dados1$Altura)
summary(dados1$Altura)
var(dados1$Altura)
plot(dados1$Altura,dados1$Peso)
