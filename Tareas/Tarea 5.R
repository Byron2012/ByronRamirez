#Tarea No.5

#INTEGRANTES
#Gloria Eugenia González Dionicio 999000336
#José Rolando Sánchez Pineda 999003628
#Byron Daniel Ramirez Tebalan 999003721


#Ejercicio 1
#Ho: u > 20000
#H1: u < 20000

alfa <- 0.03
n=100
media= 19500
sd= 3900
mu= 20000

z0<- (media-mu)/(sd/sqrt(n))
z0

zAlfa<- qnorm(alfa,0,1, lower.tail = TRUE)
zAlfa

z0 <  zAlfa


#Ejercicio 2
#H0: sigma <= 40
#H1: sigma >= 40

n= 10
s2 <- 27 #(varianza de la muestra)
sigma20<- 40  #(varianza de la población)
alfa <- 0.05

X2 <- ((n-1)*s2)/sigma20
X2

chi_sd2<- qchisq(1-alfa, n-1)
chi_sd2

X2> chi_sd2



#Ejercicio 3
media <- 35
n <- 30
desv <- 3.5
alfa <- 0.05/2
nivelconfianza<- 1-alfa

normal<- qnorm(nivelconfianza,0,1)
normal

error<-  desv/sqrt(n)
error
margen <- normal*error
margen

limInf<- media - margen
limsup <- media +margen
limInf
limsup



#Ejercicio 4
media <- 500
n <-5000
desv <- 100
alfa <- 0.1/2
nivelconfianza<- 1-alfa

normal<- qnorm(nivelconfianza,0,1)
normal

error<-  desv/sqrt(n)
error
margen <- normal*error
margen

limInf<- media - margen
limsup <- media +margen
limInf
limsup



#Ejercicio 5
#Girth (y) y Height(x
?trees
data(trees)
lm.trees <- lm(Girth~Height, data=trees)
summary (lm.trees)
plot (trees$Girth ~trees$Height)
abline (lm.trees)

#Girth(y) volumen (x)
data(trees)
lm.trees2 <- lm(Girth~Volume, data=trees)
summary (lm.trees2)
plot (trees$Girth ~trees$Volume)
abline (lm.trees2)

