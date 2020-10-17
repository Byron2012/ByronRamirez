#Sesión 6
# Pruebas de hipótesis 



#cálculo de la normal
#elementos necesario: intervalo de confianza, normal=0 y DS=1
#la normal se calcula a dos colas a menos que se diga lo contrario
?qnorm

qnorm(0.90,0,1, lower.tail = FALSE)
qnorm(0.10,0,1, lower.tail = TRUE)
qnorm(0.90,0,1, lower.tail = TRUE)
qnorm(0.90,0,1)
qnorm(0.95,0,1)
qnorm(0.975,0,1)
qnorm(0.99,0,1)
qnorm(0.995,0,1)

#tstudent para muestras pequeñas (p= probabilidad y df= grados de libertad)
?qt
qt(0.005,14)



#calculo de la chi cuadrada
#elementos necesarios:  Intervalo de confianza (probabilidad), grados de libertad(df)
#la chi cuadrada se calcula a una cola, tirada a la derecha. 

?qchisq

qchisq(0.900, 5, lower.tail = F)
qchisq(0.900, 5, lower.tail = T)
qchisq(0.900, 5)
qchisq(0.95,20, lower.tail=F)
qchisq(0.99,20, lower.tail=F)
qchisq(0.995,20, lower.tail=F)




#Calculo intervalo de confianza media
#elementos necesarios:  media, nivel de confianza, DS, tamaño muestra n

#formula general:  media +/- error
#error=  z(alfa medios)(Desv/SQRT(n))

media <- 250
n <-25
desv <- 2.5
alfa <- 0.05/2

nivelconfianza<- 1-alfa

#se está calculando a dos colas +/-, por lo que debe calcularse Z de alfa medios)
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


#Calculando intervalo de confianza para la varianza
#elementos necesarios:  varianza muestral (s2), grados de libertad (gl)

n<- 10
gl<- n-1
s2<- 38.5
alfa<-0.05
x2sup <- 1-(alfa/2)
x2inf <- (alfa/2)

x2sup
x2inf
ChisQsup<- qchisq(x2sup, gl, lower.tail = F)
chisQinf <- qchisq(x2sup, gl, lower.tail = T)
#o 
chisQinf2 <- qchisq(x2inf, gl, lower.tail = F)

chisQinf
chisQinf2

e1<- (gl*s2)/chisQinf2
e1
e2 <- (gl*s2)/ChisQsup
e2


#pruebas de hipotesis
data("airquality")
attach(airquality)
head(airquality)
?airquality

#prueba de hipotesis a dos colas, Igualdad. 

#hipótesis: con el conjunto de datos de airquality, se establece la hipotesis que la media de solar.R2 es igual a 170
#1 ho: Mu= 170
#2 h1: Mu=/ 170

#limpiando  los datos. 
Solar.R
mean(Solar.R)
#sin na
Solar.R2 <- Solar.R[!is.na(Solar.R)]
Solar.R2


#3 definimos alfa
alfa <-0.05

#4 definimos valores
n <- length(Solar.R2)
media <- mean(Solar.R2)
s <- sd(Solar.R2)
mu <- 170

# 5 calculando el estadistico de prueba - VER DIAPO 
#dado que no conocemos la desviación estandar poblacional, solamente la muestral, utilizamos T-studen. 
t0<- (media-mu)/(s/sqrt(n)) 
t0

#6 calculando el valor de la t-student
#desde que es una prueba de igualdad, es una prueba a dos colas, esto significa alfa/2
#porque QT xq desconocia la desviacion estandar y la que tengo arriba yo la calcule
tAlfa2 <- qt((alfa/2),(n-1), lower.tail = F)
tAlfa2

#su conozco la Desviacion Estadar entonces uso la normal QNORM  

#7 validamos la hipotesis
?abs
abs(t0)> tAlfa2
#se rechaza la ho al no ser igual




#prueba de hipotesis para valores mayores
#1 h0: MU > 170
#2 h1:  Mu < 170

#los valores calculados en 3, 4 y 5 quedan igual

#6 el valor de la cola ya no se divide en dos, por lo que la prueba T debe hacerse sobre Alfa y no Alfa/2
tAlfa <- qt((alfa),(n-1), lower.tail = F)

#7 analizando el criterio de rechazo. 
t0 > tAlfa
#se acepta la hipotesis nula, donde se define la media como mayor a 170





#hipotesis de varianza. 
#definimos la hipótesis. 
#1 h0: sigma=0.36
#2 h1: sigma =/ 0.36

#3 y 4 el ejemplo es el que presenta en el video con las siguientes variables. 
n= 18
s2 <- 0.68
sigma20<- 0.36
alfa <- 0.05

#5 calculamos el estadistico de pruebas
X2 <- ((n-1)*s2)/sigma20
X2

#6 se calcula los valores de chi cuadrada 

chi_sd<- qchisq(1- alfa/2, n-1)
chi_sd

# Verificamos el criterio de rechazo
X2> chi_sd
#se rechaza la h0, significa que la variación del proceso es diferente a 0.36

#prueba de hipótesis de valor mayor 
#1. h0: sigma > 0.36
#2  h1: sigma < 0.36 

#3, 4 y 5 quedan igual

#6 se calcula chi cuadrada para alfa (ojo, alfa, no alfa 2, ya que se está validando una cola)
chi_sd2<- qchisq(1- alfa, n-1)
chi_sd2

#7 verificación
X2> chi_sd2
#se acepta la h0, significa que el valor de la varianza del proceso es mayor a 0.36




#pruebas T
media<- mean(Solar.R2)
media
mu1<-170
#h0: mu=170
?t.test
t.test(Solar.R2, mu= mu1, alternative = "two.sided")
