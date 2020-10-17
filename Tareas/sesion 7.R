#Sesión 7
# Pruebas de hipótesis y regresión lineal


#hipotesis de varianza. 
#definimos la hipótesis. 
#1 h0: sigma=0.36
#2 h1: sigma =/ 0.36

#3 y 4 el ejemplo es el que presenta en el video con las siguientes variables. 
n= 18
s2 <- 0.68  #(varianza de la muestra)
sigma20<- 0.36  #(varianza de la población)
alfa <- 0.05

#5 calculamos el estadistico de pruebas
X2 <- ((n-1)*s2)/sigma20
X2

#6 se calcula los valores de chi cuadrada (a dos colas) (solo es necesario calcular una cola)
#(si escojemos la cola superior 1-alfa/2)
chi_sd<- qchisq(1- alfa/2, n-1)
chi_sd

#(si escojemos la cola inferior alfa/2)
chi_sd2<- qchisq(alfa/2, n-1)
chi_sd2

# Verificamos el criterio de rechazo 1
X2 < chi_sd2
#criterio de rechazo 2
X2 > chi_sd
#se rechaza la h0, significa que la variación del proceso es diferente a 0.36
#hay suficiente evidencia estadística para inferir que la hipótesis nula es falsa


#prueba de hipótesis de valor mayor 
#1. h0: sigma => 0.36
#2  h1: sigma < 0.36 

#3, 4 y 5 quedan igual

#6 se calcula chi cuadrada para alfa (ojo, alfa, no alfa/ 2, ya que se está validando una cola)
chi_sd2<- qchisq(alfa, n-1)
chi_sd2

#7 verificación
X2< chi_sd2
#se acepta la h0 que indica que el valor de sigma es mayor o igual a 0.36
#hay suficiente evidencia estadística para inferir que la hipótesis nula es falsa


#prueba de hipótesis de valor mayor 
#1. h0: sigma <= 0.36
#2  h1: sigma > 0.36 

#3, 4 y 5 quedan igual

#6 se calcula chi cuadrada para alfa (ojo, alfa, no alfa/ 2, ya que se está validando una cola)
chi_sd2<- qchisq(1-alfa, n-1)
chi_sd2

#7 verificación
X2> chi_sd2
#se rechaza la h0 que indica que el valor de sigma es menor o igual a 0.36
#no hay suficiente evidencia estadística para inferir que la hipótesis nula es falsa


###REGRESIÓN LINEAL****
?cars
data(cars)
#install.packages("car")
#install.packages("carData")
#install.packages("tidyverse")
#install.packages("nortest")


#calculamos la regresión para cars
?lm
#Tanto en plot como en lm se coloca el eje Y primero y el eje x después
lm.cars <- lm(dist~speed, data=cars)
summary (lm.cars)
plot (cars$dist ~cars$speed)
?abline
abline (lm.cars)


#son diferentes
plot (cars$speed ~cars$dist)
plot(x=cars$speed, y=cars$dist)
lm.cars <- lm(speed~dist, data=cars)
abline (lm.cars)

#calculamos regresión para iris
plot(iris)
data(iris)
lm.iris <- lm(Petal.Length~Petal.Width, data=iris)

summary (lm.iris)
plot(iris$Petal.Length~iris$Petal.Width)
abline(lm.iris)


#calculamos regresión para iris especies

#hay relación entre la especie y el largo de petalo? 
lm.iris.especie <-  lm(Petal.Length ~Species, data=iris)
summary(lm.iris.especie)
plot(iris$Petal.Length~iris$Species, type="l")
abline(lm.iris.especie)

lm.iris.especie2 <-  lm(Petal.Width ~Species, data=iris)
summary(lm.iris.especie2)
plot(iris$Petal.Width~iris$Species)
abline(lm.iris.especie2)


#como se ve cuando no hay relación
lm.iris.especie3 <- lm(Sepal.Length ~ Sepal.Width, data= iris)
summary(lm.iris.especie3)
plot(iris$Petal.Length~iris$Sepal.Length)
abline(lm.iris.especie3)
  
  