#PARCIAL 1
#Byron Daniel Ramirez Tebalan 999003721

airquality
#Pregunta 1
#Utilizando el conjunto de datos "airquality" ¿Cuál es la media, mediana y moda de la variable Temp?
summary(airquality)
mean(airquality$Temp)
median(airquality$Temp, na.rm = TRUE)
library(modeest)
mfv(airquality$Temp)


#Pregunta 2
#Utilizando el conjunto de datos "airquality" grafique el histograma de la variable Ozono e indique qué tipo de asimetría tiene
plot(airquality$Ozone)
hist(x = airquality$Ozone)

#Pregunta 3
#Utilizando el conjunto de datos "airquality" ¿Cuál es la media, mediana y moda de la variable Wind?
summary(airquality)
mean(airquality$Wind)
median(airquality$Wind)
mfv(airquality$Wind)

#Pregunta 4
#Utilizando el conjunto de datos "airquality" calcule el grado de simetría de la variable SolarR. ¿Qué significa el valor de su simetría?
library(psych)
table(airquality$Solar.R)
skew(airquality$Solar.R)

#Pregunta 5
#Utilizando el conjunto de datos "airquality" indique qué desviación estándar es más adecuada. ¿La de Wind o la de Ozono?
sd(airquality$Ozone, na.rm = TRUE)
sd(airquality$Wind, na.rm = TRUE)



#GRAFICAS
#PARTE 2 - PARCIAL 1
library(ggplot2)

diamonds
plot(x=diamonds$carat, y=diamonds$price)
ggplot(data = diamonds, aes(x = carat, y = price)) + 
  geom_boxplot(aes(color = carat), alpha = 0.7) + 
  theme_minimal()


hist(diamonds$carat)
ggplot(diamonds) +
  geom_histogram(binwidth = 1, aes(x = carat), fill = 'steelblue') + 
  theme_minimal()

plot(x=diamonds$carat, y=diamonds$price, main = "carat", xlab = "carat", ylab = "price")
       
plot(x=diamonds$color)



#PROBABILIDAD
#PARTE 3 - PARCIAL 1

xpoisson= 1:24
lambda= 20
distpoison <- dpois(1:24, 20)
distpoison

dpois(1:24,20)
ppois(24,20)
barplot(distpoison)
plot(xpoisson, distpoison, type = "h", col=c("orange", "yellow", "gray"), xlab = xpoisson)



#sesgo 
n= 20
p= 0.6
x=0:n; 
prob=dbinom(x,n,p);
prob
barplot(prob, main = "distribucion binomial sesgo positivo", names.arg = x)
