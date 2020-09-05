#Tarea 1
#Byron Daniel Ramirez Tebalan
#Seccion A

#install.packages("readr")
library(readr)
carD <- read.csv("Tareas/Car.csv",header = TRUE)
View(carD)
summary(carD)
mean(carD$speed1)
library(modeest)
mfv(carD$type4)
library(psych)
sd(carD$pollution3)
sd(carD$pollution5)