#Examen FInal 
#BYRON DANIEL RAMÍREZ TEBALÁN
#999003721

#Pregunta 11 y 12
# Se desea estimar con un nivel de confianza del 95 % la talla media de los hombres de 18 o más años de un país
media <- 173.47
n <-15
desv <- 4
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


#Pregunta 13
media <- 173.47
n <-15
desv <- 4
alfa <- 0.2/2
nivelconfianza<- 1-alfa

normal<- qnorm(nivelconfianza,0,1)
normal

error<-  desv/sqrt(n)
error
margen <- normal*error
margen

#Pregunta 18
#Ho: u >= 800
#H1: u <= 800

alfa <- 0.01
n=50
media= 750
sd= 120
mu= 800

#¿Cuál es el valor del estadístico de prueba? 
z0<- (media-mu)/(sd/sqrt(n))
z0

zAlfa<- qnorm(alfa,0,1, lower.tail = TRUE)
zAlfa

z0 <  zAlfa


#Pregunta 20
#La St. Louis Metro Bus Company de Estados Unidos, desea dar una imagen de confiabilidad haciendo que sus conductores sean puntuales en los horarios de llegada a las paradas. La empresa desea que haya poca variabilidad en dichos tiempos. 
#h0: sigma<=4
#h1: sigma > 4

n= 24
s2 <- 4.9
sigma20<- 4
alfa <- 0.05


#Pregunta 21
#¿Cuál es el valor de chi-cuadrado?
chi_sd<- qchisq(1- alfa, n-1)
chi_sd

#Pregunta 22
#Estadistico
X2 <- ((n-1)*s2)/sigma20
X2

#Pregunta 24
#¿Se rechaza o se acepta la H0?
X2> chi_sd


#Pregunta 25
#volumen (x) Height(y) 
data(trees)
lm.trees <- lm(Height~Volume, data=trees)
summary (lm.trees)

#Pregunta 26, 27 y 28
#volumen (x) Girth(y) 
data(trees)
lm.trees1 <- lm(Girth~Volume, data=trees)
summary (lm.trees1)

#Pregunta 29
#volumen (x) Girth(y) 
plot (trees$Girth ~trees$Volume)
abline (lm.trees1)



