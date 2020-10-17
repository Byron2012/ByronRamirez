#Ejercicio 1
#prueba de hipotesis para valores mayores
#1 h0: MU >= 500
#2 h1:  Mu < 500

#3- Alfa
alfa <- 0.05

n=100
media= 499.4
sd= 5
mu=500

#4- estadistico
z0<- (media-mu)/(sd/sqrt(n))
z0


#5- valor de la normal al ser mas de 30 elementos en la muestra con alfa = 5%
zAlfa<- qnorm(alfa,0,1, lower.tail = TRUE)
zAlfa


#7 analizando el criterio de rechazo. 
z0 <  zAlfa
#-1.2<-1.64 (ES FALSO)
#se acepta la hipotesis nula, donde se define la media como mayor o igual a 500
#no hay suficiente evidencia estadística para inferir que la hipótesis nula es falsa


#Ejercicio 2
#prueba de hipotesis para valores mayores
#1 h0: MU =< 40
#2 h1:  Mu > 40

#3- Alfa
alfa <- 0.01

n=8
media= 42
sd= 2  #(muestral)
mu=40

#4- estadistico
z0<- (media-mu)/(sd/sqrt(n))
z0


#Valor de t student al ser menos de 30 elementos en la muestra
zAlfa <- qt((1-alfa),(n-1))

zAlfa


#7 analizando el criterio de rechazo. 
z0 >  zAlfa
#2.82 > 2.99 (ES FALSO)
#se acepta la hipotesis nula, donde se define la media como igual a 40 
#no hay suficiente evidencia estadística para inferir que la hipótesis nula es falsa