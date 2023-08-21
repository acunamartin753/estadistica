# 2. El archivo datos titanic.csv contiene informacion sobre una muestra seleccionada al azar
# de las personas, no tripulantes, que viajaban en el barco tristemente celebre Titanic, al
# momento de su hundimiento en el Oceano Atlantico (mas informacion en el archivo Acerca
# de los datos, en el Aula Virtual).

# (a) Estimar la probabilidad de ser mujer sabiendo que sobrevivio y comparar con la estimacion de ser mujer a bordo del Titanic.

titanic <- read.csv('datos_titanic.csv')
# Probabilidad de ser mujer dado que sobrevivio
sum(titanic$Sex == "female" & titanic$Survived==1)/sum(titanic$Survived==1)
# Probabilidad de ser mujer
sum(titanic$Sex=="female")/NROW(titanic)

# (b) Hacer una tabla de contingencia entre las variables categoricas Survived y Pclass. A
# partir de esta tabla estimar la probabilidad de sobrevivir dada la clase para los distintos
# valores de la variable Pclass.

sobrevivientes <- titanic$Survived==1
totalSobrevivientes <- sum(sobrevivientes)

CantidadABordo <- table(titanic$Pclass)
Sobreviven <- table(titanic[sobrevivientes,]$Pclass)
ProbaSobrevivir <- Sobreviven / totalSobrevivientes

clases <- cbind(CantidadABordo, Sobreviven, ProbaSobrevivir)
clases

# (c) Realizar un grafico de barras que vincule a las variables categoricas Survived y Pclass.

par(mfrow = c(1, 2))
barplot(clases[,1], ylab = 'Cantidad a bordo', col = 'red')
barplot(clases[,2], add = TRUE, col = 'green', legend.text = 'sobreviven')
barplot(clases[,3], ylab = 'Probabilidad supervivencia', col = 'yellow',)
mtext("Clases", outer = TRUE, side =1 , line=-2)

