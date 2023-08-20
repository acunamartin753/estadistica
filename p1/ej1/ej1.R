# 1. El archivo Debernardi.csv contiene los datos referentes a un estudio acerca del cancer de
# pancreas (mas informacion en el archivo Acerca de los datos, en el Aula Virtual).

# (a) Construir una tabla con los valores observados para la variable diagnosis y su frecuencia relativa.

estudio <- read.csv('Debernardi.csv',header = TRUE)
Freq <- table(estudio$diagnosis)
FreqRel <- Freq/sum(Freq)
diagnosis <- cbind(Freq, FreqRel)
diagnosis

# (b) Realizar un grafico de barras usando la tabla del item anterior.

par(mfrow = c(1, 2))
barplot(diagnosis[,1],xlab = "Frecuencia absoluta")
barplot(diagnosis[,2], xlab = "Frecuencia relativa")
mtext("Diagnosticos", outer = TRUE, side =3 , line=-2)
