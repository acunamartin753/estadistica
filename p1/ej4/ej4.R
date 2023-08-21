# 4. En un estudio nutricional se consideran las calorias y el contenido de sodio de tres tipos de
# salchichas y se obtuvieron los datos que se encuentran en los archivos salchichas A.txt,
# salchichas B.txt y salchichas C.txt.

# (a) Armar un archivo que se llame salchihas.txt que contenga toda la informacion registrada en los tres archivos mencionados agregando una columna que indique el tipo de
# salchicha en cada caso.

A <- read.table('salchichas_A.txt', header = TRUE)
B <- read.table('salchichas_B.txt', header = TRUE)
C <- read.table('salchichas_C.txt', header = TRUE)

A['Tipo'] <- rep('A', nrow(A))
B['Tipo'] <- rep('B', nrow(B))
C['Tipo'] <- rep('C', nrow(C))

colnames(A) <- c('Calorias','Sodio','Tipo')
colnames(B) <- c('Calorias','Sodio','Tipo')
colnames(C) <- c('Calorias','Sodio','Tipo')

salchichas <- rbind(A,B,C)

write.table(salchichas, "salchichas.txt")

# (b) Realizar un histograma para las calorias de cada tipo de salchichas. ¿Observa grupos
# en algun grafico? ¿Cuantos grupos observa? ¿Observa algun candidato a dato atıpico?
# ¿Alguno de los histogramas tiene una caracterıstica particular?

rangoCal <- range(c(salchichas[salchichas$Tipo=="A",]$Calorias, salchichas[salchichas$Tipo=="B",]$Calorias, salchichas[salchichas$Tipo=="C",]$Calorias))

par(mfrow = c(1, 3))
hist(salchichas[salchichas$Tipo=="A",]$Calorias, main = "Tipo A", xlab = NULL, xlim = rangoCal, breaks = 10, ylim = range(1,6))
hist(salchichas[salchichas$Tipo=="B",]$Calorias, main = "Tipo B", xlab = NULL, xlim = rangoCal, breaks = 10, ylim = range(1,6))
hist(salchichas[salchichas$Tipo=="C",]$Calorias, main = "Tipo C", xlab = NULL, xlim = rangoCal, breaks = 10, ylim = range(1,6))
mtext("Histogramas de Calorias", outer = TRUE, side =1 , line=-2, cex = 1.5)

# (c) Realizar los boxplots paralelos para las calorias. ¿Observa la misma cantidad de grupos
# que antes? ¿A cual conclusion llega? De acuerdo con los boxplots graficados, ¿como
# caracterizaria la diferencia entre los tres tipos de salchichas desde el punto de vista de
# las calorias?

par(mfrow = c(1, 3))
boxplot(salchichas[salchichas$Tipo=="A",]$Calorias, main = "Tipo A", xlab = NULL, ylim = rangoCal)
boxplot(salchichas[salchichas$Tipo=="B",]$Calorias, main = "Tipo B", xlab = NULL, ylim = rangoCal)
boxplot(salchichas[salchichas$Tipo=="C",]$Calorias, main = "Tipo C", xlab = NULL, ylim = rangoCal)
mtext("Boxplots de Calorias", outer = TRUE, side =1 , line=-2, cex = 1.5)

#  (d) Repetir con la cantidad de sodio.

rangoSod <- range(c(salchichas[salchichas$Tipo=="A",]$Sodio, salchichas[salchichas$Tipo=="B",]$Sodio, salchichas[salchichas$Tipo=="C",]$Sodio))

range(1,5)
par(mfrow = c(1, 3))
hist(salchichas[salchichas$Tipo=="A",]$Sodio, main = "Tipo A", xlab = NULL, xlim = rangoSod, breaks = 10, ylim = range(1,6))
hist(salchichas[salchichas$Tipo=="B",]$Sodio, main = "Tipo B", xlab = NULL, xlim = rangoSod, breaks = 10, ylim = range(1,6))
hist(salchichas[salchichas$Tipo=="C",]$Sodio, main = "Tipo C", xlab = NULL, xlim = rangoSod, breaks = 10, ylim = range(1,6))
mtext("Histogramas de Sodio", outer = TRUE, side =1 , line=-2, cex = 1.5)

par(mfrow = c(1, 3))
boxplot(salchichas[salchichas$Tipo=="A",]$Sodio, main = "Tipo A", xlab = NULL, ylim = rangoSod)
boxplot(salchichas[salchichas$Tipo=="B",]$Sodio, main = "Tipo B", xlab = NULL, ylim = rangoSod)
boxplot(salchichas[salchichas$Tipo=="C",]$Sodio, main = "Tipo C", xlab = NULL, ylim = rangoSod)
mtext("Boxplots de Sodio", outer = TRUE, side =1 , line=-2, cex = 1.5)

