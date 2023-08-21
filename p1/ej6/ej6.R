# 6. Con la finalidad de incrementar las lluvias en zonas deserticas, se desarrollo un metodo que
# consiste en el bombardeo de la nube con atomos. Para evaluar la efectividad del metodo se
# realizo el siguiente experimento:
#   Para cada nube que se podıa bombardear se decidio al azar si se la trataba o no.
# Las nubes no tratadas fueron denominadas nubes controles.
# En el archivo nubes.txt se presentan la cantidad de agua caıda de 26 nubes tratadas y 26
# nubes controles.

# (a) Realizar boxplots paralelos. ¿Le parece que el metodo produce algun efecto?

nubes <- read.table('nubes.txt', header = TRUE)

tratadas <- nubes$TRATADAS
controles <- nubes$CONTROLES

rango <- range(c(tratadas, controles))
par(mfrow = c(1, 2))
boxplot(tratadas, sub = "Tratadas", ylim = rango)
boxplot(controles, sub = "Controles", ylim = rango)
mtext("Cantidad de agua caida", outer = TRUE, side =3 , line=-2)

# (b) Analizar la normalidad realizando qqplots e histogramas (de densidad) para ambos
# conjuntos de datos y superponiendo la curva normal.

rango <- range(c(tratadas, controles))

par(mfrow = c(1, 2))
qqnorm(tratadas, main = 'Tratadas')
qqline(tratadas, col = "red", sub = "Tratadas")
qqnorm(controles, main = 'Controles')
qqline(controles, col = "red", sub = "Controles")
mtext("Q-Q Plots", outer = TRUE, side =3 , line=-2)

par(mfrow = c(1, 2))
xT <- seq(min(rango), max(rango), length = 40)
curvaT <- dnorm(xT, mean = mean(tratadas), sd = sd(tratadas))
hist(tratadas, main = NULL, xlab = 'Lluvia tratadas', ylab = 'Densidad', probability = TRUE, xlim = rango, breaks = 10)
lines(xT, fun, col = 2, lwd = 2)
xC <- seq(min(rango), max(rango), length = 40)
curvaC <- dnorm(xT, mean = mean(controles), sd = sd(controles))
hist(controles, main = NULL, xlab = 'Lluvia controles', ylab = 'Densidad', probability = TRUE, xlim = rango, breaks = 10)
lines(xC, fun, col = 2, lwd = 2)
mtext("Histogramas de Densidad", outer = TRUE, side =3 , line=-2)

# c) Realizar la transformacion logaritmo natural a los datos (log en R) y repetir b) para
# los datos transformados.

tratadasL <- log(tratadas)
controlesL <- log(controles)
rangoL <- log(rango)

par(mfrow = c(1, 2))
qqnorm(tratadasL, main = 'Tratadas')
qqline(tratadasL, col = "red", sub = "Tratadas")
qqnorm(controlesL, main = 'Controles')
qqline(controlesL, col = "red", sub = "Controles")
mtext("Q-Q Plots (escala logaritmica)", outer = TRUE, side =3 , line=-2)

par(mfrow = c(1, 2))
xT <- seq(min(rangoL), max(rangoL), length = 40)
curvaT <- dnorm(xT, mean = mean(tratadasL), sd = sd(tratadasL))
hist(tratadasL, main = NULL, xlab = 'Lluvia tratadas', ylab = 'Densidad', probability = TRUE, xlim = rangoL, breaks = 10)
lines(xT, curvaT, col = 2, lwd = 2)
xC <- seq(min(rangoL), max(rangoL), length = 40)
curvaC <- dnorm(xC, mean = mean(controlesL), sd = sd(controlesL))
hist(controlesL, main = NULL, xlab = 'Lluvia controles', ylab = 'Densidad', probability = TRUE, xlim = rangoL, breaks = 10)
lines(xC, curvaC, col = 2, lwd = 2)
mtext("Histogramas de Densidad (escala logaritmica)", outer = TRUE, side =3 , line=-2)

# d) Realizar boxplots paralelos habiendo transformado las variables con el logaritmo natural. 
# Observar como se modificaron los datos atipicos respecto del item a).

par(mfrow = c(1, 2))
boxplot(tratadasL, sub = "Tratadas", ylim = rangoL)
boxplot(controlesL, sub = "Controles", ylim = rangoL)
mtext("Cantidad de agua caida (escala logaritmica)", outer = TRUE, side =3 , line=-2)