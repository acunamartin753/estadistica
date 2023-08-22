# 5. El conjunto de datos que figura en el archivo estudiantes.txt corresponde a 100 determinaciones repetidas de la concentracion de ion nitrato (en µg/l), 50 de ellas corresponden a
# un grupo de estudiantes (Grupo 1) y las restantes 50 a otro grupo (Grupo 2).

estudiantes <- read.table(paste0("estudiantes.txt"), header = TRUE)

# (a) Estudiar si la distribución de los conjuntos de datos para ambos grupos es normal, realizando los correspondientes histogramas y superponiendo la curva normal. Además dibujar los qqplots para cada conjunto de datos superponiendo, en otro color, la recta mediante el comando qqline.

GRUPO1 <- estudiantes$GRUPO1
GRUPO2 <- estudiantes$GRUPO2

par(mfrow=c(1,2))
hist(GRUPO1, breaks = 10)
hist(GRUPO2, breaks = 20)

(t1 <- table(GRUPO1))
length(t1)
t2 <- table(GRUPO2)
length(t2)

grupos <- list("G1"=GRUPO1, "G2"=GRUPO2)
names(grupos)

par(mfrow=c(1,2))
for (num in seq_along(grupos)) {
  grupo <- grupos[[num]]
  hist(grupo, freq=FALSE, main=paste("Grupo", num))
  curve(dnorm(x, mean = mean(grupo), sd = sd(grupo)), add = TRUE, col = "blue")
}


#grupos_bis <- list(G1=GRUPO1, "G2"=GRUPO2)
#stopifnot(identical(grupos, grupos_bis))

for (key in names(grupos)) {
  value <- grupos[[key]]
  qqnorm(value, main = key)
  qqline(value, col = "red")
}

# Con ggplot2
library(ggplot2)

ggplot(data.frame(sample = GRUPO1), aes(sample = sample)) +
  stat_qq() +
  stat_qq_line(color="red")

# A mano
GRUPO1_ordenado <- sort(GRUPO1)

n <- length(GRUPO1)
cuantiles <- qnorm((seq_len(n) - 0.5)/ n)
dnorm(cuantiles)
plot(cuantiles, GRUPO1_ordenado, main = "A mano")
qqnorm(GRUPO1, main = "Con qqnorm")
qqline(GRUPO1, col = "red")

# (b) ¿Le parece a partir de estos datos que ambos grupos estan midiendo lo mismo? Responder comparando medidas de centralidad y de dispersion de los datos. Hacer boxplots paralelos.

#install.packages('moments')
library(moments)

media <- c(G1 = mean(GRUPO1), G2 = mean(GRUPO2))
mediana <- c(G1 = median(GRUPO1), G2 = median(GRUPO2))
mediana_0.1 <- c(G1 = median(GRUPO1), G2 = median(GRUPO2))
Q_25 <- c(G1 = quantile(GRUPO1, 0.25), G2 = quantile(GRUPO2, 0.25))
Q_75 <- c(G1 = quantile(GRUPO1, 0.75), G2 = quantile(GRUPO2, 0.75))
IQRs <- c(G1 = IQR(GRUPO1), G2 = IQR(GRUPO2))
MADs <- c(G1 = mad(GRUPO1), G2 = mad(GRUPO2))
desvio <- c(G1 = sd(GRUPO1), G2 = sd(GRUPO2))
asimetria <- c(G1 = skewness(GRUPO1), G2 = skewness(GRUPO2))
curtosis <- c(G1 = kurtosis(GRUPO1), G2 = kurtosis(GRUPO2))

medidas <- rbind(media, mediana, mediana_0.1, Q_25, Q_75, IQRs, MADs, desvio, asimetria, curtosis)
medidas

rango <- range(c(GRUPO1, GRUPO2))
par(mfrow = c(1,2))
boxplot(GRUPO1, ylim = rango, xlab = 'Grupo 1', ylab = 'Concentracion nitrato')
boxplot(GRUPO2, ylim = rango, xlab = 'Grupo 2')
mtext("Boxplots concentraciones de nitrato por grupo", outer = TRUE, side = 3 , line=-2)

