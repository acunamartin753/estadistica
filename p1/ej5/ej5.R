estudiantes <- read.table(paste0("estudiantes.txt"), header = TRUE)

# (a) Estudiar si la distribución de los conjuntos de datos para ambos grupos es normal, realizando los correspondientes histogramas y superponiendo la curva normal. Además dibujar los qqplots para cada conjunto de datos superponiendo, en otro color, la recta mediante el comando qqline.

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

# (b) ¿Le parece a partir de estos datos que ambos grupos est´an midiendo lo mismo? Responder comparando medidas de centralidad y de dispersi´on de los datos. Hacer boxplotsparalelos.

