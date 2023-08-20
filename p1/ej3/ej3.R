iridio <- read.table(paste0("iridio.txt"), header = TRUE)
rodio <- read.table(paste0("rodio.txt"), header = TRUE)

iridio <- iridio$iridio
rodio <- rodio$rodio

# (a) Comparar los dos conjuntos de datos mediante histogramas y boxplots, graficando los boxplots en paralelo.

hist(iridio, main = "Histograma de temperatura de sublimación del iridio")
boxplot(iridio, main = "Boxplot de temperatura de sublimación del iridio")

hist(rodio, main = "Histograma de temperatura de sublimación del rodio")
boxplot(rodio, main = "Boxplot de temperatura de sublimación del rodio")

par(mfrow = c(1, 2))
boxplot(iridio, sub = "Iridio")
boxplot(rodio, sub = "Rodio")

rango <- range(c(rodio, iridio))
par(mfrow = c(1, 2))
boxplot(iridio, sub = "Iridio", ylim = rango)
boxplot(rodio, sub = "Rodio", ylim = rango)
mtext("Boxplots de temperatura de sublimación", outer = TRUE, side =3 , line=-2)

# (b) Hallar las medias, las medianas y las medias podadas al 10% y 20% muestrales. Comparar.

media_iridio <- mean(iridio)
media_rodio <- mean(rodio)

mediana_iridio <- median(iridio)
mediana_rodio <- median(rodio)

media_podadas_10_iridio <- mean(iridio, 0.1)
media_podadas_10_rodio <- mean(rodio, 0.1)

# (c) Hallar los desvíıos estándares, las distancias intercuartiles y las MAD muestrales como medidas de dispersión.

desvios <- c(iridio = sd(iridio), rodio = sd(rodio))
IQRs <- c(iridio = IQR(iridio), rodio = IQR(rodio))
MADs <- c(iridio = mad(iridio), rodio = mad(rodio))

# (d) Hallar los cuantiles 0.90, 0.75, 0.50, 0.25 y 0.10.
qs <- c(0.90, 0.75, 0.50, 0.25, 0.10)

df_cuantiles <- data.frame(
  iridio = quantile(iridio, qs),
  rodio = quantile(rodio, qs)
)

df_cuantiles