# 7. El archivo data credit card.csv tiene informacion de n=500 clientes de un banco, para las
# siguientes variables: purchases es el monto total de compras en el ultimo año, credit limit
# es el limite de credito disponible para el cliente, purchases freq es la proporcion de semanas
# del año en las que el cliente realizo compras y tenure es la cantidad de meses que restan al
# cliente para cancelar el credito. Se pide:

#install.packages('moments')
library(moments)

clientes <- read.csv('data_credit_card.csv')
purchases <- clientes$purchases
credit_limit <- clientes$credit_limit
purchases_freq <- clientes$purchases_freq
tenure <- clientes$tenure

# (a) Para todas las variables, graficar la funcion de distribucion empirica. Discutir sobre el
# tipo de variable aleatoria que utilizarıa para modelar en cada caso.

freq_abs <- function(df){
  par(mfrow = c(2,2))
  for(i in 1:ncol(df)) {
    plot(ecdf(df[, i]), main = names(df)[i], xlab = '', ylab = '')
  }
  mtext("Funcion de distribucion empirica", outer = TRUE, side = 1 , line=-2)
}

freq_abs(clientes)

# (b) Para la variable credit limit hacer un histograma y un grafico de densidad usando
# la funcion density, ¿Que observa? ¿Le parece adecuado realizar estos graficos para las
# variables purchases y tenure?

graficar_medias <- function(Data){
  abline(v = mean(Data), col = 'red', type='dashed')
  abline(v = median(Data), col = 'blue', type='dashed')
  abline(v = median(Data, 0.1), col = 'cyan3', type='dashed', lty=2)
  legend(x = "topright",
         c("Mean", "Median", "Median 0.1"),
         col = c("red", "blue", "cyan3"),
         lwd = c(2, 2, 2))
}

hist_y_dens <- function(Data, medidas = FALSE ) { # create a function with the name my_function
  hist(Data, xlab = '')
  if(medidas){
    graficar_medias(Data)
  }
  plot(density(Data), main = 'Density of credit_limit', xlab = '', ylab = 'Densidad')
  if(medidas){
    graficar_medias(Data)
  }
}

graficar_tabla <- function(Data1, Data2, Data3, medidas=FALSE) {
  par(mfrow = c(3,2))
  hist_y_dens(Data1, medidas)
  hist_y_dens(Data2, medidas)
  hist_y_dens(Data3, medidas)
}

graficar_tabla(credit_limit, purchases, tenure)

# (c) Para la variable tenure hacer un barplot con las frecuencias relativas de cada valor.
# ¿Que observa?

par(mfrow = c(1,1))
barplot(table(tenure)/length(tenure), main = 'Tenure')

# (d) Para todas las variables, calcular la media, la mediana y la media α−podada (con α =0.1). 
# Comparar los resultados y justificar. ¿Que medida de posicion del centro de
# los datos le parece mas adecuada en cada caso?

graficar_tabla(credit_limit, purchases, tenure, TRUE)

# (e) Para todas las variables, obtener los cuantiles de nivel 0.25 y 0.75 de los datos. Calcular
# el rango inter-cuartılico y la MAD muestrales. Graficar boxplots. ¿Que observa?

calcular_medidas <- function(Data){
  Q_25 <- c(credit_limit = quantile(credit_limit, 0.25), purchases = quantile(purchases, 0.25), purchases_freq = quantile(purchases_freq, 0.25), tenure = quantile(tenure, 0.25))
  Q_75 <- c(credit_limit = quantile(credit_limit, 0.75), purchases = quantile(purchases, 0.75), purchases_freq = quantile(purchases_freq, 0.75), tenure = quantile(tenure, 0.75))
  IQRs <- c(credit_limit=IQR(credit_limit), purchases=IQR(purchases), purchases_freq=IQR(purchases_freq), tenure=IQR(tenure))
  MADs <- c(credit_limit = mad(credit_limit), purchases = mad(purchases), purchases_freq=mad(purchases_freq), tenure=mad(tenure))
  desvio <- c(credit_limit = sd(credit_limit), purchases = sd(purchases), purchases_freq = sd(purchases_freq), tenure = sd(tenure))
  asimetria <- c(credit_limit = skewness(credit_limit), purchases = skewness(purchases), purchases_freq = skewness(purchases_freq), tenure = skewness(tenure))
  curtosis <- c(credit_limit = kurtosis(credit_limit), purchases = kurtosis(purchases), purchases_freq = kurtosis(purchases_freq), tenure = kurtosis(tenure))
  
  return(rbind(Q_25, Q_75, IQRs, MADs, desvio, asimetria, curtosis))
}

medidas <- calcular_medidas(clientes)
medidas[1:4,]

box_plots <- function(df){
  par(mfrow = c(2,2))
  for(i in 1:ncol(df)) {
    boxplot(df[,i], main = names(df)[i])
  }
  mtext("Boxplots", outer = TRUE, side = 1 , line=-2)
}
box_plots(clientes)

# (f) Calcular el desvıo estandar, el coeficiente de asimetria y el coeficiente de curtosis muestrales. Interpretar los resultados en relacion a las distribuciones vistas.

medidas[5:7,]

# (g) Identificar datos atipicos. ¿Deberian excluirse? ¿Como se modifican las medidas obtenidas
# anteriormente si se los excluye?

