# 8. En el archivo ciclocombinado.xlsx hay datos de la potencia entregada por una central
# termica de ciclo combinado. Se registraron datos diarios de la potencia maxima entregada
# (PE, en MW) por la planta funcionando en capacidad maxima. La variable HighTemp vale
# 1 si la temperatura media diaria fue superior a 20◦C en el dia en el que se tomo el dato y
# vale 0 en caso contrario.

#install.packages("readxl")
library("readxl")
cicloComb <- read_excel('ciclocombinado.xlsx')

# (a) Realizar un histograma y un grafico density con los datos de PE, ¿Que se observa?

par(mfrow=c(1,2))
hist(cicloComb$PE, main = NULL, xlab = NULL)
hist(cicloComb$PE, probability = TRUE, main = NULL, xlab = NULL)
mtext("PE", outer = TRUE, side = 1 , line=-2)

# (b) Clasificar los datos en dos vectores segun la variable HighTemp y realizar graficos
# density separados. Visualizar simultaneamente los graficos en la misma escala. ¿Que se
# observa?

high <- cicloComb[cicloComb$HighTemp==1,1]
low <- cicloComb[cicloComb$HighTemp==0,1]

rango <- range(cicloComb$PE)
par(mfrow = c(2,1))
hist(low$PE, probability = TRUE, main = NULL, xlab = 'Low', breaks = 10, xlim = rango)
hist(high$PE, probability = TRUE, main = NULL, xlab = 'High', breaks = 10, xlim = rango)
mtext("PE", outer = TRUE, side = 3, line = -2)

# (c) Estimar P(PE < 450|HighTemp = 0) y P(PE < 300|HighTemp = 1).

proba1 <- sum(low$PE<450) / nrow(low)
proba2 <- sum(high$PE<300) / nrow(high)

# (d) Estimar P(PE < 450).

proba3 <- sum(high$PE<450) / nrow(high)
proba4 <- proba1 * nrow(low) / nrow(cicloComb) + proba3 * nrow(high) / nrow(cicloComb) 

# (e) Estimar la potencia minima garantizada con probabilidad 0.9 para un cierto dia con
# Hightemp = 1.

quantile(high$PE, 0.1)

# (f) Estimar la potencia minima garantizada con probabilidad 0.9 para un cierto dia.

quantile(high$PE, 0.1) * nrow(high) / nrow(cicloComb) + quantile(low$PE, 0.1) * nrow(low) / nrow(cicloComb)

