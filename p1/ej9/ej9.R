# 9. Considerar nuevamente el conjunto de datos del ejercicio 1.

estudio <- read.csv('Debernardi.csv')

# a) Realizar histogramas para la variable LYVE1 basados en los datos brindados para
# las observaciones que cumplen diagnosis=1, diagnosis=2 y diagnosis=3. Es decir
# efectuar histogramas segun los niveles de la variable factor diagnosis. Indicar las
# caracteristicas mas sobresalientes de los histogramas y aquellas que los diferencian.

d1 <- estudio[estudio$diagnosis==1,]
d2 <- estudio[estudio$diagnosis==2,]
d3 <- estudio[estudio$diagnosis==3,]

rango <- range(estudio$LYVE1)

par(mfrow = c(3,1))
hist(d1$LYVE1, xlab = 'Diagnostico 1', main = 'Histograma LYVE1', xlim = rango, breaks = 10)
hist(d2$LYVE1, xlab = 'Diagnostico 2', main = NULL, xlim = rango, breaks = 10)
hist(d3$LYVE1, xlab = 'Diagnostico 3', main = NULL, xlim = rango, breaks = 10)

# b) Graficar, en distintos colores y superpuestas, las funciones de distribucion empiricas
# de la variable LYVE1 segun los niveles de la variable factor diagnosis. Decidir si la
# siguiente afirmacion es verdadera o falsa y justificar: “los valores de la variable LYVE1
# tienden a ser mas altos entre quienes tienen cancer de pancreas que entre quienes sufren
# otras enfermedades asociadas al pancreas”.

par(mfrow = c(3,1))
plot(ecdf(d1$LYVE1), main = 'Funcion de distribucion empirica LYVE1', xlab = 'Diagnostico 1', xlim = rango)
plot(ecdf(d2$LYVE1), main = NULL, xlab = 'Diagnostico 2', xlim = rango)
plot(ecdf(d3$LYVE1), main = NULL, xlab = 'Diagnostico 3', xlim = rango)

# c) Realizar boxplots paralelos para la variable LYVE1 segun los niveles de la variable
# factor diagnosis, considerando el sexo de los pacientes (variable sex). Decidir si la
# siguiente afirmacion es verdadera o falsa y justificar: “en terminos generales, el sexo del
# paciente no afecta los niveles de la proteina que se mide en la variable LYVE1”.

par(mfrow = c(3,2))
boxplot(d1[d1$sex == 'M',]$LYVE1, main = 'Boxplot LYVE1 Hombres', xlab = 'Diagnostico 1', ylim = rango)
boxplot(d1[d1$sex == 'F',]$LYVE1, main = 'Boxplot LYVE1 Mujeres', xlab = 'Diagnostico 1', ylim = rango)
boxplot(d2[d2$sex == 'M',]$LYVE1, main = NULL, xlab = 'Diagnostico 2', ylim = rango)
boxplot(d2[d2$sex == 'F',]$LYVE1, main = NULL, xlab = 'Diagnostico 2', ylim = rango)
boxplot(d3[d3$sex == 'M',]$LYVE1, main = NULL, xlab = 'Diagnostico 3', ylim = rango)
boxplot(d3[d3$sex == 'F',]$LYVE1, main = NULL, xlab = 'Diagnostico 3', ylim = rango)


# d) Graficar superpuestas las densidades estimadas, que brinda la funcion density, para la
# variable LYVE1 segun los niveles de la variable factor diagnosis. 
# Describir las caracteristicas mas sobresalientes de las densidades estimadas y aquellas que las diferencian.

par(mfrow = c(3,1))
plot(density(d1$LYVE1), main = 'Densidades estimadas de LYVE1', xlab = 'Diagnostico 1', xlim = rango)
plot(density(d2$LYVE1), main = '', xlab = 'Diagnostico 2', xlim = rango)
plot(density(d3$LYVE1), main = '', xlab = 'Diagnostico 3', xlim = rango)

# e) Repetir a) y d) para el logaritmo de LYVE1.

par(mfrow = c(3,1))
hist(log(d1$LYVE1), xlab = 'Diagnostico 1', main = 'Histograma LYVE1 (escala logaritmica)', xlim = log(rango))
hist(log(d2$LYVE1), xlab = 'Diagnostico 2', main = NULL, xlim = log(rango))
hist(log(d3$LYVE1), xlab = 'Diagnostico 3', main = NULL, xlim = log(rango))

par(mfrow = c(3,1))
plot(density(log(d1$LYVE1)), main = 'Densidades estimadas de LYVE1 (escala logaritmica)', xlab = 'Diagnostico 1', xlim = log(rango))
plot(density(log(d2$LYVE1)), main = '', xlab = 'Diagnostico 2', xlim = log(rango))
plot(density(log(d3$LYVE1)), main = '', xlab = 'Diagnostico 3', xlim = log(rango))
