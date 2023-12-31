fh_i <- function(x, i, h){
  n.x <- length(x)
  suma <- 0
  for(j in 1:n.x){
    if (j!=i){
      suma <- suma + dnorm((x[i]-x[j])/h)
    }
  }
  return((1/((n.x-1)*h)) * suma)
}

h.CV <- function(x,grilla.h){
  n.x <- length(x)
  n.h <- length(grilla.h)
  maxi <- -9999
  hmax <- NA
  for(j in 1:n.h) {
    h <- grilla.h[j]
    suma <- 0
    for (i in 1:n.x) {
      suma <- suma + log(fh_i(x,i,h))
    }
    suma <- suma * (1/n.x)
    if(suma>maxi){
      maxi <- suma
      hmax <- h
    }
  }
  return(hmax)
}

bw.loocv <- function(x, grilla.h=NA) {
  if (is.na(grilla.h)[1]) {
    grilla.h <- seq(bw.nrd0(x)/100, bw.nrd0(x)*100, length=100)
  }
  n.h <- length(grilla.h)
  n.x <- length(x)
  loglikes <- rep(NA, n.h)
  r <- matrix(NA, ncol = n.x, nrow = n.h)
  for(j in 1:n.h) {
    h <- grilla.h[j]
    for (i in 1:n.x) {
      r[j,i] <- log(fh_i(x,i,h))
    }
    loglikes[j] <- mean(r[j,])
  }
  
  hmax <- h.CV(x, grilla.h)
  
  return(list(h.opt=hmax, h.ret = grilla.h, loglikes=loglikes))
}

muestra <- function(seed, n=200) {
  set.seed(1234)
  binoms <- rbinom(n, size=1, p=0.75)
  return (
    binoms * rnorm(n, mean=0, sd=1)
    + (1 - binoms) * rnorm(n, mean=3.25, sd=sqrt(0.5))
  )
}
write.table(muestra(1234), "entrega1.txt", row.names=FALSE, col.names=FALSE)

x <- read.table('entrega1.txt')$V1
grilla.h <- seq(0.1,1,0.01)
datos <- bw.loocv(x,grilla.h)

plot(grilla.h, datos$loglikes, type = "l", ylab = "Promedio", xlab = "Ventana h", main = "Comparacion Log-verosimilitud")
abline(v = datos$h.opt, col = "blue")
abline(v = bw.nrd0(x), col = "red")
abline(v = bw.SJ(x), col = "green")
abline(v = bw.ucv(x), col = "purple")
legend("topright", legend=c("h.CV", "Silverman","Sheather & Jones","VC insesgada R"), fill=c("blue", "red","green","purple"), col=c("blue", "red","green","purple"), border="black")

plot(density(x), lwd=3, ylab = "", xlab = "", main = "Estimacion de la densidad", ylim =c(0,0.35))
lines(density(x,bw=as.numeric(datos$h.opt)),col='blue')
lines(density(x,bw=bw.nrd0(x)),col='red')
lines(density(x,bw=bw.SJ(x)),col='green')
lines(density(x,bw=bw.ucv(x)),col='purple')
legend("topright", legend=c("Densidad R","h.CV", "Silverman","Sheather & Jones","VC insesgada R"), fill=c("black","blue", "red","green","purple"), col=c("black", "blue", "red","green","purple"), border="black")
