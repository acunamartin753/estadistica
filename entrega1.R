# Punto 3
fh_i <- function(x, xi, h){
  n <- length(x)
  y <- rep(xi,n)
  return (1/((n-1)*h)) * sum(dnorm((y-x)/h))
}

# Punto 4
hcv <- function(x,grilla.h){
  n.x <- length(x)
  max <- -99999
  hmax <- NA
  for(j in 1:n.h) {
    h <- grilla.h[j]
    suma <- rep(0,n.x)
    for (i in 1:n.x) {
      suma[i] <- 1/n.x * sum(log(fh_i(x,x[i],h)))
    }
  }
  
}


bw.loocv <- function(x, grilla.h=NA) {
  if (is.na(grilla.h)) {
    grilla.h <- seq(bw.nrd0(x)/100, bw.nrd0(x), length=100)   # Punto 2
  }
  n.h <- length(grilla.h)
  n.x <- length(x)
  loglikes <- rep(NA, n)
  # Punto 3
  r <- matrix(0, ncol = n.h, nrow = n.x)
  for(j in 1:n.h) {
    h <- grilla.h[j]
    # Punto 3
    prom <- rep(0,n.x)
    for (i in 1:n.x) {
      
      # Punto 3
      r[j,i] <- fh_i(x,x[i],h)
    }
    prom[j] <- mean(r[j,])
  }
  
  return(list(h.opt=NA, h.ret = NA, loglikes=NA))
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

