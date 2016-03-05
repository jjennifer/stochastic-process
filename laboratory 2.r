#-----------------------------------------------
# LABORATORY SESSION 2

# Author  : Joan Martinez
#Goals    : 1 We program two states of a weather day outcome.
#           2 We program the states and stochastic process of a flip coin.
#           3 Next, a random walk proces
#-----------------------------------------------



# Part 1a ----------------------------------------------------------------------
A = matrix(data=c(0.9, 0.5, 0.1, 0.5),
           nrow=2, ncol=2)
rownames(A) = estados
colnames(A) = estados

estados = c("nublado", "soleado")
p0 = c(nublado=0.9, soleado=0.1)
N = 128
  
x = numeric()
x[1] = sample(estados, size=1, prob=p0)
for(i in 1:N) {
  x[i+1] = sample(estados, size=1, prob=A[x[i], ])
}

# Part 1b ----------------------------------------------------------------------
N = 10
B = matrix(data=c(1, 0, 0, 0, 0, 0,
                  0.5, 0, 0.5, 0, 0, 0,
                  0, 0.5, 0, 0.5, 0, 0,
                  0, 0, 0.5, 0, 0.5, 0,
                  0, 0, 0, 0.5, 0, 0.5,
                  0, 0, 0, 0, 0, 1),
           nrow=6, ncol=6, byrow=TRUE)
estados = 0:5
x = numeric()
x[1] = 2
for(i in 1:N) {
  x[i+1] = sample(estados, size=1, prob=B[x[i]+1, ])
}

plot(x, type="s", ylab="ganancia")

# Part 2a (new) ----------------------------------------------------------------------
estados = c("cara", "sello")
p0 = c(cara=0.5, sello=0.5)

duracion = numeric()

N = 50
for(j in 1:N) {
  print(sprintf("Apuesta %d", j))
  ganancia = 2
  trayectoria = numeric()
  trayectoria[1] = ganancia
  i = 1
  # != : diferente de, == : igual a
  while(ganancia>0 & ganancia < 5) {
    
    moneda = sample(estados, 1, prob=p0)
    
    if(moneda=="cara") {
      ganancia = ganancia + 1
    } else {
      ganancia = ganancia - 1 
    }
    
    trayectoria[i+1] = ganancia  
    i = i + 1
  }
  duracion[j] = length(trayectoria)
}



# Part 2b ----------------------------------------------------------------------

estados = 1:4
P = matrix(data=c(0.5, 0.5, 0, 0,
                  0.5, 0.5, 0, 0,
                  0, 0, 0.3, 0.7,
                  0, 0, 0.7, 0.3),
           nrow=4, ncol=4, byrow=TRUE)
rownames(P) = estados
colnames(P) = estados

p0 = c(E1=0.5, E2=0, E3=0, E4=0.5)

N = 128 # 128 réplicas
T = 200 # 100 pasos de tiempo

x = matrix(ncol=N, nrow=T+1)

for(j in 1:N) {
#   x = numeric(T+1)
  x[1, j] = sample(estados, size=1, prob=p0)
  for(i in 1:T) {
    x[i+1, j] = sample(estados, size=1, prob=P[x[i, j], ])
  }
  
}

plot(1:(T+1), x[,3], type="s")


matplot(1:(T+1), x, type="s", 
        col="gray", lty=1)



# 2c ----------------------------------------------------------------------

estados = 1:5
P = matrix(data=c(0.5, 0.5, 0.05, 0, 0,
                  0.5, 0.5, 0.10, 0, 0,
                  0, 0, 0.7, 0, 0, 
                  0, 0, 0.1, 0.3, 0.7,
                  0, 0, 0.05, 0.7, 0.3),
           nrow=5, ncol=5)
rownames(P) = estados
colnames(P) = estados

p0 = c(E1=0.2, E2=0.2, E3=0.2, E4=0.2, E5=0.2)

N = 128 # 128 réplicas
T = 20000 # 100 pasos de tiempo

x = matrix(ncol=N, nrow=T+1)

for(j in 1:N) {
  #   x = numeric(T+1)
  x[1, j] = sample(estados, size=1, prob=p0)
  for(i in 1:T) {
    x[i+1, j] = sample(estados, size=1, prob=P[x[i, j], ])
  }
  
}

plot(1:(T+1), x[,3], type="s")

# 
# matplot(1:(T+1), x, type="s", 
#         col="gray", lty=1)


xx = x[tail(1:T, 0.8*T), ]
table(xx)/sum(table(xx))


# 3 Radom walk  ---------------------------------------------------------

T = 100
posInicial = 0

saltos = c(-1, 1)

x = numeric(T+1)
x[1] = posInicial

for(i in 1:T) {
  salto = sample(saltos, 1, prob=)
  x[i+1] = x[i] + salto
}

plot(x, type="b")



T = 1000
N = 50

posInicial = 0
prob = c(0.5, 0.5)

saltos = c(-1, 1)

x = matrix(nrow=T+1, ncol=N)
x[1, ] = posInicial

for(j in 1:N) {
  for(i in 1:T) {
    salto = sample(saltos, 1, prob=prob)
    x[i+1, j] = x[i, j] + salto
  }
}

media = apply(x, 1, FUN=mean)
sd = apply(x, 1, FUN=sd)
q99 = apply(x, 1, FUN=quantile, prob=0.99)

ylim = range(c(x, media, media-2*sd, media+2*sd))
matplot(x[, 1:10], type="l", col="gray", lty=1,
        ylim=ylim)
lines(media, col="red", lwd=2)
lines(media+2*sd, col="red", lwd=1, lty=3)
lines(media-2*sd, col="red", lwd=1, lty=3)
lines(q99, col="blue", lwd=1, lty=3)


# Random walk 2D ------------------------------------------------------


T = 1000
N = 50

posInicial = c(0,0)
prob = c(0.5, 0.5)

saltos = c(-1, 1)

x = matrix(nrow=T+1, ncol=N)
y = matrix(nrow=T+1, ncol=N)
x[1, ] = posInicial[1]
y[1, ] = posInicial[2]

for(j in 1:N) {
  for(i in 1:T) {
    saltoX = sample(saltos, 1, prob=prob)
    saltoY = sample(saltos, 1, prob=prob)
    x[i+1, j] = x[i, j] + saltoX
    y[i+1, j] = y[i, j] + saltoY
  }
}


plot(x[,2], y[,2], type="l")

mediaX = apply(x, 1, FUN=mean)
mediaY = apply(y, 1, FUN=mean)
plot(mediaX, mediaY, type="l")



# Random walk 2D (new) -----------------------------------------------


T = 1000
N = 50

posInicial = c(0,5)
prob = c(0.5, 0.5)

saltos = c(-1, 1)

x = array(dim=c(T+1, N, 2))
x[1,,1] = posInicial[1]
x[1,,2] = posInicial[2]

for(j in 1:N) {
  for(i in 1:T) {
    saltoX = sample(saltos, 1, prob=prob)
    saltoY = sample(saltos, 1, prob=prob)
    x[i+1, j, 1] = x[i, j, 1] + saltoX
    x[i+1, j, 2] = x[i, j, 2] + saltoY
  }
}


plot(x[, 2, 1], x[, 2, 2], type="l")

media = apply(x, c(1,3), FUN=mean)
plot(media[,1], media[,2], type="l")




