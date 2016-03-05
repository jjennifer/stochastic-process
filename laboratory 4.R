#-----------------------------------------------
# LABORATORY SESSION 4

# Author  : Joan Martinez
#Goals    : 1 Poisson processes
#           2 We program the states and stochastic process of a flip coin.
#           3 Next, a random walk proces
#-----------------------------------------------



#0. Setting initial conditions
T=10
i=1

t0=0
X0=0

t = numeric()
X = numeric()


#1. Simulation of the trajectory for a particular counting process
t[1]=t0
X[1]=X0

    #Start
while(t[i] <= T) {
  dt = runif(1, min=0, max=1)
  dX = 1
  t[i+1]= t[i]+dt
  X[i+1]= X[i]+dX
  
  i=i+1
}
    #end of a trajctory

X= head(X,-1)
t= head(t,-1)

plot(t,X,type="s")



#2. Compute average distribution of the process
T=10
N=20
t0=0  #initial conditions
X0=0

      #We define observation process
t.obs = seq(from=0, to= T, by=0.01)
X.obs=matrix(nrow=lenght(t.obs))

      #loop
for(j in 1:N){
  i=1
  t=numeric()
  X=numeric()
    
  t[1]=t0
  X[1]=X0

      #start
  while(t[i] <= T) {
    dt = runif(1, min=0, max=1)
    dX = 1
    t[i+1]= t[i]+dt
    X[i+1]= X[i]+dX
    
    i=i+1
  }
      #end of a trajectory

#   X= head(X,-1)  #Cambio No. 1. Se removia ultimo tiempo y estado por pasarse horizonte de simulacion
#   t= head(t,-1)
#  X= tail(X,1)   #Captuara el ultimo elemento Cambio No. 3 
   X[length(X)]= X[length(X)-1]  #Cambio No.3. Ultimo estado se reemlplaza por anterior
   t[length(t)]= T #Para especificar que no hubo cambio en el ultimo estado de tiempo
   
  XX[[j]] = X
  tt[[j]] = t

      #defining cut points (<3)
pos = as.numeric(cut(t.obs,breaks=tt[[j]], right=FALSE, include.lowest=TRUE))
X.obs[,j] = X[pos]  #Para cada realizacion, veo los numeros de observacion
}

      #number of events
lapply(X=XX, FUN=length) #calculos sobre margenes de una matrix, lapply para listas
sapply(X=XX, FUN=length)

      #average of observed X; graph
media = rowMeans(X.obs)
q95=apply(X.obs,1,uantile,pro=0.95)
q05=apply(X.obs,1,uantile,pro=0.05)

plot(tt[[1]], XX[[1]], type="s",ylim=c(0,10), xlim=c(0,T))
abline(v=T,lty=3)

for(i in 2:N)
  lines(tt[[i]],XX[[i]], type="s", col=i)
lines(t.obs,media,lwd=2,cold="red")
lines(t.obs,q95,lwd=2,cold="blue")
lines(t.obs,q05,lwd=2,cold="blue")

#Legend. 
#Red. If the average number of events that we observe for each 
#event. Green. 'Happenings' of a particular counting process.


        #another way to observe the process
        xmax = max(sapply(X=XX,FUN=length))
        obs = function(t,t.obs) {
          pos =as.numeric(cut(t.obs,breaks=t, right=FALSE, include.lowest=TRUE))
          return(os)
        }
        lapply(tt, FUN=obs, t.obs=t.obs) #Me da todas las posiciones. Las uso para extraer los estados correspondietes.
        

#Note.  Generating categorical variables based on continue
X = sort(runif(10))  #S
Y = seq(from=0, to=1, by=0.01)  #Observaciones del proceso (literal, evaluarlo)
cut(Y, breaks=X)  #J_n
cut(Y,breaks=c(-Inf,X,Inf)) #Intervalos de por defecto, cerrados por derecha y abierto en izq.
cut(Y,breaks=c(-Inf,X,Inf),right=FALSE) #Intervalos de por defecto, cerrados por derecha y abierto en izq.
as.numeric(cut(Y,breaks=c(-Inf,X,Inf),right=FALSE)) #Intervalos de por defecto, cerrados por derecha y abierto en izq.

#Note.  I have the number of jumps in which obs. go into the second state
Y #S  
X 

#Obs.   I see the process to verify the 'observation time'
