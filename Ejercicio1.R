# SOlucion Ejercicio

x1 = 0:5   # Posibles resultados
f1 = c(1,2,2,3,4,3)/15 # Probabilidad de ocurrencia de cada resultado
# Creación de las líneas verticales
plot(x1, f1, type="h", col="red", lwd=3, main="Función de probabilidad", xlab="X", ylab="f(x)",
     xlim=c(-0.5,5.5), ylim=c(0,0.3)) 
# Se crean los puntos y se guarda la gráfica completa en un objeto para su uso posterior
points(x1, f1, col="red", lwd=10)




F1 = cumsum(f1) # Se genera un vector con la suma acumulada
plot(c(-1,x1,6), c(0,F1,1), type="s", col="red", lwd=3, main="Función de distribución", xlab="X",
     ylab="F(x)")
points(x1, F1, col="red", lwd=8)




#f(2)=2/15
#F(2)=1/3
#f(3.5)=0
#F(3.5)=8/15
#f(6)=0
#F(6)=1


miu.X <- sum(x1*f1); miu.X


Q2.X <- max(x1[F1<=0.5]); Q2.X


var.X <- sum((x1-miu.X)^2*f1); var.X


set.seed(12)
sim.venta <- sample(x1,30,replace=T,prob=f1)


fi <- table(sim.venta)/length(sim.venta) 
xb <- barplot(fi)
lines(xb, f1, type="h", col="red", lwd=3)  
points(xb, f1, col="red", lwd=10)

mean.sim <- mean(sim.venta); mean.sim

var.sim <- var(sim.venta); var.sim


set.seed(12)
sim.venta <- sample(x1,10000,replace=T,prob=f1)
fi <- table(sim.venta)/length(sim.venta) 
xb <- barplot(fi)
lines(xb, f1, type="h", col="red", lwd=3)  
points(xb, f1, col="red", lwd=10)


mean.sim <- mean(sim.venta); mean.sim

var.sim <- var(sim.venta); var.sim


miu.Y <- 0.75*miu.X-1.5; miu.Y
var.Y <- 0.75^2*var.X; var.Y

set.seed(12)
y1 <- 0.75*x1-1.5
sim.ben <- sample(y1,10000,replace=T,prob=f1)
fi <- table(sim.ben)/length(sim.ben) 
xb <- barplot(fi)
lines(xb, f1, type="h", col="red", lwd=3)  
points(xb, f1, col="red", lwd=10)


mean.sim.ben <- mean(sim.ben); mean.sim.ben

var.sim.ben <- var(sim.ben); var.sim.ben




