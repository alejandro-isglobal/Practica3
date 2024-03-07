#Solucion pregunta 1

fx <- c(1-0.32,0.32)
names(fx) <- c("- de 2 teles", " 2 teles o +")
x <- c(0,1)

plot(x, fx, type="h", col="red", lwd=3, main="Probabilidad", xlab="X", ylab="f(x)",
     xlim=c(0,1), ylim=c(0,1)) 
points(x, fx, col="red", lwd=5)



Fx <- cumsum(fx) 
plot(c(-1,x,2), c(0,Fx,1), type="s", col="red", lwd=3, main="Función de distribución", xlab="X",
     ylab="F(x)")
points(x, c(0,Fx[1]), col="red", pch=21, bg="white")


### numero de casas con 2 teles

sample(x, 43, prob = fx, replace=T)

sum(sample(x, 43, prob = fx, replace=T))

Y <- function(i){sum(sample(x, 43, prob = fx, replace=T))
}


Y(1)

reps <- 1:500000
repY <- sapply(reps, Y)

fr <- table(repY)/length(reps)
barplot(fr)

fr[13]

dbinom(13, size = 43, prob = 0.32)

###

y <- as.numeric(names(fr))
fy <- dbinom(y, size = 43, prob = 0.32)
names(fy) <- y


yb <- barplot(fr, ylim=c(0,0.2))

lines(yb, fy, type="h", col="red", lwd=3) 
points(yb, fy, col="red", lwd=5)

Fy <- cumsum(fy) 
names(Fy) <- y
plot(c(-1,y,30), c(0,Fy,1), type="s", col="red", lwd=3, main="Función de distribución", xlab="X",
     ylab="F(x)")

Fy["6"]
##########

pbinom(6, size = 43, prob = 0.32)

qbinom(0.5, size = 43, prob = 0.32)

qbinom(0.25, size = 43, prob = 0.32)

###Repuesta

pbinom(16, size = 43, prob = 0.32)

###########

y <- 0:100
fy <- dbinom(y, size = 24, prob = 1-0.32) 
  
mu <- sum(fy*y)
(1-0.32)*24

sum(fy*(y-mu)^2)
(1-0.32)*0.32*24

qbinom(0.25,size = 24, prob = 1-0.32)

###

Y <- function(i){sum(sample(x, 46, prob = fx, replace=T))
}

Y(1)

reps <- 1:400000
repY <- sapply(reps, Y)

mean(repY)
0.32*46
