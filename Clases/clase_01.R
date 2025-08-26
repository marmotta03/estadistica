# Clase 01

#1_a
t <- 0
for(i in 1:1000){
  t <- t + i
}

#1_b
N <- 0
i <- 0
while(N<=10000){
  N <- N + i
  i <- i + 1
}

print(paste("El menor valor es ", i-1))

#1_c

vector_1_c <- c(1,2,5,-1,675,-34,-9,-21,-23)

suma_coord <-function(vector){
  suma <- 0
  for(element in vector){
    if(element>0){
      suma <- suma + element
    }
  }
  return(suma)
}

print(suma_coord(vector_1_c))


#2
pasos <- c(0.2,0.1,0.01,0.5)

f <- function(p){
  y <- p * (1-p)
  return(y)
}

for(paso in pasos){
  p <- seq(0,1,by=paso)
  y <- f(p)
  plot(p,y,type="l")
}


#4
xs <- seq(0,2*pi,length.out=100)
y1 <- sin(xs)
y2 <- cos(xs)
y3 <- cos(xs^2)

# Plot the first graph
plot(xs, y1, type = "l", col = "blue", 
     xlab = "Eje x", ylab = "Eje y", main = "Funciones trigonometricas")

# Add the second graph using lines()
lines(xs, y2, col = "red", type = "l")

# Add the second graph using lines()
lines(xs, y3, col = "green", type = "l")

# Add a legend to distinguish the lines
legend("topleft", legend = c("y = sen(x)", "y = cos(x)","y = cos(x^2)"), 
       col = c("blue", "red","green"),lty = c(1, 1, 1))

autos <- read.csv(
  "/home/Estudiante/Escritorio/autos.txt", sep = " ", header = TRUE
)

print(autos[3,])
print(autos[,2])
autos[autos$precio == min(autos$precio),"calidad"]

sum(autos[1:4,"precio"])

apply(autos,MARGIN=1,FUN=sum) #row
apply(autos,MARGIN=2,FUN=sum) #column

plot(autos[,"precio"],autos[,"calidad"],type = "p")
plot(autos)

autos <- autos[order(autos$precio),]

#5
rownames(mtcars[mtcars$gear==4,])
nrow(mtcars[mtcars$gear==4,])
