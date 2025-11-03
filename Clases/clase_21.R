data <- read.csv("/home/Estudiante/Descargas/glakes.csv")

LT <- log(data$Time)
W <- (data$Tonnage)^0.25

modelo <- lm(LT~W, data = data)
modelo$residuals
summary(modelo)

plot(W, LT,
     xlab = "Peso^0.25",
     ylab = "log(tiempo)")

n <- length(LT)
Wmat <- cbind(1,W) # le agrego la primera columna de 1's
LTmat <- matrix(LT,n,1)

XtX_inv <- solve(t(Wmat)%*%Wmat) # la matriz (X^tX)^(-1)

beta_hat <- XtX_inv %*% (t(Wmat) %*% LTmat) # B_0, B_1

Yhat <- Wmat %*% beta_hat

abline(a = beta_hat[1], b = beta_hat[2], col = "red", lwd = 2)

points(W, Yhat, col = "orange")

residuos <- LTmat - Yhat

print(beta_hat)
#coinciden con los de lm modelo
# p = 2 porque tenemos 2 parametros
s2 <- norm(LTmat - Yhat, type="2")^2 / (n - 2)
s <- sqrt(s2)


# 3
D <- t(Wmat)%*%Wmat
d11 <- D[2,2]
print(d11)
# quiero ver si tita 1 es distinto de cero:

t_alpha2 <- qt(0.005, df = (n-2))
estadistico <- beta_hat[2]/(s*sqrt(d11))
rechazo <- abs(estadistico) > t_alpha2
# Rechazo, es decir, hay evidencia para decir que es distinto a nivel 0.01

p_2 <- 1- pt(estadistico, df=(n-2))
pvalor <- p_2 * 2
