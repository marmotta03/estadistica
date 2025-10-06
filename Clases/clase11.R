muestra <- c(500, 488, 426, 510, 450, 368, 508, 514, 426, 476, 512, 526, 444, 524, 236)
n <- 15
lambda1_obs <- mean(muestra)
B <- 1000
est_boost <- rep(NA, B)
for (i in 1:B) {
  x_boost <- rpois(n, lambda1_obs)
  lambda_boost <- mean(x_boost)
  est_boost[i] <- lambda_boost
}

hist(est_boost)
# Vemos que se parece a una normal
plot(density(est_boost))

# Veo el desvio estandar muestral y lo elevo al cuadrado
desvio_boost_lambda1 <- sd(est_boost)
varianza_boost_lambda1 <- desvio_boost_lambda1 **2
# Veamos la media muestral
media_boost_lambda1 <- mean(est_boost)

# Para lambda 2
lambda2_obs <- (-1 + sqrt(1 + (4 * mean(muestra ** 2))))/2
est_boost_2 <- rep(NA, B)
for (i in 1:B) {
  x_boost <- rpois(n, lambda2_obs)
  lambda_boost <- (-1 + sqrt(1 + (4 * mean(x_boost ** 2))))/2
  est_boost_2[i] <- lambda_boost
}

hist(est_boost_2)
# Vemos que se parece a una normal
plot(density(est_boost_2))

# Veo el desvio estandar muestral y lo elevo al cuadrado
desvio_boost_lambda2 <- sd(est_boost_2)
varianza_boost_lambda2 <- desvio_boost_lambda2 **2
# Veamos la media muestral
media_boost_lambda2 <- mean(est_boost_2)

# -----------------------------------
# NO PARAMETRICO
# lambda1
est_boost_np_1 <- rep(NA, B)
for(i in 1:B){
  xboot <- sample(muestra, n, replace = TRUE)
  est_boost_np_1[i] <- mean(xboot)
}

hist(est_boost_np_1)
# Vemos que se parece a una normal
plot(density(est_boost_np_1))

# Veo el desvio estandar muestral y lo elevo al cuadrado
desvio_boost_np_lambda1 <- sd(est_boost_np_1)
varianza_boost_np_lambda1 <- desvio_boost_np_lambda1 **2
# LA VARIANZA DIO MUCHO MAS ALTA!!!

# Veamos la media muestral
media_boost_np_lambda1 <- mean(est_boost_np_1)

# lambda2
est_boost_np_2 <- rep(NA, B)
for(i in 1:B){
  xboot <- sample(muestra, n, replace = TRUE)
  est_boost_np_2[i] <- (-1 + sqrt(1 + (4 * mean(xboot ** 2))))/2
}

hist(est_boost_np_2)
# Vemos que se parece a una normal
plot(density(est_boost_np_2))

# Veo el desvio estandar muestral y lo elevo al cuadrado
desvio_boost_np_lambda2 <- sd(est_boost_np_2)
varianza_boost_np_lambda2 <- desvio_boost_np_lambda2 **2
# LA VARIANZA DIO MUCHO MAS ALTA!!!

# Veamos la media muestral
media_boost_np_lambda2 <- mean(est_boost_np_2)

# -------------
# -------------
# EJERCICIO 2
tita_dado_equilibrado <- 1/2
muestra <- scan(what= numeric(), text="2 2 4 6 1 3 1 3 2 4 4 4 4 4 6 3 3 4 1 2 1 6 3 2 3 4 1 1 5 4 1 4 6 4 1 2 1 5 4 3 3 1 3 1 6 5 1 3 2 3 6 2 4 2 6 6 5 2 4 4 1 4 3 1 2 1 6 1 1 3 1 6 6 1 2 6 1 1 4 5 4 1 5 2 2 1 6 6 1 2 1 3 1 3 3 4 3 3 3 5")
n <- 100
B <- 5000
contar_pares <- function(vectorcito){
  tamaño <- length(vectorcito)
  cantidad_pares <- 0
  for(i in 1:tamaño){
    if(vectorcito[i]%%2 == 0){
      cantidad_pares <- cantidad_pares + 1
    }
  }
  return(cantidad_pares)
}
tita_hat <- contar_pares(muestra)/n

titas_boot <- rep(NA, B)

for (i in 1:B){
  xboost <- sample(muestra, 100, replace=TRUE)
  tita_boost <- contar_pares(xboost) / 100
  titas_boot[i] <- tita_boost
}

hist(titas_boot)
# Vemos que se parece a una normal
plot(density(titas_boot))

# Veo el desvio estandar muestral y lo elevo al cuadrado
desvio_boost_titas <- sd(titas_boot)
varianza_boost_titas <- desvio_boost_titas **2
# LA VARIANZA DIO MUCHO MAS ALTA!!!

# Veamos la media muestral
media_boost_titas <- mean(titas_boot)

# Hacemos los dos intervalos:
# Asumiendo normalidad
zalfa2 <- qnorm(0.975)
IC_n <- c(tita_hat - zalfa2 * desvio_boost_titas, tita_hat + zalfa2 * desvio_boost_titas)
# No asumiendo nada, intervalo boostrap percentil
inf <- quantile(titas_boot, 0.025)
sup <- quantile(titas_boot, 0.975)
IC_b <- c(inf,sup)

# Es equilibrado