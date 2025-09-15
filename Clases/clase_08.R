set.seed(42)

datos_normales <- rnorm(5,4,9)

intervalo <- function(datos,sigma,nivel){
  alfa <- 1 - nivel
  alfa_2 <- alfa/2
  zalfa_2 <- qnorm((1-alfa_2),0,1)
  promedio <- mean(datos)
  n <- length(datos)
  a <- promedio - (sigma/sqrt(n) * zalfa_2)
  b <- promedio + (sigma/sqrt(n) * zalfa_2)
  ic <- c(a,b)
  return(ic)
}

ic <- intervalo(datos_normales,3,0.95)


##########
Nrep <- 1000
n <- 5
mu <- 4
sigma2 <- 9

# Guardamos los intervalos y si cubren
intervalos <- data.frame(
  simulacion = 1:Nrep,
  inf = numeric(Nrep),
  sup = numeric(Nrep),
  cubre = logical(Nrep)
)
for (i in 1:Nrep) {
  muestra <- rnorm(n, mean = mu, sd = sqrt(sigma2))
  ic <- intervalo(muestra, sqrt(sigma2), nivel = 0.95)
  intervalos$inf[i] <- ic[1]
  intervalos$sup[i] <- ic[2]
  intervalos$cubre[i] <- mu >= ic[1] && mu <= ic[2]
}


# Tomamos una muestra para visualizar (e.g., 100 simulaciones)
intervalos_vis <- intervalos[1:100, ]
intervalos_vis$simulacion <- factor(intervalos_vis$simulacion, levels =
                                      rev(intervalos_vis$simulacion))
ggplot(intervalos_vis, aes(y = simulacion, x = inf, xend = sup, color = cubre)) +
  geom_segment(aes(x = inf, xend = sup, yend = simulacion), linewidth =1) +
  geom_vline(xintercept = mu, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("red", "blue"), labels = c("No cubre", "Cubre")) +
  labs(
    title = "IC para mu",
    x = "Valor",
    y = "Simulacion",
    color = "Cubre mu?"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_blank())


intervalo_s2 <- function(datos,nivel){
  promedio <- mean(datos)
  n <- length(datos)
  s2 <- 1/(n-1) * sum((datos - promedio)**2)
  s <- sqrt(s2)
  alfa <- 1 - nivel
  alfa_2 <- alfa/2
  zalfa_2 <- qnorm((1-alfa_2),0,1)
  a <- promedio - (s/sqrt(n) * zalfa_2)
  b <- promedio + (s/sqrt(n) * zalfa_2)
  ic <- c(a,b)
  return(ic)
}

# Guardamos los intervalos y si cubren
intervalos_s2 <- data.frame(
  simulacion = 1:Nrep,
  inf = numeric(Nrep),
  sup = numeric(Nrep),
  cubre = logical(Nrep)
)
for (i in 1:Nrep) {
  muestra <- rnorm(n, mean = mu, sd = sqrt(sigma2))
  ic <- intervalo_s2(muestra, nivel = 0.95)
  intervalos_s2$inf[i] <- ic[1]
  intervalos_s2$sup[i] <- ic[2]
  intervalos_s2$cubre[i] <- mu >= ic[1] && mu <= ic[2]
}


# Tomamos una muestra para visualizar (e.g., 100 simulaciones)
intervalos_vis_2 <- intervalos_s2[1:100, ]
intervalos_vis_2$simulacion <- factor(intervalos_vis_2$simulacion, levels =
                                      rev(intervalos_vis_2$simulacion))
ggplot(intervalos_vis_2, aes(y = simulacion, x = inf, xend = sup, color = cubre)) +
  geom_segment(aes(x = inf, xend = sup, yend = simulacion), linewidth =1) +
  geom_vline(xintercept = mu, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("red", "blue"), labels = c("No cubre", "Cubre")) +
  labs(
    title = "IC para mu",
    x = "Valor",
    y = "Simulacion",
    color = "Cubre mu?"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_blank())