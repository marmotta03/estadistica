install.packages("Lock5Data")
library(Lock5Data)
data("FloridaLakes")
help("FloridaLakes")   # da informacion sobre los datos

aa <- hist(FloridaLakes$Alkalinity, freq = FALSE)
max(aa$density)
mean(FloridaLakes$Alkalinity)
median(FloridaLakes$Alkalinity)

alcalinidad_ordenados <- sort(FloridaLakes$Alkalinity)
alcalinidad_ordenados_poda <- mean(FloridaLakes$Alkalinity, trim = 0.2, na.rm = TRUE)

#Proba alcalinidad <= 40
proba <- (20 - 0) * aa$density[1] + (40 - 20) * aa$density[2]

# 12
library(ggplot2)
p_boxplot <- ggplot(FloridaLakes, aes(y = Alkalinity)) +
  geom_boxplot(fill = "lightgreen", color = "darkgreen", outlier.colour = "red", outlier.size = 2) +
  labs(title = paste("Boxplot de Alcalinidad"),
       y = "Alcalinidad") +
  theme_minimal()
print(p_boxplot)

#14
densidad_alcalinidad <- density(FloridaLakes$Alkalinity)
plot(densidad_alcalinidad)
