# Guía de ejercicios 1

# 1
debernardi <- read.csv("/home/Estudiante/Escritorio/martu/Conjuntos de datos-20250828/Debernardi.csv", sep = ",", header = TRUE)

attach(debernardi)

diagnosis_freq_abs <- table(diagnosis)
diagnosis_freq_rel <- prop.table(table(diagnosis))

barplot(diagnosis_freq_rel,
        main = "Frecuencia de Diagnosis",
        xlab = "DIagnosis",
        ylab = "Frecuencia",
        col = c("green", "yellow", "red"),
        ylim = c(0, max(diagnosis_freq_rel) * 1.1)
)

# 3
iridio <- read.csv("/home/Estudiante/Escritorio/martu/Conjuntos de datos-20250828/iridio.txt", sep = " ", header = TRUE)
rodio <- read.csv("/home/Estudiante/Escritorio/martu/Conjuntos de datos-20250828/rodio.txt", sep = " ", header = TRUE)

hist(iridio$iridio)
hist(rodio$rodio)

boxplot(iridio$iridio, main = "Iridio", col = "lightblue")
boxplot(rodio$rodio, main = "Rodio", col = "lightgreen")

boxplot(iridio$iridio, rodio$rodio, names = c("Iridio", "Rodio"),
        main = "Comparación de Boxplots", col = c("lightblue", "lightgreen"))

media_i <- mean(iridio$iridio)
mediana_i <- median(iridio$iridio)
media_podada10_i <- mean(iridio$iridio, trim = 0.1, na.rm = TRUE)
media_podada20_i <- mean(iridio$iridio, trim = 0.2, na.rm = TRUE)

media_r <- mean(rodio$rodio)
mediana_r <- median(rodio$rodio)
media_podada10_r <- mean(rodio$rodio, trim = 0.1, na.rm = TRUE)
media_podada20_r <- mean(rodio$rodio, trim = 0.2, na.rm = TRUE)


desviacion_estandar_i <- sqrt(var(iridio$iridio))
desviacion_estandar_r <- sqrt(var(rodio$rodio))


cuantiles_i <- quantile(iridio$iridio, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
cuantiles_r <- quantile(rodio$rodio, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)

IQR_i <- cuantiles_i[3] - cuantiles_i[1]
IQR_r <- cuantiles_r[3] - cuantiles_r[1]

MAD_i <- median(abs(iridio$iridio - mediana_i))
MAD_r <- median(abs(rodio$rodio - mediana_r))
