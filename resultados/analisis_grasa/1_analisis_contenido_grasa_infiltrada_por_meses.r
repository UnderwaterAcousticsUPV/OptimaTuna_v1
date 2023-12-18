
# ANALISIS DEL CONTENIDO DE GRASA INFILTRADA POR MESES

library(readxl)
library(lubridate)
library(vioplot)

ext2021_path <- "C:/Lab/projects/balfego_optimatuna_v1/datos/Extracciones_c2021_r.xlsx"
ext2020_path <- "C:/Lab/projects/balfego_optimatuna_v1/datos/Extracciones_c2020_r.xlsx"
ext2018_path <- "C:/Lab/projects/balfego_optimatuna_v1/datos/Extracciones_c2018_r.xlsx"

# Campana seleccionada
selected <- ext2018_path

# Ultima fecha de capturas
#f_1 <- as.POSIXct("2021-06-28")
#f_1 <- as.POSIXct("2020-06-16")
f_1 <- as.POSIXct("2018-06-18")

ext <- read_excel(selected)
ext$Month <- month(ext$`Fecha Sacrificio`)

# Elimina valores anómalos de grasa (>3 0%)
ext <- ext[ext$Grasa < 30,]

# Elimina valores de grasa == 0
ext <- ext[ext$Grasa > 0,]

# Calcula numero de sacrificios entre el 8-12%
print(paste0("Número de sacrificios totales: ", nrow(ext)))
meses <- c(1:12)
for(i in 1:length(meses)){
  df_mes <- ext[ext$Month == i,]
  df_mes <- df_mes[!is.na(df_mes$Grasa),]
  total_mes <- nrow(df_mes)
  df_ok <- df_mes[df_mes$Grasa >= 8,]
  df_ok <- df_ok[df_ok$Grasa <= 12,]
  print(paste0("Mes ", i, ": ", round(nrow(df_ok)/total_mes*100, digits=2)))
}

# Plot porcentajes de cumplimiento 8-12%
par(mfrow=c(2,6))
meses = c(1,2,3,4,5,6,7,8,9,10,11,12)
for(i in meses){
  df_mes <- ext[ext$Month == i,]
  df_mes <- df_mes[!is.na(df_mes$Grasa),]
  plot(df_mes$Grasa, xlab="Índice de la muestra", ylab="Grasa [%]",
       main=paste0("Mes ", i), col="dodgerblue")
  abline(h=8, col="red", lwd=2)
  abline(h=12, col="red", lwd=2)
}

# Separa los machos (M) y las hembras (H)
extM <- ext[ext$Sexo == "M",]; nrow(extM)
extH <- ext[ext$Sexo == "H",]; nrow(extH)

# Elimina sacrificios con menos de 2 meses de crianza
th_date <- f_1+lubridate::days(60)
ext <- ext[ext$`Fecha Sacrificio` > th_date, ]
extM <- ext[ext$Sexo == "M",]; nrow(extM)
extH <- ext[ext$Sexo == "H",]; nrow(extH)

# Genera el plot
par(mfrow=c(1,1))
vioplot(ext$Grasa ~ ext$Month, ylim=c(0,25), col="gray88", border="transparent",
        xlab="Mes", ylab="Grasa [%]",
        main="DISTRIBUCIÓN MENSUAL DE LA GRASA INFILTRADA (CAMPAÑA 2021)")
means <- tapply(ext$Grasa, ext$Month, mean, na.rm=TRUE)
means_M <- tapply(extM$Grasa, extM$Month, mean, na.rm=TRUE)
means_H <- tapply(extH$Grasa, extH$Month, mean, na.rm=TRUE)
points(means, pch=20, cex=1.5, col="black")
lines(means, lwd=1.5, col="black")
lines(means_M, lwd=2.5, col="dodgerblue1")
lines(means_H, lwd=2.5, col="firebrick2")
nrow(ext)
abline(v=6.5, lwd=2, col="black", lty=2) # inicio de campana de pesca
abline(v=6.0, lwd=2, col="black", lty=2) # final de campana de pesca
legend(6.6, 23, "➜ Inicio de campaña 2021", adj = 0.1)
legend(x = "topright",          # Position
       legend = c("   M ", "   H ", "Todos"),  # Legend texts
       lty = c(1),           # Line types
       col = c("dodgerblue1", "firebrick2", "black"),
       lwd = 3,
       title="Sexo") 
abline(h=8, col="green4", lwd=3)
abline(h=12, col="green4", lwd=3)
text(2, 7, "Rango óptimo de grasa [8-12%]", col="green4", bg="white")

# Cálculo del porcentaje de cumplimiento 8-12% global del año
ext_ok <- ext[ext$Grasa >= 8,]
ext_ok <- ext_ok[ext_ok$Grasa <= 12,]
print(paste0("Porcentaje de cumplimiento global de la temporada: ", round(nrow(ext_ok)/nrow(ext)*100, digits=2)))

