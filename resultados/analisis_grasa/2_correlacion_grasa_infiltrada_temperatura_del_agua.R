
# ANALISIS DE LA CORRELACIÓN ENTRE GRASA INFILTRADA Y TEMPERATURA DEL AGUA

library(readxl)
library(lubridate)

ext2021_path <- "C:/Lab/projects/balfego_optimatuna_v1/datos/Extracciones_c2021_r.xlsx"
ext2020_path <- "C:/Lab/projects/balfego_optimatuna_v1/datos/Extracciones_c2020_r.xlsx"
ext2018_path <- "C:/Lab/projects/balfego_optimatuna_v1/datos/Extracciones_c2018_r.xlsx"

temp_path <- "C:/Lab/projects/balfego_optimatuna_v1/datos/Datos_ambientales_F1631016791475_ZVMEDIO.xlsx"

T_total <- list()
G_total <- list()

#### 2021 ####


# Campana seleccionada
selected <- ext2021_path

# Lectura de archivo de extracctiones
ext <- read_excel(selected)

# Lectura de archivo de datos ambientales
temp <- read_excel(temp_path)

# Elimina valores anómalos de temperatura
# > 30ºC
# <= 0ºC
temp <- temp[temp$`Tª mar medio`< 35,]
temp <- temp[temp$`Tª mar medio`> 0,]

# Subset del archivo de temperatura
temp <- temp[temp$`Fecha Imputación` <= max(ext$`Fecha Sacrificio`),]
temp <- temp[temp$`Fecha Imputación` >= min(ext$`Fecha Sacrificio`),]

# Indice de meses
temp$Month <- month(temp$`Fecha Imputación`)
ext$Month <- month(ext$`Fecha Sacrificio`)

# Calcula promedios mensuales de grasa y temperatura
means_G <- tapply(ext$Grasa, ext$Month, mean, na.rm=TRUE)
means_T <- tapply(temp$`Tª mar medio`, temp$Month, mean, na.rm=TRUE)

#---ajuste manual para completar los valores del mes 7
means_G1 <- as.vector(means_G)[1:6]
means_G2 <- as.vector(means_G)[7:11]
means_G <- c(means_G1, (means_G1[6]+means_G2[1])/2, means_G2)

# Plot
plot(means_G, means_T, ylim=c(5,30), xlim=c(5,15), col="deepskyblue", pch=19, cex=2,
     xlab="Grasa promedio [%]", ylab="Temperatura promedio [ºC]",
     main="CORRELACIÓN ENTRE PROMEDIOS MENSUALES DE GRASA Y TEMPERATURA")

T_total <- append(T_total, means_T)
G_total <- append(G_total, means_G)

#### 2020 ####

# Campana seleccionada
selected <- ext2020_path

# Lectura de archivo de extracctiones
ext <- read_excel(selected)

# Lectura de archivo de datos ambientales
temp <- read_excel(temp_path)

# Elimina valores anómalos de temperatura
# > 30ºC
# <= 0ºC
temp <- temp[temp$`Tª mar medio`< 35,]
temp <- temp[temp$`Tª mar medio`> 0,]

# Subset del archivo de temperatura
temp <- temp[temp$`Fecha Imputación` <= max(ext$`Fecha Sacrificio`),]
temp <- temp[temp$`Fecha Imputación` >= min(ext$`Fecha Sacrificio`),]

# Indice de meses
temp$Month <- month(temp$`Fecha Imputación`)
ext$Month <- month(ext$`Fecha Sacrificio`)

# Calcula promedios mensuales de grasa y temperatura
means_G <- tapply(ext$Grasa, ext$Month, mean, na.rm=TRUE)
means_T <- tapply(temp$`Tª mar medio`, temp$Month, mean, na.rm=TRUE)

#---ajuste manual para completar los valores del mes 7
#means_G1 <- as.vector(means_G)[1:6]
#means_G2 <- as.vector(means_G)[7:11]
#means_G <- c(means_G1, (means_G1[6]+means_G2[1])/2, means_G2)

# Plot
points(means_G, means_T, ylim=c(5,30), xlim=c(5,15), col="dodgerblue2", pch=19, cex=2)

T_total <- append(T_total, means_T)
G_total <- append(G_total, means_G)

#### 2018 ####

# Campana seleccionada
selected <- ext2018_path

# Lectura de archivo de extracctiones
ext <- read_excel(selected)

# Lectura de archivo de datos ambientales
temp <- read_excel(temp_path)

# Elimina valores anómalos de temperatura
# > 30ºC
# <= 0ºC
temp <- temp[temp$`Tª mar medio`< 35,]
temp <- temp[temp$`Tª mar medio`> 0,]

# Subset del archivo de temperatura
temp <- temp[temp$`Fecha Imputación` <= max(ext$`Fecha Sacrificio`),]
temp <- temp[temp$`Fecha Imputación` >= min(ext$`Fecha Sacrificio`),]

# Indice de meses
temp$Month <- month(temp$`Fecha Imputación`)
ext$Month <- month(ext$`Fecha Sacrificio`)

# Calcula promedios mensuales de grasa y temperatura
means_G <- tapply(ext$Grasa, ext$Month, mean, na.rm=TRUE)
means_T <- tapply(temp$`Tª mar medio`, temp$Month, mean, na.rm=TRUE)

#---ajuste manual para completar los valores del mes 7
#means_G1 <- as.vector(means_G)[1:6]
#means_G2 <- as.vector(means_G)[7:11]
#means_G <- c(means_G1, (means_G1[6]+means_G2[1])/2, means_G2)

# Plot
points(means_G, means_T, ylim=c(5,30), xlim=c(5,15), col="dodgerblue4", pch=19, cex=2)

T_total <- append(T_total, means_T)
G_total <- append(G_total, means_G)

T_total <- unlist(T_total)
G_total <- unlist(G_total)

cor(T_total, G_total)

text(14, 28, "ρ = 0.58")

l <- lm(T_total ~ G_total)
abline(l)