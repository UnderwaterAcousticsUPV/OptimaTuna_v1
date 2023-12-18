
# ANALISIS DE LA CORRELACIÓN ENTRE GRASA INFILTRADA Y TEMPERATURA DEL AGUA (MEJORADO)

library(readxl)
library(lubridate)

temp_path <- "C:/Lab/projects/balfego_optimatuna_v1/analisis_grasa/Temp_ametlla_de_mar.xlsx"

ext2021_path <- "C:/Lab/projects/balfego_optimatuna_v1/datos/Extracciones_c2021_r.xlsx"
ext2020_path <- "C:/Lab/projects/balfego_optimatuna_v1/datos/Extracciones_c2020_r.xlsx"
ext2018_path <- "C:/Lab/projects/balfego_optimatuna_v1/datos/Extracciones_c2018_r.xlsx"

temp <- read_excel(temp_path)
temp$Dia_ <- as.numeric(strftime(temp$Día, format = "%j"))
Dia <- temp$Día; temp$Día <- NULL

temp <- as.data.frame(apply(temp, 2, function(y) as.numeric(gsub("°C", "", y))))
temp$Dia <- Dia

names(temp) <- c("Year_2018", "Year_2019", "Year_2020", "Year_2021", "Year_2022", "Min", "Max", "Dia_ano", "Dia_ref")

plot(temp$Year_2020 ~ temp$Dia_ref, ylim=c(10,30), xlab=" ", ylab="Temperatura diaria [ºC]", col="white",
     main="EVOLUCIÓN ANUAL DE LA TEMPERATURA")
lines(temp$Year_2020 ~ temp$Dia_ref, col="blue", lwd=2)

#points(temp$Year_2021 ~ temp$Dia_ref, ylim=c(10,30), col="red")
lines(temp$Year_2021 ~ temp$Dia_ref, col="red", lwd=2)

#points(temp$Year_2018 ~ temp$Dia_ref, ylim=c(10,30), col="forestgreen")
lines(temp$Year_2018 ~ temp$Dia_ref, col="forestgreen", lwd=2)

legend("topright", legend=c("2018", "2020", "2021"), col=c("forestgreen", "blue", "red"), lty=1, lwd=2)

lines(temp$Min ~ temp$Dia_ref, col="gray", lty=2)
lines(temp$Max ~ temp$Dia_ref, col="gray", lty=2)

y2018 <- temp$Year_2018
y2019 <- temp$Year_2019
y2020 <- temp$Year_2020
y2021 <- temp$Year_2021
y2022 <- temp$Year_2022
y <- c(y2018, y2019, y2020, y2021, y2022)
d <- seq(from=as.Date("2018/01/01"), to=as.Date("2022/12/31"), "day")
d <- d[1:length(y)]
d <- as.POSIXct(d)

dftemp <- data.frame(
  Temp = y,
  Day = d
)

# Campana seleccionada
selected <- ext2021_path

# Lectura de archivo de extracctiones
ext <- read_excel(selected)

# Elimina valores anómalos de temperatura
# > 30ºC
# <= 0ºC
dftemp <- dftemp[dftemp$Temp < 30,]
dftemp <- dftemp[dftemp$Temp > 0,]

# Crea un campo en "ext" para guardar la temperatura de days_buffer
ext$Ta_buffer <- c(0)

# Calcula la temperatura en un buffer de 30 dias
days_buffer <- 7
temp_buffer <- list()
for(i in 1:nrow(ext)){
  df <- ext[i,]
  fecha_sacrif <- df$`Fecha Sacrificio`
  f_max <- fecha_sacrif
  f_min <- fecha_sacrif-lubridate::days(days_buffer)
  df_temp <- dftemp[dftemp$Day <= f_max,]
  df_temp <- df_temp[df_temp$Day >= f_min,]
  t_mean <- mean(df_temp$Temp, na.rm=TRUE)
  #print(t_mean)
  temp_buffer[[i]] <- t_mean
  ext$Ta_buffer[i] <- t_mean
}

#results <- data.frame(
#  Temp_mean <- unlist(temp_buffer),
#  Gras <- ext$Grasa
#)

# Aplica un reductor
# mean, median, min, max
reducer <- max

ext$Fecha_sacrificio <- ext$`Fecha Sacrificio`
res_gras <- aggregate(Grasa ~ Fecha_sacrificio, ext, reducer)

res_temp <- list()
for(i in 1:nrow(res_gras)){
  df <- res_gras[i,]
  fecha_sacrif <- df$Fecha_sacrificio
  f_max <- fecha_sacrif
  f_min <- fecha_sacrif-lubridate::days(days_buffer)
  df_temp <- dftemp[dftemp$Day <= f_max,]
  df_temp <- df_temp[df_temp$Day >= f_min,]
  t_mean <- mean(df_temp$Temp, na.rm=TRUE)
  res_temp[[i]] <- t_mean
}

results <- data.frame(
  Fecha_sacrificio = res_gras$Fecha_sacrificio,
  Grasa_promedio = res_gras$Grasa,
  Temperatura = unlist(res_temp)
)

results <- results[complete.cases(results), ]

cor <- cor(results$Grasa_promedio, results$Temperatura)

plot(results$Temperatura, results$Grasa_promedio, ylim=c(0, 30),
     col="blue", pch=19, main=paste0("Max (7 días)   ρ = ", round(cor, digits=2)),
     xlab="Temperatura [ºC]", ylab="Grasa [%]")
l <- lm(results$Grasa_promedio ~ results$Temperatura)
abline(l, col="red", lwd=2)

#hist(ext$Grasa, breaks = 30, prob=TRUE)
#lines(density(ext$Grasa), col="red", lwd=2)

# Correlación entre Grasa promedio diaria y Temperatura en los ultimos days_buffer dias
cor(results$Grasa_promedio, results$Temperatura)
