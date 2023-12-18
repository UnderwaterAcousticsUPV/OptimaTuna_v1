
# ANÁLISIS INTERANUAL GRASA-TEMPERATURA

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

#### Mes más cálido

ext2018 <- read_excel(ext2018_path); ext2018$Month <- month(ext2018$`Fecha Sacrificio`)
ext2020 <- read_excel(ext2020_path); ext2020$Month <- month(ext2020$`Fecha Sacrificio`)
ext2021 <- read_excel(ext2021_path); ext2021$Month <- month(ext2021$`Fecha Sacrificio`)

ext2018 <- ext2018[ext2018$Month == 8,]; ext2018$camp <- 2018
ext2020 <- ext2020[ext2020$Month == 8,]; ext2020$camp <- 2020
ext2021 <- ext2021[ext2021$Month == 8,]; ext2021$camp <- 2021

names(ext2018) <- names(ext2020); names(ext2021) <- names(ext2018)
df_calido <- rbind(ext2018, ext2020, ext2021)
df_calido <- df_calido[df_calido$Grasa < 30,]
df_calido <- df_calido[df_calido$Grasa > 0,]

boxplot(df_calido$Grasa ~ df_calido$camp, xlab="", ylab="Grasa [%]", ylim=c(0,15),
        main="Agosto")

#### Mes más frio

ext2018 <- read_excel(ext2018_path); ext2018$Month <- month(ext2018$`Fecha Sacrificio`)
ext2020 <- read_excel(ext2020_path); ext2020$Month <- month(ext2020$`Fecha Sacrificio`)
ext2021 <- read_excel(ext2021_path); ext2021$Month <- month(ext2021$`Fecha Sacrificio`)

ext2018 <- ext2018[ext2018$Month == 1,]; ext2018$camp <- 2018
ext2020 <- ext2020[ext2020$Month == 1,]; ext2020$camp <- 2020
ext2021 <- ext2021[ext2021$Month == 1,]; ext2021$camp <- 2021

names(ext2018) <- names(ext2020); names(ext2021) <- names(ext2018)
df_frio <- rbind(ext2018, ext2020, ext2021)
df_frio <- df_frio[df_frio$Grasa < 30,]
df_frio <- df_frio[df_frio$Grasa > 0,]

boxplot(df_frio$Grasa ~ df_frio$camp, xlab="", ylab="Grasa [%]",
        main="Enero")


#### Mes más frio2 (diciembre)

ext2018 <- read_excel(ext2018_path); ext2018$Month <- month(ext2018$`Fecha Sacrificio`)
ext2020 <- read_excel(ext2020_path); ext2020$Month <- month(ext2020$`Fecha Sacrificio`)
ext2021 <- read_excel(ext2021_path); ext2021$Month <- month(ext2021$`Fecha Sacrificio`)

ext2018 <- ext2018[ext2018$Month == 12,]; ext2018$camp <- 2018
ext2020 <- ext2020[ext2020$Month == 12,]; ext2020$camp <- 2020
ext2021 <- ext2021[ext2021$Month == 12,]; ext2021$camp <- 2021

names(ext2018) <- names(ext2020); names(ext2021) <- names(ext2018)
df_frio <- rbind(ext2018, ext2020, ext2021)
df_frio <- df_frio[df_frio$Grasa < 30,]
df_frio <- df_frio[df_frio$Grasa > 0,]

boxplot(df_frio$Grasa ~ df_frio$camp, xlab="", ylab="Grasa [%]",
        main="Enero")

#### Temp mes mas calido

temp$Month <- month(temp$Dia_ref)
temp_calido <- temp[temp$Month == 8,]

temp_calido_2018 <- temp_calido$Year_2018; df_cal2018 <- data.frame(temp18 = temp_calido$Year_2018); df_cal2018$camp <- 2018
temp_calido_2020 <- temp_calido$Year_2020; df_cal2020 <- data.frame(temp18 = temp_calido$Year_2020); df_cal2020$camp <- 2020
temp_calido_2021 <- temp_calido$Year_2021; df_cal2021 <- data.frame(temp18 = temp_calido$Year_2021); df_cal2021$camp <- 2021

df_calido <- rbind(df_cal2018, df_cal2020, df_cal2021)

boxplot(df_calido$temp18 ~ df_calido$camp, xlab="", ylab="Temperatura [ºC]", main="Agosto", ylim=c(25,30))

#### Temp mes mas frio

temp$Month <- month(temp$Dia_ref)
temp_calido <- temp[temp$Month == 1,]

temp_calido_2018 <- temp_calido$Year_2018; df_cal2018 <- data.frame(temp18 = temp_calido$Year_2018); df_cal2018$camp <- 2018
temp_calido_2020 <- temp_calido$Year_2020; df_cal2020 <- data.frame(temp18 = temp_calido$Year_2020); df_cal2020$camp <- 2020
temp_calido_2021 <- temp_calido$Year_2021; df_cal2021 <- data.frame(temp18 = temp_calido$Year_2021); df_cal2021$camp <- 2021

df_calido <- rbind(df_cal2018, df_cal2020, df_cal2021)

boxplot(df_calido$temp18 ~ df_calido$camp, xlab="", ylab="Temperatura [ºC]", main="Enero", ylim=c(12, 17))
