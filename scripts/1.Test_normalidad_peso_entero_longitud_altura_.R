# EPLORACIÓN DATOS DE EXTRACCIONES (EXTRACCIONS.xlsx -> Extractions.xlsx)

# Análisis de normalidad de las variables Peso_Entero, Longitud, Altura y Grasa.

library("readxl")
set.seed(1234)

# Workflow
# 1. Limpieza manual de tablas excel: Se eliminaron las columnas vacias y se pegaron los 
#    datos a un nuevo archivo .xlsx de texto plano.
# 2. Añadir registros con 0 o x para jaulas faltantes (marcado en rojo): Se añadieron dos muestras 
#    con valor 0 y 1 para las jaulas faltantes en cada año con objeto de tener 20 códigos de jaulas 
#    cada año. Se realizan los dos pasos anteriores para todos los años (pestaña).
# 3. Leer datos [2015:2021]: Se importaron los datos a R data.frame.
# 4. Graficar histogramas de 'Peso Entero' y pruebas de normalidad Shapiro.Wilk por jaula y para una 
#    pool de todas las jaulas. Se grafica la densidad de una distribución normal vs. real.
# 5. Graficar histogramas de 'Longitud' y pruebas de normalidad Shapiro.Wilk por jaula y para una 
#    pool de todas las jaulas. Se grafica la densidad de una distribución normal vs. real.
# 6. Graficar histogramas de 'Altura' y pruebas de normalidad Shapiro.Wilk por jaula y para una 
#    pool de todas las jaulas. Se grafica la densidad de una distribución normal vs. real.
# 7. Graficar histogramas de 'Grasa' y pruebas de normalidad Shapiro.Wilk por jaula y para una 
#    pool de todas las jaulas. Se grafica la densidad de una distribución normal vs. real.
# Nota: como prueba de normalidad se usa el test de Shapiro-Wilk con un número de muestras
# igual a 4*sqrt(M), donde M es el número total de muestras disponibles.

path <- 'C:/Users/sergi/Desktop/OptimaTUNA/data/extracciones/Extracciones.xlsx'

# Data
# $Matrícula: identificador individual del individuo
# $Artículo:
# $Presentación:
# $Peso: peso neto (después del evisceramiento)
# $Peso Entero: peso bruto (peso del individuo)
# $EBCD:
# $Grasa: porcentaje de grasa
# $Fecha Sacrificio: fecha de sacrificio
# $Ubicación: código de la jaula
# $Longitud: longitud del individuo (L)
# $Altura: altura del individuo (H)
# $Sexo: sexo del individuo (M/H)

# Selección de año de estudio (year_index)

years <- c("2021", "2020", "2019", "2018", "2017", "2016", "2015")
year_index <- 1

# Tabla de datos

data <- read_excel(path, sheet=year_index)
data <- data[complete.cases(data),]
head(data); names(data)

# Jaulas

print(paste0("Número de jaulas: ", length(unique(data$Ubicación))))
print(sort(unique(data$Ubicación)))

#### Distribución de pesos por jaula ####

jaulas_list <- list(); jaulas_names <- list()

i = 0
for(jaula in sort(unique(data$Ubicación))){
  i = i+1
  jaulas_list[[i]] <- data[data$Ubicación == jaula,]
  jaulas_names[[i]] <- jaula
}

par(mfrow=c(5,4))
for(i in 1:length(jaulas_list)){
  data <- jaulas_list[[i]]
  if(mean(data$`Peso Entero`) < 1){
    hist(data$`Peso Entero`, freq = FALSE, 
         xlab="Peso entero [Kg] ", ylab=" ",
         main=jaulas_names[[i]],
         las=1, tck=0.02,
         col="white", border="white")
    box()
  } else{
    sw_samp <- trunc(4*sqrt(length(data$`Peso Entero`)))
    sw_perc <- (sw_samp*100)/length(data$`Peso Entero`)
    hist(data$`Peso Entero`, freq = FALSE, 
         xlab="Peso entero [Kg] ", ylab=" ",
         main=paste0(jaulas_names[[i]], " (", years[[year_index]], ")"),
         sub=paste0("Shapiro-Wilk samples: ", sw_samp,
                    " (", trunc(sw_perc), " %)"),
         las=1, tck=0.02)
    x <- data$`Peso Entero`
    media <- mean(x); dt <- sd(x); min <- min(x); max <- max(x)
    curve(dnorm(x, media, dt), min, max, add=TRUE, lwd=2, col="black")
    sw_test <- shapiro.test(sample(data$`Peso Entero`, 
                                   sw_samp))
    #print(paste0("Muestras usadas para Shapiro-Wilk test :", 
    #             sw_samp))
    if(sw_test$p.value < 0.05){
      lines(density(data$`Peso Entero`), col="red", lwd=2)
      box()
    } else{
      lines(density(data$`Peso Entero`), col="limegreen", lwd=2)
      box()
    }
    
    
  }
}

# Descripción de la figura:
# El gráfico muestra el histograma de frecuencias de la variable "Peso Entero" para cada una de
# las jaulas en el año "year_index". La línea muestra las densidades de peso entero. Las
# las líneas verdes indican que superó el test de normalidad de Shapiro-Wilk, mientras que las
# lineas rojas indican que no superó el test.
# Los histogramas en blanco indican que no existen datos para dicha jaula.

#### Distribución de pesos para la pool de todas las jaulas ####

par(mfrow=c(1,1))
data <- read_excel(path, sheet=year_index)
data <- data[complete.cases(data),]
hist(data$`Peso Entero`, freq = FALSE, 
     xlab="Peso entero [Kg]", ylab=" ",
     main=paste0("Pool ", years[[year_index]]),
     las=1, tck=0.02,
     col="gray", border="black")
x <- data$`Peso Entero`
media <- mean(x); dt <- sd(x); min <- min(x); max <- max(x)
curve(dnorm(x, media, dt), min, max, add=TRUE, lwd=2, col="black")
sw_test <- shapiro.test(sample(data$`Peso Entero`, 
                               4*sqrt(length(data$`Peso Entero`)), 
                               replace=FALSE))
print(paste0("Muestras usadas para Shapiro-Wilk test :", 
             trunc(4*sqrt(length(data$`Peso Entero`)))))
if(sw_test$p.value < 0.05){
  lines(density(data$`Peso Entero`), col="red", lwd=2)
  box()
} else{
  lines(density(data$`Peso Entero`), col="limegreen", lwd=2)
  box()
}

#### Distribución de longitudes por jaula ####

jaulas_list <- list(); jaulas_names <- list()

i = 0
for(jaula in sort(unique(data$Ubicación))){
  i = i+1
  jaulas_list[[i]] <- data[data$Ubicación == jaula,]
  jaulas_names[[i]] <- jaula
}

par(mfrow=c(5,4))
for(i in 1:length(jaulas_list)){
  data <- jaulas_list[[i]]
  if(mean(data$`Longitud`) < 1){
    hist(data$`Longitud`, freq = FALSE, 
         xlab="Longitud [cm] ", ylab=" ",
         main=jaulas_names[[i]],
         las=1, tck=0.02,
         col="white", border="white")
    box()
  } else{
    sw_samp <- trunc(4*sqrt(length(data$`Longitud`)))
    sw_perc <- (sw_samp*100)/length(data$`Longitud`)
    hist(data$`Longitud`, freq = FALSE, 
         xlab="Longitud [cm] ", ylab=" ",
         main=paste0(jaulas_names[[i]], " (", years[[year_index]], ")"),
         sub=paste0("Shapiro-Wilk samples: ", sw_samp,
                    " (", trunc(sw_perc), " %)"),
         las=1, tck=0.02)
    x <- data$`Longitud`
    media <- mean(x); dt <- sd(x); min <- min(x); max <- max(x)
    curve(dnorm(x, media, dt), min, max, add=TRUE, lwd=2, col="black")
    sw_test <- shapiro.test(sample(data$`Longitud`, 
                                   sw_samp))
    print(paste0("Muestras usadas para Shapiro-Wilk test :", 
                 sw_samp))
    if(sw_test$p.value < 0.05){
      lines(density(data$`Longitud`), col="red", lwd=2)
      box()
    } else{
      lines(density(data$`Longitud`), col="limegreen", lwd=2)
      box()
    }
    
    
  }
}

# Descripción de la figura:
# El gráfico muestra el histograma de frecuencias de la variable "Longitud" para cada una de
# las jaulas en el año "year_index". La línea muestra las densidades de peso entero. Las
# las líneas verdes indican que superó el test de normalidad de Shapiro-Wilk, mientras que las
# lineas rojas indican que no superó el test.
# Los histogramas en blanco indican que no existen datos para dicha jaula.


#### Distribución de longitudes para la pool de todas las jaulas ####

par(mfrow=c(1,1))
data <- read_excel(path, sheet=year_index)
data <- data[complete.cases(data),]
hist(data$`Longitud`, freq = FALSE, 
     xlab="Longitud [cm]", ylab=" ",
     main=paste0("Pool ", years[[year_index]]),
     las=1, tck=0.02,
     col="gray", border="black")
x <- data$`Longitud`
media <- mean(x); dt <- sd(x); min <- min(x); max <- max(x)
curve(dnorm(x, media, dt), min, max, add=TRUE, lwd=2, col="black")
sw_test <- shapiro.test(sample(data$`Longitud`, 
                               4*sqrt(length(data$`Longitud`)), 
                               replace=FALSE))
print(paste0("Muestras usadas para Shapiro-Wilk test :", 
             trunc(4*sqrt(length(data$`Longitud`)))))
if(sw_test$p.value < 0.05){
  lines(density(data$`Longitud`), col="red", lwd=2)
  box()
} else{
  lines(density(data$`Longitud`), col="limegreen", lwd=2)
  box()
}

#### Distribución de alturas por jaula ####

jaulas_list <- list(); jaulas_names <- list()

i = 0
for(jaula in sort(unique(data$Ubicación))){
  i = i+1
  jaulas_list[[i]] <- data[data$Ubicación == jaula,]
  jaulas_names[[i]] <- jaula
}

par(mfrow=c(5,4))
for(i in 1:length(jaulas_list)){
  data <- jaulas_list[[i]]
  if(mean(data$`Alto`) < 1){
    hist(data$`Alto`, freq = FALSE, 
         xlab="Altura [cm] ", ylab=" ",
         main=jaulas_names[[i]],
         las=1, tck=0.02,
         col="white", border="white")
    box()
  } else{
    sw_samp <- trunc(4*sqrt(length(data$`Alto`)))
    sw_perc <- (sw_samp*100)/length(data$`Alto`)
    hist(data$`Alto`, freq = FALSE, 
         xlab="Altura [cm] ", ylab=" ",
         main=paste0(jaulas_names[[i]], " (", years[[year_index]], ")"),
         sub=paste0("Shapiro-Wilk samples: ", sw_samp,
                    " (", trunc(sw_perc), " %)"),
         las=1, tck=0.02)
    x <- data$`Alto`
    media <- mean(x); dt <- sd(x); min <- min(x); max <- max(x)
    curve(dnorm(x, media, dt), min, max, add=TRUE, lwd=2, col="black")
    sw_test <- shapiro.test(sample(data$`Alto`, 
                                   sw_samp))
    print(paste0("Muestras usadas para Shapiro-Wilk test :", 
                 sw_samp))
    if(sw_test$p.value < 0.05){
      lines(density(data$`Alto`), col="red", lwd=2)
      box()
    } else{
      lines(density(data$`Alto`), col="limegreen", lwd=2)
      box()
    }
    
    
  }
}

# Descripción de la figura:
# El gráfico muestra el histograma de frecuencias de la variable "Alto" para cada una de
# las jaulas en el año "year_index". La línea muestra las densidades de peso entero. Las
# las líneas verdes indican que superó el test de normalidad de Shapiro-Wilk, mientras que las
# lineas rojas indican que no superó el test.
# Los histogramas en blanco indican que no existen datos para dicha jaula.


#### Distribución de alturas para la pool de todas las jaulas ####

par(mfrow=c(1,1))
data <- read_excel(path, sheet=year_index)
data <- data[complete.cases(data),]
hist(data$`Alto`, freq = FALSE, 
     xlab="Altura [cm]", ylab=" ",
     main=paste0("Pool ", years[[year_index]]),
     las=1, tck=0.02,
     col="gray", border="black")
x <- data$`Alto`
media <- mean(x); dt <- sd(x); min <- min(x); max <- max(x)
curve(dnorm(x, media, dt), min, max, add=TRUE, lwd=2, col="black")
sw_test <- shapiro.test(sample(data$`Alto`, 
                               4*sqrt(length(data$`Alto`)), 
                               replace=FALSE))
print(paste0("Muestras usadas para Shapiro-Wilk test :", 
             trunc(4*sqrt(length(data$`Alto`)))))
if(sw_test$p.value < 0.05){
  lines(density(data$`Alto`), col="red", lwd=2)
  box()
} else{
  lines(density(data$`Alto`), col="limegreen", lwd=2)
  box()
}

#### Distribución de grasa por jaula ####

jaulas_list <- list(); jaulas_names <- list()

i = 0
for(jaula in sort(unique(data$Ubicación))){
  i = i+1
  jaulas_list[[i]] <- data[data$Ubicación == jaula,]
  jaulas_names[[i]] <- jaula
}

par(mfrow=c(5,4))
for(i in 1:length(jaulas_list)){
  data <- jaulas_list[[i]]
  if(mean(data$`Grasa`) < 1){
    hist(data$`Grasa`, freq = FALSE, 
         xlab="Grasa [%] ", ylab=" ",
         main=jaulas_names[[i]],
         las=1, tck=0.02,
         col="white", border="white")
    box()
  } else{
    sw_samp <- trunc(4*sqrt(length(data$`Grasa`)))
    sw_perc <- (sw_samp*100)/length(data$`Grasa`)
    hist(data$`Grasa`, freq = FALSE, 
         xlab="Grasa [%] ", ylab=" ",
         main=paste0(jaulas_names[[i]], " (", years[[year_index]], ")"),
         sub=paste0("Shapiro-Wilk samples: ", sw_samp,
                    " (", trunc(sw_perc), " %)"),
         las=1, tck=0.02)
    x <- data$`Grasa`
    media <- mean(x); dt <- sd(x); min <- min(x); max <- max(x)
    curve(dnorm(x, media, dt), min, max, add=TRUE, lwd=2, col="black")
    sw_test <- shapiro.test(sample(data$`Grasa`, 
                                   sw_samp))
    print(paste0("Muestras usadas para Shapiro-Wilk test :", 
                 sw_samp))
    if(sw_test$p.value < 0.05){
      lines(density(data$`Grasa`), col="red", lwd=2)
      box()
    } else{
      lines(density(data$`Grasa`), col="limegreen", lwd=2)
      box()
    }
    
    
  }
}

# Descripción de la figura:
# El gráfico muestra el histograma de frecuencias de la variable "Grasa" para cada una de
# las jaulas en el año "year_index". La línea muestra las densidades de peso entero. Las
# las líneas verdes indican que superó el test de normalidad de Shapiro-Wilk, mientras que las
# lineas rojas indican que no superó el test.
# Los histogramas en blanco indican que no existen datos para dicha jaula.


#### Distribución de grasa para la pool de todas las jaulas ####

par(mfrow=c(1,1))
data <- read_excel(path, sheet=year_index)
data <- data[complete.cases(data),]
hist(data$`Grasa`, freq = FALSE, 
     xlab="Grasa [%]", ylab=" ",
     main=paste0("Pool ", years[[year_index]]),
     las=1, tck=0.02,
     col="gray", border="black")
x <- data$`Grasa`
media <- mean(x); dt <- sd(x); min <- min(x); max <- max(x)
curve(dnorm(x, media, dt), min, max, add=TRUE, lwd=2, col="black")
sw_test <- shapiro.test(sample(data$`Grasa`, 
                               4*sqrt(length(data$`Grasa`)), 
                               replace=FALSE))
print(paste0("Muestras usadas para Shapiro-Wilk test :", 
             trunc(4*sqrt(length(data$`Grasa`)))))
if(sw_test$p.value < 0.05){
  lines(density(data$`Grasa`), col="red", lwd=2)
  box()
} else{
  lines(density(data$`Grasa`), col="limegreen", lwd=2)
  box()
}



'
Las variables Longitud, Altura y Grasa parecen NO NORMALES. Mientras que la variable Peso_Entero parece NORMAL.
Sin embargo, en algunas jaulas salen distribuciones NO NORMALES. Andrés me sugiere cambiar el valor alpha de P (¿p-valor?).
¿Esto es el valor crítico de p? COnsultar con Pepe.
¿Puedo asumir la normalidad de la variable Peso_Entero para el año 2021?
'

