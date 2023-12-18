# EXPLORACIÓN DE LA RELACIÓN ENTRE LAS VARIABLES PESO ENTERO Y GRASA

# ¿Existe una correlación positiva entre el peso de los individuos y el porcentaje de grasa infiltrada?
# Es decir, a medida que los atunes son más grandes tienen mayor porcentaje de grasa?

library("readxl")
set.seed(1234)

# Workflow
# 1. Limpieza manual de tablas excel: Se eliminaron las columnas vacias y se pegaron los 
#    datos a un nuevo archivo .xlsx de texto plano.
# 2. Añadir registros con 0 o x para jaulas faltantes (marcado en rojo): Se añadieron dos muestras 
#    con valor 0 y 1 para las jaulas faltantes en cada año con objeto de tener 20 códigos de jaulas 
#    cada año. Se realizan los dos pasos anteriores para todos los años (pestaña).
# 3. Leer datos [2015:2021]: Se importaron los datos a R data.frame.
# 4. Grafica un scatterplot del Peso_Entero [Kg] vs Grasa [%] y calcula el coeficiente de correlación
#    de Pearson y ajusta un modelo lineal y calcula su R^2.

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

# Seleccion de jaula

print(paste0("Número de jaulas: ", length(unique(data$Ubicación))))
print(sort(unique(data$Ubicación)))

# Descomentar la siguiente línea para seleccionar una jaula
# sino se realiza el análisis para una pool de todas las jaulas
#data <- data[data$Ubicación == 'JAL01', ]

# Elimina valores de medición de grasa erróneos con un humbral de 50%.
# Valores de grasa mayores del 50% se consideran errores.
data <- data[data$Grasa < 30, ]

# Análisis OLS - Ordinary Least Squares regression

p <- data$`Peso Entero`
g <- data$`Grasa`

plot(p,g, las=1, tck=0.02, xlab="Peso entero [Kg]", ylab="Grasa [%]", pch=16, col="dodgerblue1",
     main="Relación Peso Entero -vs- Grasa (pool 2021)",
     xlim=c(0,600))
fit <- lm(g~p)
abline(fit, col="red", lwd=3)
summary(fit); s <- summary(fit)
abline(h=mean(g), col="black", lty=2)

c <- cor(p,g, method="spearman")

legend("topleft", paste("??-Spearman's: ", round(c, digits=3)), bty="n")

r2 = s$adj.r.squared
legend("topright", paste("R-squared: ", round(r2, digits=3), "   "), bty="n")

'
Existe una correlación positiva pero muy baja entre el tamaño de los individuos y 
el porcentaje de grasa infiltrada.
'

# Análisis TLS - Total Least Squares regression

#install.packages("pracma")
library(pracma)

odr <- odregress(g, p)
print(odr$coeff)

plot(p,g, las=1, tck=0.02, xlab="Peso entero [Kg]", ylab="Grasa [%]", pch=16, col="dodgerblue1",
     main="Relación Peso Entero -vs- Grasa (pool 2021)",
     xlim=c(0,600))
abline(odr$coeff[2], odr$coeff[1])

# MSE, RMSE and R^2
mse_odreg <- function(object) mean(object$resid^2)
rmse_odreg <- function(object) sqrt(mse_odreg(object))
r_squared_odreg <- function(object, y) {
  denom <- sum((y - mean(y))^2)
  1 - object$ssq/denom
}

mse_odreg(odr)
rmse_odreg(odr)
r_squared_odreg(odr, g)
