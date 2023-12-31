# EXPLORACI�N DE LA RELACI�N ENTRE LAS VARIABLES PESO ENTERO Y GRASA

# �Existe una correlaci�n positiva entre el peso de los individuos y el porcentaje de grasa infiltrada?
# Es decir, a medida que los atunes son m�s grandes tienen mayor porcentaje de grasa?

library("readxl")
set.seed(1234)

# Workflow
# 1. Limpieza manual de tablas excel: Se eliminaron las columnas vacias y se pegaron los 
#    datos a un nuevo archivo .xlsx de texto plano.
# 2. A�adir registros con 0 o x para jaulas faltantes (marcado en rojo): Se a�adieron dos muestras 
#    con valor 0 y 1 para las jaulas faltantes en cada a�o con objeto de tener 20 c�digos de jaulas 
#    cada a�o. Se realizan los dos pasos anteriores para todos los a�os (pesta�a).
# 3. Leer datos [2015:2021]: Se importaron los datos a R data.frame.
# 4. Grafica un scatterplot del Peso_Entero [Kg] vs Grasa [%] y calcula el coeficiente de correlaci�n
#    de Pearson y ajusta un modelo lineal y calcula su R^2.

path <- 'C:/Users/sergi/Desktop/OptimaTUNA/data/extracciones/Extracciones.xlsx'

# Data
# $Matr�cula: identificador individual del individuo
# $Art�culo:
# $Presentaci�n:
# $Peso: peso neto (despu�s del evisceramiento)
# $Peso Entero: peso bruto (peso del individuo)
# $EBCD:
# $Grasa: porcentaje de grasa
# $Fecha Sacrificio: fecha de sacrificio
# $Ubicaci�n: c�digo de la jaula
# $Longitud: longitud del individuo (L)
# $Altura: altura del individuo (H)
# $Sexo: sexo del individuo (M/H)

# Selecci�n de a�o de estudio (year_index)

years <- c("2021", "2020", "2019", "2018", "2017", "2016", "2015")
year_index <- 1

# Tabla de datos

data <- read_excel(path, sheet=year_index)
data <- data[complete.cases(data),]
head(data); names(data)

# Seleccion de jaula

print(paste0("N�mero de jaulas: ", length(unique(data$Ubicaci�n))))
print(sort(unique(data$Ubicaci�n)))

# Descomentar la siguiente l�nea para seleccionar una jaula
# sino se realiza el an�lisis para una pool de todas las jaulas
#data <- data[data$Ubicaci�n == 'JAL01', ]

# Elimina valores de medici�n de grasa err�neos con un humbral de 50%.
# Valores de grasa mayores del 50% se consideran errores.
data <- data[data$Grasa < 30, ]

# An�lisis OLS - Ordinary Least Squares regression

p <- data$`Peso Entero`
g <- data$`Grasa`

plot(p,g, las=1, tck=0.02, xlab="Peso entero [Kg]", ylab="Grasa [%]", pch=16, col="dodgerblue1",
     main="Relaci�n Peso Entero -vs- Grasa (pool 2021)",
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
Existe una correlaci�n positiva pero muy baja entre el tama�o de los individuos y 
el porcentaje de grasa infiltrada.
'

# An�lisis TLS - Total Least Squares regression

#install.packages("pracma")
library(pracma)

odr <- odregress(g, p)
print(odr$coeff)

plot(p,g, las=1, tck=0.02, xlab="Peso entero [Kg]", ylab="Grasa [%]", pch=16, col="dodgerblue1",
     main="Relaci�n Peso Entero -vs- Grasa (pool 2021)",
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
