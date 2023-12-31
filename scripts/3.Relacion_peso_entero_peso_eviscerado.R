# EXPLORACI�N DE LA RELACI�N ENTRE LAS VARIABLES PESO ENTERO Y PESO EVISCERADO

# �Existe un valor de peso entero que maximice el peso eviscerado?
# Ej. Se cumple que: a un mayor peso del individuo el porcentaje de masa que se
# pierde al eviscerar es menor?

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

# Relaci�n peso entero -vs- peso eviscerado

pt <- data$`Peso Entero`
pe <- data$`Peso` # Peso eviscerado

porcentaje_perdida <- 100-(pe*100/pt)
absoluto_perdida <- pt-pe

# Se observan algunos registros con 0 Kg de p�rdida de masa
# Se consideran errores
sample_index <- 1:length(absoluto_perdida)
df_perdida <- data.frame(sample_index, absoluto_perdida, porcentaje_perdida, pt, pe)
plot(df_perdida$sample_index, df_perdida$absoluto_perdida, 
     xlab="Sample index", ylab="P�rdida de masa [Kg]",
     pch=1, col="dodgerblue1", 
     main="P�rdida de masa al eviscerar")
df_perdida_0 <- df_perdida[df_perdida$absoluto_perdida == 0, ]
points(df_perdida_0$sample_index, df_perdida_0$absoluto_perdida, col="firebrick1")

# Se observa que el peso eviscerado (Peso) se calcula aplicando el 20% y el 11.5%
# a partir del peso entero
df_perdida_n0 <- df_perdida[df_perdida$absoluto_perdida != 0, ]
plot(df_perdida_n0$pt, df_perdida_n0$porcentaje_perdida,
     xlab="Peso Entero [Kg]", ylab="P�rdida de masa [%]",
     pch=1, col="dodgerblue1", 
     main="Porcentaje de p�rdida en funci�n del peso")

'
Se observa que el Peso (eviscerado) no se mide sino que se calcula aplicando una
relaci�n de -11.5% o -20.0% al Peso_Entero. Por tanto, no podemos buscar el peso
opimo al que se minimiza el % de p�rdida de masa al eviscerar.
'