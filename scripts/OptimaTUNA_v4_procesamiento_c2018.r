
# ---------- OptimaTUNA_v4.R ----------

# Titulo: Procesamiento de datos del proyecto OptimaTUNA. Version v4.
# Autor: Sergio Morell-Monzo
# Fecha actualizacion: 12/06/2023
# Test: OK

#### 0.1. DEFINE INPUTS DE ENTRADA ####

# El script espera una tabla excel MEDIDIONES_ALTA_MAR_Y_ENJAULAMIENTOS.xlsx
MEDICIONES <- "C:/Users/PECES-i9/Desktop/OptimaTUNA/Datos_ok/c2018/Mediciones_alta_mar_y_enjaulamientos_c2018_r.xlsx"
HOJA_MEDICIONES <- 1

# El script espera una tabla excel TRASPASOS_INTERNOS.xlsx con las siguientes columnas en el siguiente orden:
# "Fecha de solicitud", "Codigo", "Jaula_origen", "Jaula_destino", "Fecha_de_realizacion", "Estado", "Campa?a" 
# El campo "Jaula origen" y "Jaula destion" deben estar en formato entero
# El campo "Fecha de realizacion" debe estar en formato DD/MM/YYYY
TRASPASOS_INTERNOS <- "C:/Users/PECES-i9/Desktop/OptimaTUNA/Datos_ok/c2018/Traspasos_internos_c2018_r.xlsx"
HOJA_TRASPASOS_INTERNOS <- 1

# El script espera una tabla excel EXTRACCIONES.xlsx con las siguientes columnas en el siguiente orden:
# "Matricula", "Articulo", "Presentacion", "Peso", "Peso_entero", "EBCD", "Fecha_sacrificio", "Ubicacion", "Longitud", "Alto", "Sexo"
# Los campos "Peso", "Peso_entero" y "Grasa" deben estar en formato decimal (separado por coma)
# El campo "Fecha_sacrificio" debe estar en formato DD/MM/YYYY
# El campo "Longitud" y "Altura" deben estar en formato entero (unidad en cm.)
# El campo "Ubicacion" que identifica la jaula debe usar los c?digos JAL01, JAL02, JAL03...
EXTRACCIONES <- "C:/Users/PECES-i9/Desktop/OptimaTUNA/Datos_ok/c2018/Extracciones_c2018.xlsx" # Crear versi?n "_r.xlsx"
HOJA_EXTRACCIONES <- 1

# El script espera una tabla excel ALIMENTACION_DIARIA.xlsx con las siguientes columnas en el siguiente orden:
# 'Fecha Alimentacion', 'Cantidad en KG', 'LOC_0', 'MFGTRKNUM_0' 'MFGNUM_0', 'Campa?a', 'ITMREF_0', 'BOMNUM_0', 'YFLAG_0', 'ESPECIE', 'LOT_0', 'GRASA_0', 'CALIBRE_0', 'JAULA'
# El campo 'Fecha Alimentacion' debe estar en formato DD/MM/YYY
# El campo 'Cantidad en KG' debe estar en KG
# El campo 'ESPECIE' deben ser strings separados por espacios
# El campo 'GRASA_0' y 'CALIBRE_0' deben ser float separado por comas (unidades % y cm. respectivamente)
ALIMENTACION_DIARIA <- "C:/Users/PECES-i9/Desktop/OptimaTUNA/Datos_ok/c2018/Alimentacion_diaria_c2018_r.xlsx" # Crear versi?n "_r.xlsx"
HOJA_ALIMENTACION_DIARIA <- 1

# El script espera una tabla excel FECHA_CONGELACION.xlsx con las siguientes columnas en el siguiente orden:
# `Fecha Congelacion`, `LOT`
# El campo `Fecha Congelacion`contiene la fecha en que se congel? el lote.
# El campo `LOT`contiene el c?digo del lote.
FECHA_CONGELACION <- "E:/OptimaTUNA/data/alimentacion_diaria/Fecha_congelacion_r.xlsx" # Crear versi?n "_r.xlsx"
HOLA_FECHA_CONGELACION <- 1

# A?o de la campa?a y hoja de los archivos excel a analizar
# Para ver las hojas que contiene cada archivo excel puede usar:

# if(!require("tidyverse")) install.packages("tidyverse")
# TRASPASOS_INTERNOS %>%
#   excel_sheets()
# EXTRACCIONES %>%
#   excel_sheets()

CAMPAÑA <- 2018
NUMERO_JAULAS <- 20

#### 0.2. PREPARA EL ENTORNO DE TRABAJO ####

# Set work directory and environment
#dirname(rstudioapi::getSourceEditorContext()$path) |> setwd()
#print(paste0("Current directory at: ", getwd()))

source("E:/OptimaTUNA/Analysis_scripts/automatizacion_funciones.r")

#### 0.3. LECTURA DE ARCHIVOS ####

if(file.exists(MEDICIONES) & file.exists(EXTRACCIONES) & file.exists(TRASPASOS_INTERNOS) & file.exists(ALIMENTACION_DIARIA) & file.exists(FECHA_CONGELACION)){
  traspasos_internos <- readxl::read_excel(TRASPASOS_INTERNOS)
  extracciones <- readxl::read_excel(EXTRACCIONES)
  mediciones <- readxl::read_excel(MEDICIONES)
  alimentacion_diaria <- readxl::read_excel(ALIMENTACION_DIARIA)
  fecha_congelacion <- readxl::read_excel(FECHA_CONGELACION)
} else{
  message("Alguno de los archivos de entrada no est? correctamente localizado.
Compruebe las rutas a los archivos: TRASPASOS_INTERNOS, EXTRACCIONES, MEDICIONES, ALIMENTACION_DIARIA y FECHA_CONGELACION")
}



#### 1.1. PREPARACION ARCHIVO MEDICIONES ####

# Check column names
expected_names_mediciones <- c("Fecha", "Remolcador", "Jaula_de_transporte", "Jaula_en_granja", 
                               "Numero_Balfego", "Longitud_m_Balfego", "Peso_Kg_Balfego", 
                               "Numero_Inspectores", "Longitud_m_Inspectores", "Peso_Kg_Inspectores", 
                               "Numero_UPV", "Longitud_m_UPV", "Peso_Kg_UPV")
names(mediciones) <- expected_names_mediciones

# Show table columns
str(mediciones)

#### 1.2. PREPARACION ARCHIVO TRASPASOS INTERNOS ####

# Check column names
expected_names_traspasos_internos <- c("Fecha_solicitud", "Codigo", "Jaula_origen", "Jaula_destino", "Fecha_de_realizacion", "Estado", "Campana", "Piezas", "Tipo_de_traspaso")
names(traspasos_internos) <- expected_names_traspasos_internos

# Remove records from previous campaigns
traspasos_internos <- traspasos_internos[traspasos_internos$Campana == CAMPAÑA,]

# Define some data types as numeric
traspasos_internos$Campaña <- as.numeric(traspasos_internos$Campana)
traspasos_internos$Piezas <- as.numeric(traspasos_internos$Piezas)
# Convert $Fecha_de_realizacion to POSIXct date-time conversion function
traspasos_internos$Fecha_de_realizacion <- as.POSIXct(traspasos_internos$Fecha_de_realizacion)

# Show table columns
str(traspasos_internos)

#### 1.3. PREPARACION ARCHIVO ALIMENTACION DIARIA ####

# Check column names and datatypes
expected_names_alimentacion_diaria <- c("Fecha_alimentacion", "Cantidad_Kg_acum", "Loc", "MFGTRKNUM", "MFGNUM", "Campaña", "ITMREF", "BOMNUM", "YFLAG", "Especie", "Zeros", "Lot", "Cantidad_Kg", "Grasa", "Calibre_cm", "Jaula")
names(alimentacion_diaria) <- expected_names_alimentacion_diaria

# Convert JAL code to JAL number
alimentacion_diaria$Jaula <- jal_code_2_jal_number(alimentacion_diaria$Jaula)

# Convert Lot to character (necessari for Fecha_congelacion indexaction)
alimentacion_diaria$Lot <- as.character(alimentacion_diaria$Lot)

# Show table columns
str(alimentacion_diaria)

# Remove non-useful fields
alimentacion_diaria$MFGTRKNUM <- NULL
alimentacion_diaria$MFGNUM <- NULL
alimentacion_diaria$ITMREF <- NULL
alimentacion_diaria$BOMNUM <- NULL

#### 1.4. PREPARACION ARCHIVO EXTRACCIONES ####

# Check column names
expected_names_extracciones <- c("Matricula", "Articulo", "Presentacion", "Peso_Kg", "Peso_entero_Kg", "EBCD", "Grasa", "Fecha_sacrificio", "Ubicacion", "Longitud", "Alto", "Sexo")
names(extracciones) <- expected_names_extracciones

# Convert JAL code to JAL number
extracciones$Jaula <- jal_code_2_jal_number(extracciones$Ubicacion)

# Fecha_sacrificio to Date (Not run!)
#extracciones$Fecha_sacrificio <- as.Date(extracciones$Fecha_sacrificio)

# Show table columns
str(extracciones)

#### 1.5. PREPARACION ARCHIVO FECHA CONGELACION ####

# Check column names
expected_names_fecha_congelacion <- c("Fecha_congelacion", "Lot")
names(fecha_congelacion) <- expected_names_fecha_congelacion

# Convert Lot to character (necessari for Alimentacion_diaria indexation) and Fecha_congelacion to Date
fecha_congelacion$Lot <- as.character(fecha_congelacion$Lot)
fecha_congelacion$Fecha_congelacion <- as.Date(fecha_congelacion$Fecha_congelacion)

# Show table columns
str(fecha_congelacion)



#### 2.1. MUESTRA DISCREPANCIAS ENTRE MEDICIONES BALFEGO / INSPECTORES ####

# Years 2019, 2020 and 2021 aggregated
MEDICIONES_AGGREGATED <- "E:/OptimaTUNA/data/mediciones/Mediciones_inspectores_vs_balfego.xlsx"

# Read data from file
mediciones_inspec_vs_balfego <- readxl::read_excel(MEDICIONES_AGGREGATED)
names(mediciones_inspec_vs_balfego) <- c("Jaula", "Balfego_Kg", "Inspec_Kg", "Ano")

# Convert JAL code to JAL number
mediciones_inspec_vs_balfego$Jaula <- jal_code_2_jal_number(mediciones_inspec_vs_balfego$Jaula)

# Show table columns
str(mediciones_inspec_vs_balfego)

# Identify discrepances between input mesurements Balfego-Inspectores-UPV
par(mfrow=c(1,3))
options(repr.plot.width=20, repr.plot.height=6.5)
acc <- c(50, 100, 120, 140, 160, 180, 200, 300, 400, 500)

plot(mediciones_inspec_vs_balfego[mediciones_inspec_vs_balfego$Ano == 2019, ]$Inspec_Kg, mediciones_inspec_vs_balfego[mediciones_inspec_vs_balfego$Ano == 2019, ]$Balfego_Kg,
     xlab="Peso [Kg] Inspectores", ylab="Peso [Kg] Balfeg?",
     las=1, tck=0.02, col="deepskyblue3", pch=19, lwd=8,
     main="2019", cex.lab=1.8, cex.axis=1.5, cex.main=3)
lines(acc, acc, col="red", lwd=2)

plot(mediciones_inspec_vs_balfego[mediciones_inspec_vs_balfego$Ano == 2020, ]$Inspec_Kg, mediciones_inspec_vs_balfego[mediciones_inspec_vs_balfego$Ano == 2020, ]$Balfego_Kg,
     xlab="Peso [Kg] Inspectores", ylab="Peso [Kg] Balfeg?",
     las=1, tck=0.02, col="deepskyblue3", pch=19, lwd=8,
     main="2020", cex.lab=1.8, cex.axis=1.5, cex.main=3)
lines(acc, acc, col="red", lwd=2)

plot(mediciones_inspec_vs_balfego[mediciones_inspec_vs_balfego$Ano == 2021, ]$Inspec_Kg, mediciones_inspec_vs_balfego[mediciones_inspec_vs_balfego$Ano == 2021, ]$Balfego_Kg,
     xlab="Peso [Kg] Inspectores", ylab="Peso [Kg] Balfeg?",
     las=1, tck=0.02, col="deepskyblue3", pch=19, lwd=8,
     main="2021", cex.lab=1.8, cex.axis=1.5, cex.main=3)
lines(acc, acc, col="red", lwd=2)

#### 2.2. ANALISIS ESTADISCITO DE LAS DIFERENCIAS ENCONTRADAS ####

# Boxplot
par(mfrow=c(1,1))
options(repr.plot.width=9, repr.plot.height=8)
color <- c("red", "deepskyblue3")
boxplot(mediciones_inspec_vs_balfego$Inspec_Kg, mediciones_inspec_vs_balfego$Balfego_Kg, col=color, ylab="Peso [Kg]")
legend("topright", inset=.02, title="Peso [Kg]",
       c("Inspectores","Balfeg?"), fill=color, horiz=TRUE, cex=1.2)

# File prepared for ANOVA
mediciones_anova <- readxl::read_excel(gsub(".xlsx", "_anova.xlsx", MEDICIONES_AGGREGATED))
mediciones_anova$Grupo <- as.factor(mediciones_anova$Grupo)

str(mediciones_anova)

# Shapito-Wilk test
g1_test <- shapiro.test(mediciones_inspec_vs_balfego$Inspec_Kg)
g2_test <- shapiro.test(mediciones_inspec_vs_balfego$Balfego_Kg)

# Normality
print(paste("Grupo 1"))
print(g1_test)
print(paste("Grupo 2"))
print(g2_test)

# ANOVA
anova <- aov(Medicion_Kg~Grupo, mediciones_anova)
summary(anova)



#### 3.1. OBTENCION FECHAS DE ENTRADA CON MEDICIONES DISPONIBLES Y NUM. DE INDIVIDUOS PRO JAULA ####

# Locate entry dates
involved_jal <- sort(unique(mediciones$Jaula_en_granja))
print(paste0("Jaulas donde se realizaron mediciones a la entrada: "))
print(involved_jal)
entry_dates <- list()
for(i in 1:length(involved_jal)){
  df <- mediciones[mediciones$Jaula_en_granja == involved_jal[[i]], ]
  entry_dates[[i]] <- max(df$Fecha)
}

# Create data.frame containing the entry date for each JAL
entry_dates_df <- data.frame(
  jal_number = sort(unique(mediciones$Jaula_en_granja)),
  entry_date = as.Date(as.POSIXct(unlist(entry_dates), origin="1970-01-01")),
  entry_unix = unlist(entry_dates)
)

# Save data.frame
#write.csv(entry_dates_df, "C:path/to/file.csv")

# Aggregate mediciones by JAL [Numero_*, Longitud_m_*, Peso_Kg_*]
mediciones_aggregated <- aggregate(mediciones[,c("Numero_Balfego", "Longitud_m_Balfego", "Peso_Kg_Balfego", 
                                                 "Numero_Inspectores", "Longitud_m_Inspectores", "Peso_Kg_Inspectores", 
                                                 "Numero_UPV", "Longitud_m_UPV", "Peso_Kg_UPV")],
                                   by=list(c(JAULA=mediciones$Jaula_en_granja)),
                                   FUN=sum)

# Remove non-useful columns
mediciones_aggregated$Longitud_m_Balfego <- NULL
mediciones_aggregated$Peso_Kg_Balfego <- NULL

mediciones_aggregated$Longitud_m_Inspectores <- NULL
mediciones_aggregated$Peso_Kg_Inspectores <- NULL

mediciones_aggregated$Longitud_m_UPV <- NULL
mediciones_aggregated$Peso_Kg_UPV <- NULL

# Rename columns
names(mediciones_aggregated) <- c("jal_number", "Numero_Balfego", "Numero_Inspectores", "Numero_UPV")

# Plot Numero and Peso by JAL
if(FALSE){
  par(mfrow=c(1,2))
  pie(mediciones_aggregated$Numero_UPV, mediciones_aggregated$Jaula_en_granja,
      main="N?mero de individuos por jaula")
}

# Show data.frame (table)
mediciones_aggregated

#### 3.2. CALCULA BRACKPOINTS PARA CADA JAULA ####

# Locate transfers received for each JAL
J <- sort(unique(traspasos_internos$Jaula_destino))
breakpoint_date <- c()
for(i in 1:length(J)){
  # Transfers recived
  ds <- traspasos_internos[traspasos_internos$Jaula_destino == J[[i]],]
  # Save breakpoints
  dt <- min(ds$Fecha_de_realizacion)
  #if(dt==Inf){
  #  print(paste0("No movements at JAL ", J))
  #  dt=NA}
  #print(paste0(J[[i]], " at ", dt))
  print(paste0("Breakpoint detected at ", dt, " in JAL ", J[[i]]))
  breakpoint_date <- append(breakpoint_date, dt)
}

breakpoint_dates_df <- data.frame(
  jal_number = sort(unique(traspasos_internos$Jaula_destino)),
  breakpoint_date = unlist(breakpoint_date),
  breakpoint_unix = as.numeric(as.POSIXct(breakpoint_date))
)

# Show data.frame (table)
#breakpoint_dates_df

#### 3.3. CREA UNA TABLA CON EL PERIODO DE ESTUDIO PARA CADA JAULA ####

# Create dataframe of valid periods
valid_periods_df <- merge(x = entry_dates_df, y = breakpoint_dates_df, 
                          by = "jal_number", 
                          all.x = TRUE)

# Show data.frame (table)
valid_periods_df

# No breakpoint ID: 3790540800
valid_periods_df[is.na(valid_periods_df)] <- as.Date('2090-02-12')
valid_periods_df$breakpoint_unix <- as.numeric(as.POSIXct(valid_periods_df$breakpoint_date))

# Save data.frame
#write.csv(valid_periods_df, "C:path/to/file.csv")

# Show data.frame (table)
valid_periods_df

#### 3.4. CREA UNA SECUENCIA DIARIA CON LOS PERIODOS DE ESTUDIO DE CADA JAULA ####

# Create daily sequence from entry_date to breakpoint_date for each JAL
seq_dates_byjal <- list()
c <- 0 #counter
for(i in sort(unique(valid_periods_df$jal_number))){
  c <- c+1
  df <- valid_periods_df[valid_periods_df$jal_number == i,]
  seq_dates_byjal[[c]] <- seq(as.Date(df$entry_date), as.Date(df$breakpoint_date), by="day")
}

# Display the order of the list 'seq_dates_byjal'
print("Orden de las jaulas para la indexacci?n: ")
jal_order_seq_dates <- sort(unique(valid_periods_df$jal_number))
for(i in jal_order_seq_dates){print(paste0("Jaula ", i))}



#### 4.1. CALCULA EL NUMERO DE INDIVIDUOS QUE SALEN DE CADA JAULA CADA DIA DEL PERIODO DE ESTUDIO ####

# Get vector of subtract by JAL
subtract_byjal <- list()
for(j in 1:length(jal_order_seq_dates)){
  df_extracciones <- extracciones[extracciones$Jaula == jal_order_seq_dates[[j]], ]
  df_movimientos <- traspasos_internos[traspasos_internos$Jaula_origen == jal_order_seq_dates[[j]], ]
  dates <- as.POSIXct(seq_dates_byjal[[j]])
  #print(paste0("Jaula ", jal_order_seq_dates[[j]]))
  #print(paste0("Fechas ", dates))
  subtract <- c()
  for(i in 1:(length(dates)-1)){
    d <- dates[[i+1]]
    df_ex <- df_extracciones[df_extracciones$Fecha_sacrificio == d, ]
    sum_ex <- length(df_ex$Matricula)
    df_mo <- df_movimientos[df_movimientos$Fecha_de_realizacion == as.numeric(d), ]
    sum_mo <- sum(df_mo$Piezas)
    sum <- sum_mo + sum_ex
    subtract <- append(subtract, sum)
  }
  subtract_byjal[[j]] <- subtract
}

# Correct length of sustract_byjal
subtract_eq_len_byjal <- list()
for(i in 1:length(subtract_byjal)){
  subtract_eq_len_byjal[[i]] <- c(0, subtract_byjal[[i]])
}

# List containing data.frames of date -vs- sustracts for each JAL
# jal_order_seq_dates complements this information
df_subtrtact_dates_byjal <- list()
for(i in 1:length(seq_dates_byjal)){
  df <- data.frame(
    dates=seq_dates_byjal[[1]],
    subtr=subtract_eq_len_byjal[[1]]
  )
  df_subtrtact_dates_byjal[[i]] <- df
}

# Manual verification from the plot and the internal transfer table
# Plot extractions in the first 365 days
if(FALSE){
  par(mfrow=c(3,4))
  for(p in 1:length(subtract_byjal)){
    plot(subtract_byjal[[p]][1:365])
  }
}

#### 4.2. CALCULA EL NUMERO DE INDIVIDUOS DURANTE CADA DIA DEL PERIODO DE ESTUDIO ####

# Create a vector with the number of individuals at the entry of each cage since 
# mediciones_aggregated$Numero_UPV
ind_entry_byjal <- mediciones_aggregated$Numero_UPV

# List containing a set of vectors with the number of individuals at each day of the valid period.
# Each element of the list is the individual evolution of each valid jal sorted as: jal_order_seq_dates
evol_ind_byjal <- list()
for(j in 1:length(ind_entry_byjal)){
  ind <- ind_entry_byjal[[j]]  
  evol_ind <- c()
  for(i in 1:(length(seq_dates_byjal[[j]]))){
    ind <- ind-subtract_eq_len_byjal[[j]][[i]]
    evol_ind <- append(evol_ind, ind)
  }
  #print(paste0("JAL", jal_order_seq_dates[[j]], " maximum: ", max(evol_ind), "  minimum: ", min(evol_ind)))
  evol_ind_byjal[[j]] <- evol_ind
}

# Plot individuals evolution at each JAL for one year (365 days)
days <- 730
jn <- length(jal_order_seq_dates)
par(mfrow=c(trunc((jn/4)+1), 4))
options(repr.plot.width=16, repr.plot.height=13)
for(i in 1:length(evol_ind_byjal)){
  plot(evol_ind_byjal[[i]][1:days], col="deepskyblue3", ylab="Individuos", xlab="Day index",
       main=paste0("JAL", jal_order_seq_dates[[i]]))
  lines(evol_ind_byjal[[i]], col="red", lwd=2)
}

for(i in 1:length(evol_ind_byjal)){
  print(paste0("Jaula ", jal_order_seq_dates[[i]]))
  print(unique(evol_ind_byjal[[i]]))
}

# Assigns 0 when x<15
threshold_minimum <- 15
for(i in 1:length(evol_ind_byjal)){
  evol_ind_byjal[[i]][evol_ind_byjal[[i]] < threshold_minimum] <- 0
}

# Remove 0 values
evol_ind_byjal <- lapply(evol_ind_byjal, function(x) {x[x!=0]})

for(i in 1:length(evol_ind_byjal)){
  print(paste0("Jaula ", jal_order_seq_dates[[i]]))
  print(unique(evol_ind_byjal[[i]]))
}

# Cut out the seq_dates_byjal vectors
for(i in 1:length(evol_ind_byjal)){
  seq_dates_byjal[[i]] <- seq_dates_byjal[[i]][1:length(evol_ind_byjal[[i]])]
}



#### 5.1. CALCULO DE LA CADUCIDAD E INDICE DE CALIDAD (IQ) DE CADA LOTE ####

# Remove duplicates in Lot column
pre <- nrow(fecha_congelacion)
fecha_congelacion <- fecha_congelacion[!duplicated(fecha_congelacion$Lot), ]
post <- nrow(fecha_congelacion)
print(paste0(pre-post, " duplicated rows removed."))

# Calculate expiration date (540 days)
expiration_days <- 540
fecha_congelacion$Fecha_caducidad <- fecha_congelacion$Fecha_congelacion + expiration_days # add days (when use 'Date' class)

# Indexation with ALIMENTACION DIARIA via Lot column
alimentacion_diaria <- merge(x=alimentacion_diaria, y=fecha_congelacion, all.x=TRUE, all.y=FALSE, no.dups=TRUE)

# Calculate Quality Index (IQ) as: Fecha_caducidad - Fecha_alimentacion
alimentacion_diaria$IQ <- as.numeric(alimentacion_diaria$Fecha_caducidad - as.Date(as.character(as.POSIXct(alimentacion_diaria$Fecha_alimentacion))))

head(alimentacion_diaria)

# Plot Quality Index (IQ)
par(mfrow=c(1,2))

# Plot IQ by date
if(TRUE){
  plot(alimentacion_diaria$Fecha_alimentacion, alimentacion_diaria$IQ, col="dodgerblue4", las=1, pch=16, 
       main="Indice de Calidad (IQ)", 
       xlab="Fecha de consumo", ylab="IQ [-Inf, 540]", xaxt="n")
  axis(1, alimentacion_diaria$Fecha_alimentacion, format(alimentacion_diaria$Fecha_alimentacion, "%b %Y"), 
       cex.axis = 1, col="white")
  box()
  grid()
  abline(h=0, lwd=1, lty=2) # add 0 line
  points(alimentacion_diaria$IQ, col="dodgerblue4")
  # Add expired values
  points(alimentacion_diaria[alimentacion_diaria$IQ < 0,]$Fecha_alimentacion, 
         alimentacion_diaria[alimentacion_diaria$IQ < 0,]$IQ, 
         col="red", pch=16)
  legend("bottomright", inset=.02, title="Estado del alimento",
         c("En buen estado","Caducado"), fill=c("dodgerblue4", "red"), horiz=TRUE, cex=0.8)
}

# Plot IQ by date and JAL
if(TRUE){
  plot(alimentacion_diaria$Fecha_alimentacion, alimentacion_diaria$IQ, col=factor(alimentacion_diaria$Jaula), las=1, pch=19, 
       main="Indice de Calidad (IQ)", 
       xlab="Fecha de consumo", ylab="IQ [-Inf, 540]", xaxt="n")
  axis(1, alimentacion_diaria$Fecha_alimentacion, format(alimentacion_diaria$Fecha_alimentacion, "%b %Y"), 
       cex.axis = 1, col="white")
  box()
  grid()
  abline(h=0, lwd=1, lty=2) # add 0 line
  #legend("bottomright", legend = paste("JAL", jal_order_seq_dates), inset=.02, title="Estado del alimento", fill=alimentacion_diaria$Jaula, horiz=TRUE, cex=0.5)
  legend("bottomright",
         legend = paste("JAL ", levels(factor(alimentacion_diaria$Jaula))),
         pch = 19,
         col = factor(levels(factor(alimentacion_diaria$Jaula))))
}

#### 5.2. CALCULA EL VECTOR DE ALIMENTO Y GRASA INTRODUCIDO CADA DIA EN CADA JAULA ####

# Manual verification of NA values
if(FALSE){
  for(j in jal_order_seq_dates){
    jal_df <- alimentacion_diaria[alimentacion_diaria$Jaula == j, ]
    print(head(jal_df[2:ncol(jal_df)]))
  }
}
nrow(alimentacion_diaria)

print(names(alimentacion_diaria))

# Remove rows with Jaula=NA
alimentacion_diaria <- alimentacion_diaria[!is.na(alimentacion_diaria$Jaula), ]

# Replace 0 by NA in all columns
alimentacion_diaria[alimentacion_diaria == 0] <- NA

# Replace NA values with the mean of the column
alimentacion_diaria$Grasa[is.na(alimentacion_diaria$Grasa)] <- mean(alimentacion_diaria$Grasa, na.rm = TRUE)
alimentacion_diaria$Calibre_cm[is.na(alimentacion_diaria$Calibre_cm)] <- mean(alimentacion_diaria$Calibre_cm, na.rm = TRUE)

# View dataset modification
par(mfrow=c(1,2))
options(repr.plot.width=18, repr.plot.height=7)

plot(alimentacion_diaria$Grasa, col="deepskyblue3", ylab="Grasa [%]", main="Grasa", las=1, tck=0.02)
abline(h=mean(alimentacion_diaria$Grasa), col="black", lwd=2, lty=2)
plot(alimentacion_diaria$Calibre_cm, col="deepskyblue3", ylab="Calibre [cm]", main="Calibre", las=1, tck=0.02)
abline(h=mean(alimentacion_diaria$Calibre_cm), col="black", lwd=2, lty=2)

# Replace >40 cm values with the mean of the column
if(max(alimentacion_diaria$Calibre_cm) > 40){
  alimentacion_diaria[alimentacion_diaria$Calibre_cm > 40,]$Calibre_cm <- mean(alimentacion_diaria$Calibre_cm)
}

# View dataset modification
par(mfrow=c(1,2))
options(repr.plot.width=18, repr.plot.height=7)

plot(alimentacion_diaria$Grasa, col="deepskyblue3", ylab="Grasa [%]", main="Grasa", las=1, tck=0.02)
abline(h=mean(alimentacion_diaria$Grasa), col="black", lwd=2, lty=2)
plot(alimentacion_diaria$Calibre_cm, col="deepskyblue3", ylab="Calibre [cm]", main="Calibre", las=1, tck=0.02)
abline(h=mean(alimentacion_diaria$Calibre_cm), col="black", lwd=2, lty=2)

# Sort data.frame by date (Fecha_alimentacion)
alimentacion_diaria_sorted <- alimentacion_diaria[order(alimentacion_diaria$Fecha_alimentacion),]

# View dataset modification
par(mfrow=c(1,2))
options(repr.plot.width=18, repr.plot.height=7)

plot(alimentacion_diaria_sorted$Grasa, col="deepskyblue3", ylab="Grasa [%]", main="Grasa", las=1, tck=0.02)
abline(h=mean(alimentacion_diaria_sorted$Grasa), col="black", lwd=2, lty=2)
plot(alimentacion_diaria_sorted$Calibre_cm, col="deepskyblue3", ylab="Calibre [cm]", main="Calibre", las=1, tck=0.02)
abline(h=mean(alimentacion_diaria_sorted$Calibre_cm), col="black", lwd=2, lty=2)

print(paste0("Calibre m?ximo: ", max(alimentacion_diaria$Calibre_cm)))

# Calculate fat [Kg]: Grasa_Kg = Cantidad_Kg * (Grasa_%/100)
alimentacion_diaria$Grasa_Kg <- alimentacion_diaria$Cantidad_Kg * (alimentacion_diaria$Grasa/100)

#class(seq_dates_byjal[[1]])
#class(alimentacion_diaria$Fecha_alimentacion)

# Calculate vectors of Kg alim.
alimento_diario_byjal <- list()
for(j in 1:length(jal_order_seq_dates)){
  alimento_diario <- c()
  jal_df <- alimentacion_diaria[alimentacion_diaria$Jaula == jal_order_seq_dates[[j]], ]
  dates <- seq_dates_byjal[[j]]
  for(i in 1:length(dates)){
    jal_df_date <- jal_df[jal_df$Fecha_alimentacion == as.POSIXct(dates[[i]], origin="1970-01-01"), ]
    sum_alim <- sum(jal_df_date$Cantidad_Kg)
    alimento_diario <- c(alimento_diario, sum_alim)
  }
  alimento_diario_byjal[[j]] <- alimento_diario 
}

if(FALSE){
  for(p in 1:11){
    barplot(alimento_diario_byjal[[p]][1:300], lwd=2, pch=19, col="red")
  }
}

print("Cantidades m?ximas en un solo d?a: ")
for(i in 1:length(alimento_diario_byjal)){
  print(max(alimento_diario_byjal[[i]]))
}

# Calculate vectors of Kg fat
grasa_diaria_byjal <- list()
for(j in 1:length(jal_order_seq_dates)){
  grasa_diaria <- c()
  jal_df <- alimentacion_diaria[alimentacion_diaria$Jaula == jal_order_seq_dates[[j]], ]
  dates <- seq_dates_byjal[[j]]
  for(i in 1:length(dates)){
    jal_df_date <- jal_df[jal_df$Fecha_alimentacion == as.POSIXct(dates[[i]], origin="1970-01-01"), ]
    sum_gras <- sum(jal_df_date$Grasa_Kg)
    grasa_diaria <- c(grasa_diaria, sum_gras)
  }
  grasa_diaria_byjal[[j]] <- grasa_diaria 
}

#### 5.3. CALCULA LA TASA DE ALIMENTACION / GRASA DIARIA ####

# TAD [Kg/ind]
TAD_byjal <- list()
for(i in 1:length(alimento_diario_byjal)){
  tad <- TAD_Kg_ind(indv=evol_ind_byjal[[i]], alim=alimento_diario_byjal[[i]])
  TAD_byjal[[i]] <- tad
}

# TGD [Kg/ind]
TGD_byjal <- list()
for(i in 1:length(grasa_diaria_byjal)){
  tgd <- TAD_Kg_ind(indv=evol_ind_byjal[[i]], alim=grasa_diaria_byjal[[i]])
  TGD_byjal[[i]] <- tgd
}

# 5.4. CALCULA LA TASA DE ALIMENTACION / GRASA ACUMULADA

# TAA [Kg/ind]
TAA_byjal <- list()
for(i in 1:length(alimento_diario_byjal)){
  taa <- TAA_Kg_ind(indv=evol_ind_byjal[[i]], alim=alimento_diario_byjal[[i]])
  TAA_byjal[[i]] <- taa
}

# TGA [Kg/ind]
TGA_byjal <- list()
for(i in 1:length(grasa_diaria_byjal)){
  tga <- TAA_Kg_ind(indv=evol_ind_byjal[[i]], alim=grasa_diaria_byjal[[i]])
  TGA_byjal[[i]] <- tga
}

# 5.5. CALCULA LA TASA DE ALIMENTACION / GRASA PROMEDIO

# TAP [Kg/ind*dia]
TAP_byjal <- list()
for(i in 1:length(alimento_diario_byjal)){
  tap <- TAP_Kg_ind_d(indv=evol_ind_byjal[[i]], alim=alimento_diario_byjal[[i]])
  TAP_byjal[[i]] <- tap
}

# TGP [Kg/ind*dia]
TGP_byjal <- list()
for(i in 1:length(grasa_diaria_byjal)){
  tgp <- TAP_Kg_ind_d(indv=evol_ind_byjal[[i]], alim=grasa_diaria_byjal[[i]])
  TGP_byjal[[i]] <- tgp
}


#### 5.4. CALCULA EL VECTOR DE IQn DE CADA DIA EN CADA JAULA ####

# Fill NA values of IQ with the global average IQ
global_IQ <- mean(alimentacion_diaria$IQ, na.rm = TRUE)
alimentacion_diaria$IQ[is.na(alimentacion_diaria$IQ)] <- global_IQ

# Calculate diary vectors of IQn by jal
iqn_diario_byjal <- list()
for(j in 1:length(jal_order_seq_dates)){
  df_jal <- alimentacion_diaria[alimentacion_diaria$Jaula == jal_order_seq_dates[[j]],]
  iqn_diario <- c()
  dates <- seq_dates_byjal[[j]]
  for(i in 1:length(dates)){
    df_dt <- df_jal[df_jal$Fecha_alimentacion == as.POSIXct(dates[[i]], origin="1970-01-01"),]
    kg_total <- sum(df_dt$Cantidad_Kg)
    df_dt$IQxKg <- df_dt$IQ * df_dt$Cantidad_Kg
    sum_IQxKg <- sum(df_dt$IQxKg) #### NA?
    IQn_dia <- sum_IQxKg / kg_total
    iqn_diario <- c(iqn_diario, IQn_dia)
  }
  iqn_diario_byjal[[j]] <- iqn_diario
}

# Replace NA by 0 in all vectors
for(i in 1:length(iqn_diario_byjal)){
  iqn_diario_byjal[[i]][is.na(iqn_diario_byjal[[i]])] <- 0
}

#### 5.5. CALCULA EL IQn PROMEDIO (IQnP) ####

# Calculate IQn averaged (IQnP) for each cumulated period
IQnP_byjal <- list()
for(j in 1:length(iqn_diario_byjal)){
  IQn <- iqn_diario_byjal[[j]]
  TAD <- alimento_diario_byjal[[j]] # KG alimento diario
  TAA <- cumsum(TAD) # Suma acumulada de KG de alimento diario
  
  IQn_x_TAD <- IQn * TAD
  sum_IQn_x_TAD <- cumsum(IQn_x_TAD)
  
  IQnP <- sum_IQn_x_TAD / TAA
  
  IQnP_byjal[[j]] <- IQnP
}

# Replace NA values by 0
for(i in 1:length(IQnP_byjal)){
  IQnP_byjal[[i]][is.na(IQnP_byjal[[i]])] <- 0
}

#### 5.6. CALCULA EL CALIBRE PROMEDIO DE CADA JAULA EN CADA DIA DEL PERIODO DE ESTUDIO ####

# Calibre diario para cada dia del periodo de estudio
CAP_byjal <- list()
for(j in 1:length(jal_order_seq_dates)){
  alim <- alimentacion_diaria[alimentacion_diaria$Jaula == jal_order_seq_dates[[j]],]
  Cad_x <- list()
  for(i in seq_dates_byjal[[j]]){
    d <- alim[alim$Fecha_alimentacion == i,]
    Cad_x[[i]] <- mean(d$Calibre_cm, na.rm=TRUE)
  }
  CAD <- unlist(Cad_x) # Vector de calibre diario (promedio)
  CAA <- cumsum(CAD) # Vector de calibre acumulado
  id_dia <- 1:length(CAD) # Vector id de dias
  CAP <- CAD/id_dia # Vector de calibre promedio del periodo
  CAP_byjal[[j]] <- CAP
}


#### 5.7. CALCULA LA PRESENCIA/AUSENCIA EN EL TIPO DE ESPECIE-ALIMENTO PARA CADA DIA ####

# Las especies principales consideradas son Viso, Colias, Sardina, Japonicus y Arenque

library(stringr)

# Create multi-hot encoding of occurrences
alimentacion_diaria$SP_Viso <- str_count(alimentacion_diaria$Especie, "VISO")
alimentacion_diaria$SP_Colias <- str_count(alimentacion_diaria$Especie, "COLIAS")
alimentacion_diaria$SP_Sardina <- str_count(alimentacion_diaria$Especie, "SARDINA")
alimentacion_diaria$SP_Japonicus <- str_count(alimentacion_diaria$Especie, "JAPONICUS")
alimentacion_diaria$SP_Arenque <- str_count(alimentacion_diaria$Especie, "ARENQUE")

# Especie 1: VISO
SP_Viso_byjal = list()
for(j in 1:length(jal_order_seq_dates)){
  alim = alimentacion_diaria[alimentacion_diaria$Jaula == jal_order_seq_dates[[j]],]
  SP_x = list()
  for(i in seq_dates_byjal[[j]]){
    d = alim[alim$Fecha_alimentacion == as.POSIXct(i, origin="1970-01-01"),]
    SP_x[[i]] <- sum(d$SP_Viso)
  }
  SP_x_vec <- unlist(SP_x)
  SP_x_vec[SP_x_vec >= 1] <- 1;   #SP_x_vec[SP_x_vec < 1] <- 0
  SP_Viso_byjal[[j]] <- SP_x_vec
}

# Especie 2: COLIAS
SP_Colias_byjal = list()
for(j in 1:length(jal_order_seq_dates)){
  alim = alimentacion_diaria[alimentacion_diaria$Jaula == jal_order_seq_dates[[j]],]
  SP_x = list()
  for(i in seq_dates_byjal[[j]]){
    d = alim[alim$date == i,]
    SP_x[[i]] <- sum(d$SP_Colias)
  }
  SP_x_vec <- unlist(SP_x)
  SP_x_vec[SP_x_vec >= 1] <- 1;   #SP_x_vec[SP_x_vec < 1] <- 0
  SP_Colias_byjal[[j]] <- SP_x_vec
}

# Especie 3: SARDINA
SP_Sardina_byjal = list()
for(j in 1:length(jal_order_seq_dates)){
  alim = alimentacion_diaria[alimentacion_diaria$Jaula == jal_order_seq_dates[[j]],]
  SP_x = list()
  for(i in seq_dates_byjal[[j]]){
    d = alim[alim$date == i,]
    SP_x[[i]] <- sum(d$SP_Sardina)
  }
  SP_x_vec <- unlist(SP_x)
  SP_x_vec[SP_x_vec >= 1] <- 1;   #SP_x_vec[SP_x_vec < 1] <- 0
  SP_Sardina_byjal[[j]] <- SP_x_vec
}

# Especie 4: JAPONICUS
SP_Japonicus_byjal = list()
for(j in 1:length(jal_order_seq_dates)){
  alim = alimentacion_diaria[alimentacion_diaria$Jaula == jal_order_seq_dates[[j]],]
  SP_x = list()
  for(i in seq_dates_byjal[[j]]){
    d = alim[alim$date == i,]
    SP_x[[i]] <- sum(d$SP_Japonicus)
  }
  SP_x_vec <- unlist(SP_x)
  SP_x_vec[SP_x_vec >= 1] <- 1;   #SP_x_vec[SP_x_vec < 1] <- 0
  SP_Japonicus_byjal[[j]] <- SP_x_vec
}

# Especie 5: ARENQUE
SP_Arenque_byjal = list()
for(j in 1:length(jal_order_seq_dates)){
  alim = alimentacion_diaria[alimentacion_diaria$Jaula == jal_order_seq_dates[[j]],]
  SP_x = list()
  for(i in seq_dates_byjal[[j]]){
    d = alim[alim$date == i,]
    SP_x[[i]] <- sum(d$SP_Arenque)
  }
  SP_x_vec <- unlist(SP_x)
  SP_x_vec[SP_x_vec >= 1] <- 1;   #SP_x_vec[SP_x_vec < 1] <- 0
  SP_Arenque_byjal[[j]] <- SP_x_vec
}


#### 5.8. CALCULA LA PRESENCIA PROMEDIO DE CADA ESPECIE-ALIMENTO PARA CADA DIA DEL PERIODO DE ESTUDIO ####

# Nota: Este punto se podría mejorar ya que se debería asumir que para un lote (L) con 2 especies se aporta 
# el 50% de cada una, para un lote con 3 especies se aporta el 33$ de cada una y después ponderar la especie
# con los Kg del propio lote para obtener una estimación en Kg de la especie. Sin embargo por simplicidad
# se realiza un conteo de los días que aparece cada especie-alimento. Por tanto, si una especie-alimento
# aparece al menos 1 vez todos los días tendrá un valor 1 para ese periodo. Si la especie aparece la mitad
# de los días tendrá un valor 0.5.

# Ocurrencia Promecio de la especie 1: Viso
OCP_Viso_byjal = list()
for(j in 1:length(jal_order_seq_dates)){
  Xj <- SP_Viso_byjal[[j]]
  i <- 1:length(Xj)
  Xcum <- cumsum(Xj)
  Xocp <- Xcum/i
  OCP_Viso_byjal[[j]]
}

for(j in 1:length(jal_order_seq_dates)){
print(j)
}

# Ocurrencia Promecio de la especie 2: Colias
OCP_Colias_byjal = list()
for(j in 1:length(jal_order_seq_dates)){
  Xj <- SP_Colias_byjal[[j]]
  i <- 1:length(Xj)
  Xcum <- cumsum(Xj)
  Xocp <- Xcum/i
  OCP_Colias_byjal[[j]]
}

# Ocurrencia Promecio de la especie 3: Sardina
OCP_Sardina_byjal = list()
for(j in 1:length(jal_order_seq_dates)){
  Xj <- SP_Sardina_byjal[[j]]
  i <- 1:length(Xj)
  Xcum <- cumsum(Xj)
  Xocp <- Xcum/i
  OCP_Sardina_byjal[[j]]
}

# Ocurrencia Promecio de la especie 4: Japonicus
OCP_Japonicus_byjal = list()
for(j in 1:length(jal_order_seq_dates)){
  Xj <- SP_Japonicus_byjal[[j]]
  i <- 1:length(Xj)
  Xcum <- cumsum(Xj)
  Xocp <- Xcum/i
  OCP_Japonicus_byjal[[j]]
}

# Ocurrencia Promecio de la especie 5: Arenque
OCP_Arenque_byjal = list()
for(j in 1:length(jal_order_seq_dates)){
  Xj <- SP_Arenque_byjal[[j]]
  i <- 1:length(Xj)
  Xcum <- cumsum(Xj)
  Xocp <- Xcum/i
  OCP_Arenque_byjal[[j]]
}



#### 6.1. TABLA DE VARIABLES DE INTERES ####

# Variables:
  
  # Jaula: 'jal_order_seq_dates' vector con el número de jaula. Sirve para indexar las
  #         siguientes variables:
  
  # Fecha: 'seq_dates_byjal' secuencia de fechas que forma el periodo de estudio
  # TAP: 'TAP_byjal' tasa de alimentacion promedio en Kg/ind/dia
  # TGP: 'TGP_byjal' tasa de grasa promedio en Kg/ind/dia
  # IQnP: 'IQnP_byjal' indice de calidad promedio normalizado
  # CAP: 'CAP_byjal' calibre promedio
  # OCP_Viso: 'OCP_Viso' ocurrencia de Viso en la alimentación en dias/dias
  # OCP_Colias: 'OCP_Colias' ocurrencia de Colias en la alimentación en dias/dias
  # OCP_Sardina: 'OCP_Sardina' ocurrencia de Sardina en la alimentación en dias/dias
  # OCP_Japonicus: 'OCP_Japonicus' ocurrencia de Japonicus en la alimentación en dias/dias
  # OCP_Arenque: 'OCP_Arenque' ocurrencia de Arenque en la alimentación en dias/dias

# Crea un data.frame con las variables de estudio para cada jaula. Cada columna contiene una
# de las variables de estudio.
df_variables_names <- as.character(jal_order_seq_dates)
for(i in 1:length(df_variables_names)){
  assign(paste0("DF_JAL_", df_variables_names[[i]]), data.frame(
    Fecha = seq_dates_byjal[[i]],
    Tiempo_crianza = 1:length(seq_dates_byjal[[i]]),
    TAP = TAP_byjal[[i]],
    TGP = TGP_byjal[[i]],
    IQnP = IQnP_byjal[[i]],
    CAP = CAP_byjal[[i]],
    #OCP_Viso = OCP_Viso_byjal[[i]],
    #OCP_Colias = OCP_Colias_byjal[[i]],
    #OCP_Sardina = OCP_Sardina_byjal[[i]],
    #OCP_Japonicus = OCP_Japonicus_byjal[[i]],
    #OCP_Arenque = OCP_Arenque_byjal[[i]],
    Jaula_crianza = i))
}





