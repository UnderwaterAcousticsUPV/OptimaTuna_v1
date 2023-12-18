
# ---------- OptimaTUNA_analisis_v1.R ----------

# Titulo: Analisis de datos del proyecto OptimaTUNA. Version v4.
# Autor: Sergio Morell-Monzo
# Fecha actualizacion: 12/07/2023
# Test: OK
# Nota: Plots pre-analisis campa√±a 2021.

library(psych)

##### 1. LECTURA DE DATOS #####

# Lectura de datos
JAL2 <- readxl::read_excel("C:/Users/sergi/Desktop/OptimaTUNA_MAC/resultados/2021/DFSAMP_2.xlsx")
JAL3 <- readxl::read_excel("C:/Users/sergi/Desktop/OptimaTUNA_MAC/resultados/2021/DFSAMP_3.xlsx")
JAL4 <- readxl::read_excel("C:/Users/sergi/Desktop/OptimaTUNA_MAC/resultados/2021/DFSAMP_4.xlsx")
JAL5 <- readxl::read_excel("C:/Users/sergi/Desktop/OptimaTUNA_MAC/resultados/2021/DFSAMP_5.xlsx")
JAL6 <- readxl::read_excel("C:/Users/sergi/Desktop/OptimaTUNA_MAC/resultados/2021/DFSAMP_6.xlsx")
JAL7 <- readxl::read_excel("C:/Users/sergi/Desktop/OptimaTUNA_MAC/resultados/2021/DFSAMP_7.xlsx")
JAL11 <- readxl::read_excel("C:/Users/sergi/Desktop/OptimaTUNA_MAC/resultados/2021/DFSAMP_11.xlsx")
JAL14 <- readxl::read_excel("C:/Users/sergi/Desktop/OptimaTUNA_MAC/resultados/2021/DFSAMP_14.xlsx")
JAL18 <- readxl::read_excel("C:/Users/sergi/Desktop/OptimaTUNA_MAC/resultados/2021/DFSAMP_18.xlsx")
JAL19 <- readxl::read_excel("C:/Users/sergi/Desktop/OptimaTUNA_MAC/resultados/2021/DFSAMP_19.xlsx")
JAL20 <- readxl::read_excel("C:/Users/sergi/Desktop/OptimaTUNA_MAC/resultados/2021/DFSAMP_20.xlsx")

# Tablas unidas
SAMP <- rbind(JAL2, JAL3, JAL4, JAL5, JAL6, JAL7, JAL11, JAL14, JAL18, JAL19, JAL20)

##### 2.a PLOTS PRE-ANALISIS: VIOLIN-PLOT TAD/TGP POR JAULA ####
##### 2.b PLOTS PRE-ANALISIS: VIOLIN-PLOT OTRAS VARIABLES POR JAULA ####
##### 2.c PLOTS PRE-ANALISIS: PESP Y PESO ENTERO VS TIEMPO CRIANZA #####

SAMP <- na.omit(SAMP)

# PESP_L: PESP / L [Kg/cm]
SAMP$PESP_L <- SAMP$PESP / SAMP$Longitud
SAMP$PESP_L2 <- SAMP$PESP / SAMP$Longitud^2
SAMP$PESP_L3 <- SAMP$PESP / SAMP$Longitud^3

plot(SAMP$Tiempo_crianza, SAMP$PESP_L3, 
     xlab="Tiempo de crianza [dias]", ylab="Peso engordado [Kg]",
     main="EvoluciÛn del peso engordado",
     col="blue", las=1, pch=3)

# Plot: Tiempo Crianza vs Peso Engordado
plot(SAMP$Tiempo_crianza, SAMP$PESP, 
     xlab="Tiempo de crianza [dias]", ylab="Peso engordado [Kg]",
     main="EvoluciÛn del peso engordado",
     col="blue", las=1, pch=3)

# Plot: Tiempo Crianza vs Peso Entero
plot(SAMP$Tiempo_crianza, SAMP$Peso_entero_Kg, 
     xlab="Tiempo de crianza [dias]", ylab="Peso entero [Kg]",
     main="EvoluciÛn del peso entero",
     col="blue", las=1, pch=3)

# Agrega tablas seg√∫n la variable PESP_L
JAL2_PESPagg <- aggregate(PESP_L ~ Tiempo_crianza, JAL2, mean)
JAL3_PESPagg <- aggregate(PESP_L ~ Tiempo_crianza, JAL3, mean)
JAL4_PESPagg <- aggregate(PESP_L ~ Tiempo_crianza, JAL4, mean)
#JAL5_PESPagg <- aggregate(PESP_L ~ Tiempo_crianza, JAL5, mean)
JAL6_PESPagg <- aggregate(PESP_L ~ Tiempo_crianza, JAL6, mean)
JAL7_PESPagg <- aggregate(PESP_L ~ Tiempo_crianza, JAL7, mean)
JAL11_PESPagg <- aggregate(PESP_L ~ Tiempo_crianza, JAL11, mean)
JAL14_PESPagg <- aggregate(PESP_L ~ Tiempo_crianza, JAL14, mean)
JAL18_PESPagg <- aggregate(PESP_L ~ Tiempo_crianza, JAL18, mean)
JAL19_PESPagg <- aggregate(PESP_L ~ Tiempo_crianza, JAL19, mean)
#JAL20_PESPagg <- aggregate(PESP_L ~ Tiempo_crianza, JAL20, mean)

# Agrega tablas seg√∫n la variable PESP
JAL2_PESPagg <- aggregate(PESP ~ Tiempo_crianza, JAL2, mean)
JAL3_PESPagg <- aggregate(PESP ~ Tiempo_crianza, JAL3, mean)
JAL4_PESPagg <- aggregate(PESP ~ Tiempo_crianza, JAL4, mean)
#JAL5_PESPagg <- aggregate(PESP ~ Tiempo_crianza, JAL5, mean)
JAL6_PESPagg <- aggregate(PESP ~ Tiempo_crianza, JAL6, mean)
JAL7_PESPagg <- aggregate(PESP ~ Tiempo_crianza, JAL7, mean)
JAL11_PESPagg <- aggregate(PESP ~ Tiempo_crianza, JAL11, mean)
JAL14_PESPagg <- aggregate(PESP ~ Tiempo_crianza, JAL14, mean)
JAL18_PESPagg <- aggregate(PESP ~ Tiempo_crianza, JAL18, mean)
JAL19_PESPagg <- aggregate(PESP ~ Tiempo_crianza, JAL19, mean)
#JAL20_PESPagg <- aggregate(PESP ~ Tiempo_crianza, JAL20, mean)

DFSAM_PESPAgg <- rbind(JAL2_PESPagg, JAL3_PESPagg, JAL4_PESPagg, JAL6_PESPagg, JAL7_PESPagg, JAL11_PESPagg, JAL14_PESPagg, JAL18_PESPagg, JAL19_PESPagg)

# Plot: Tiempo Crianza vs Peso Engordado Sobre el Promedio
plot(JAL2_PESPagg$Tiempo_crianza, JAL2_PESPagg$PESP, col="blue", ylim=c(-100,300), xlim=c(0,300),
     xlab="Tiempo de crianza [dias]", ylab="Peso engordado [Kg]",
     main="Peso engordado (sobre el promedio de la poblaciÛn)")
abline(v=150, lwd=0.5, col="gray")
abline(v=170, col="red", lwd=2)
abline(v=190, lwd=0.5, col="gray")
points(JAL3_PESPagg$Tiempo_crianza, JAL3_PESPagg$PESP, col="green")
points(JAL4_PESPagg$Tiempo_crianza, JAL4_PESPagg$PESP, col="red")
#points(JAL5_PESPagg$Tiempo_crianza, JAL5_PESPagg$PESP, col="black")
points(JAL6_PESPagg$Tiempo_crianza, JAL6_PESPagg$PESP, col="yellow")
points(JAL7_PESPagg$Tiempo_crianza, JAL7_PESPagg$PESP, col="black", pch=19)
points(JAL11_PESPagg$Tiempo_crianza, JAL11_PESPagg$PESP, col="orange")
points(JAL14_PESPagg$Tiempo_crianza, JAL14_PESPagg$PESP, col="darkslateblue")
points(JAL18_PESPagg$Tiempo_crianza, JAL18_PESPagg$PESP, col="deeppink1")
points(JAL19_PESPagg$Tiempo_crianza, JAL19_PESPagg$PESP, col="olivedrab")
#points(JAL20_PESPagg$Tiempo_crianza, JAL20_PESPagg$PESP, col="saddlebrown")

mod <- lm(DFSAM_PESPagg$PESP ~ DFSAM_PESPagg$Tiempo_crianza)
abline(mod)

# Agrega las tambas segun Peso_entero_Kg

JAL2_PEagg <- aggregate(Peso_entero_Kg ~ Tiempo_crianza, JAL2, mean)
JAL3_PEagg <- aggregate(Peso_entero_Kg ~ Tiempo_crianza, JAL3, mean)
JAL4_PEagg <- aggregate(Peso_entero_Kg ~ Tiempo_crianza, JAL4, mean)
#JAL5_PEagg <- aggregate(Peso_entero_Kg ~ Tiempo_crianza, JAL5, mean)
JAL6_PEagg <- aggregate(Peso_entero_Kg ~ Tiempo_crianza, JAL6, mean)
JAL7_PEagg <- aggregate(Peso_entero_Kg ~ Tiempo_crianza, JAL7, mean)
JAL11_PEagg <- aggregate(Peso_entero_Kg ~ Tiempo_crianza, JAL11, mean)
JAL14_PEagg <- aggregate(Peso_entero_Kg ~ Tiempo_crianza, JAL14, mean)
JAL18_PEagg <- aggregate(Peso_entero_Kg ~ Tiempo_crianza, JAL18, mean)
JAL19_PEagg <- aggregate(Peso_entero_Kg ~ Tiempo_crianza, JAL19, mean)
#JAL20_PEagg <- aggregate(Peso_entero_Kg ~ Tiempo_crianza, JAL20, mean)

DFSAM_PEagg <- rbind(JAL2_PEagg, JAL3_PEagg, JAL4_PEagg, JAL6_PEagg, JAL7_PEagg, JAL11_PEagg, JAL14_PEagg, JAL18_PEagg, JAL19_PEagg)

# Plot: Tiempo Crianza vs Peso Entero
plot(JAL2_PEagg$Tiempo_crianza, JAL2_PEagg$Peso_entero_Kg, col="blue", ylim=c(0,500), xlim=c(0,400),
     xlab="Tiempo de crianza [dias]", ylab="Peso entero [Kg]")
points(JAL3_PEagg$Tiempo_crianza, JAL3_PEagg$Peso_entero_Kg, col="green")
points(JAL4_PEagg$Tiempo_crianza, JAL4_PEagg$Peso_entero_Kg, col="red")
#points(JAL5_PEagg$Tiempo_crianza, JAL5_PEagg$Peso_entero_Kg, col="black")
points(JAL6_PEagg$Tiempo_crianza, JAL6_PEagg$Peso_entero_Kg, col="yellow")
points(JAL7_PEagg$Tiempo_crianza, JAL7_PEagg$Peso_entero_Kg, col="purple")
points(JAL11_PEagg$Tiempo_crianza, JAL11_PEagg$Peso_entero_Kg, col="orange")
points(JAL14_PEagg$Tiempo_crianza, JAL14_PEagg$Peso_entero_Kg, col="darkslateblue")
points(JAL18_PEagg$Tiempo_crianza, JAL18_PEagg$Peso_entero_Kg, col="deeppink1")
points(JAL19_PEagg$Tiempo_crianza, JAL19_PEagg$Peso_entero_Kg, col="olivedrab")
#points(JAL20_PEagg$Tiempo_crianza, JAL20_PEagg$Peso_entero_Kg, col="saddlebrown")

mod <- lm(DFSAM_PEagg$Peso_entero_Kg ~ DFSAM_PEagg$Tiempo_crianza)
abline(mod)


##### 3. MATRIZ DE CORRELACION ENTRE VARIABLES #####

DFCORR <- SAMP[,15:21]

# Plot: matriz de correlaciones
pairs.panels(DFCORR)

##### 4. TASAS DE CONVERSION (KG-KG) DE CADA JAULA (EFICIENCIA) ####

# En este apartado calculamos la Tasa de Convers√≥n Kg alimento - Kg engordado de cada jaula.
# Sin embargo, hay que tener en cuenta que para calcular el peso engordado de cada individuo
# se utiliza como peso inicial el peso promedio de la jaula. Tambi√©n hay que tener en cuenta
# el sesgo en los sacrificios (se extraen los individuos mas grandes) y esto provoca que las
# Tasas de Conversion de estas muestras puedan estar sobreestimadas.

names(SAMP)

# Se calcula la tasa de conversi√≥n promedio de cada jaula
mean(JAL2$PESP)/mean(JAL2$CAL)*100
mean(JAL3$PESP, na.rm = TRUE)/mean(JAL3$CAL, na.rm = TRUE)*100
mean(JAL4$PESP, na.rm = TRUE)/mean(JAL4$CAL, na.rm = TRUE)*100
#mean(JAL5$PESP, na.rm = TRUE)/mean(JAL5$CAL, na.rm = TRUE)*100
mean(JAL6$PESP)/mean(JAL6$CAL)*100
mean(JAL7$PESP, na.rm = TRUE)/mean(JAL7$CAL, na.rm = TRUE)*100
mean(JAL11$PESP)/mean(JAL11$CAL)*100
mean(JAL14$PESP, na.rm = TRUE)/mean(JAL14$CAL, na.rm = TRUE)*100
mean(JAL18$PESP, na.rm = TRUE)/mean(JAL18$CAL, na.rm = TRUE)*100
mean(JAL19$PESP, na.rm = TRUE)/mean(JAL19$CAL, na.rm = TRUE)*100
mean(JAL20$PESP, na.rm = TRUE)/mean(JAL20$CAL, na.rm = TRUE)*100

## Tasa de Conversi√≥n / dias

mean(JAL2$PESP)/mean(JAL2$CAL)*100/mean(JAL2$Tiempo_crianza)
mean(JAL3$PESP, na.rm = TRUE)/mean(JAL3$CAL, na.rm = TRUE)*100/mean(JAL3$Tiempo_crianza, na.rm = TRUE)
mean(JAL4$PESP, na.rm = TRUE)/mean(JAL4$CAL, na.rm = TRUE)*100/mean(JAL4$Tiempo_crianza, na.rm = TRUE)
#mean(JAL5$PESP, na.rm = TRUE)/mean(JAL5$CAL, na.rm = TRUE)*100
mean(JAL6$PESP)/mean(JAL6$CAL)*100/mean(JAL6$Tiempo_crianza)
mean(JAL7$PESP, na.rm = TRUE)/mean(JAL7$CAL, na.rm = TRUE)*100/mean(JAL7$Tiempo_crianza, na.rm = TRUE)
mean(JAL11$PESP)/mean(JAL11$CAL)*100/mean(JAL11$Tiempo_crianza)
mean(JAL14$PESP, na.rm = TRUE)/mean(JAL14$CAL, na.rm = TRUE)*100/mean(JAL14$Tiempo_crianza, na.rm = TRUE)
mean(JAL18$PESP, na.rm = TRUE)/mean(JAL18$CAL, na.rm = TRUE)*100/mean(JAL18$Tiempo_crianza, na.rm = TRUE)
mean(JAL19$PESP, na.rm = TRUE)/mean(JAL19$CAL, na.rm = TRUE)*100/mean(JAL19$Tiempo_crianza, na.rm = TRUE)


### relaciÛn tasa conv Kg-Kg y Kg-Kg/dia

f2 <- mean(JAL2$PESP)/mean(JAL2$CAL)*100
f3 <- mean(JAL3$PESP, na.rm = TRUE)/mean(JAL3$CAL, na.rm = TRUE)*100
f4 <- mean(JAL4$PESP, na.rm = TRUE)/mean(JAL4$CAL, na.rm = TRUE)*100
f6 <- mean(JAL6$PESP)/mean(JAL6$CAL)*100
f7 <- mean(JAL7$PESP, na.rm = TRUE)/mean(JAL7$CAL, na.rm = TRUE)*100
f11 <- mean(JAL11$PESP)/mean(JAL11$CAL)*100
f14 <- mean(JAL14$PESP, na.rm = TRUE)/mean(JAL14$CAL, na.rm = TRUE)*100
f18 <- mean(JAL18$PESP, na.rm = TRUE)/mean(JAL18$CAL, na.rm = TRUE)*100
f19 <- mean(JAL19$PESP, na.rm = TRUE)/mean(JAL19$CAL, na.rm = TRUE)*100

t2<-mean(JAL2$PESP)/mean(JAL2$CAL)*100/mean(JAL2$Tiempo_crianza)
t3<-mean(JAL3$PESP, na.rm = TRUE)/mean(JAL3$CAL, na.rm = TRUE)*100/mean(JAL3$Tiempo_crianza, na.rm = TRUE)
t4<-mean(JAL4$PESP, na.rm = TRUE)/mean(JAL4$CAL, na.rm = TRUE)*100/mean(JAL4$Tiempo_crianza, na.rm = TRUE)
t6<-mean(JAL6$PESP)/mean(JAL6$CAL)*100/mean(JAL6$Tiempo_crianza)
t7<-mean(JAL7$PESP, na.rm = TRUE)/mean(JAL7$CAL, na.rm = TRUE)*100/mean(JAL7$Tiempo_crianza, na.rm = TRUE)
t11<-mean(JAL11$PESP)/mean(JAL11$CAL)*100/mean(JAL11$Tiempo_crianza)
t14<-mean(JAL14$PESP, na.rm = TRUE)/mean(JAL14$CAL, na.rm = TRUE)*100/mean(JAL14$Tiempo_crianza, na.rm = TRUE)
t18<-mean(JAL18$PESP, na.rm = TRUE)/mean(JAL18$CAL, na.rm = TRUE)*100/mean(JAL18$Tiempo_crianza, na.rm = TRUE)
t19<-mean(JAL19$PESP, na.rm = TRUE)/mean(JAL19$CAL, na.rm = TRUE)*100/mean(JAL19$Tiempo_crianza, na.rm = TRUE)

d2 <- mean(JAL2$Tiempo_crianza)
d3 <- mean(JAL3$Tiempo_crianza)
d4 <- mean(JAL4$Tiempo_crianza)
d6 <- mean(JAL6$Tiempo_crianza)
d7 <- mean(JAL7$Tiempo_crianza)
d11 <- mean(JAL11$Tiempo_crianza)
d14 <- mean(JAL14$Tiempo_crianza)
d18 <- mean(JAL18$Tiempo_crianza)
d19 <- mean(JAL19$Tiempo_crianza)

f <- c(f2, f3, f4, f6, f7, f11, f14, f18, f19)
t <- c(t2, t3, t4, t6, t7, t11, t14, t18, t19)
d <- c(d2, d3, d4, d6, d7, d11, d14, d18, d19)
l <- lm(t ~ f)

plot(f, t)
abline(l)

######### tasa de conv vs dias crianza

df <- rbind(JAL2, JAL3, JAL4, JAL6, JAL7, JAL11, JAL14, JAL18, JAL19, JAL20)
df$tasaconv_ <- df$PESP/df$CAL

plot(df$Tiempo_crianza, df$tasaconv_, col="blue")

##### 5. NORMALIZAR PESP EN FUNCION DE LA LONGITUD L, L2, L3 ####

x <- 1

JAL2 <- na.omit(JAL2)
JAL2$PESP_L <- JAL2$PESP / JAL2$Longitud^x
plot(JAL2$Tiempo_crianza, JAL2$PESP_L)

JAL3 <- na.omit(JAL3)
JAL3$PESP_L <- JAL3$PESP / JAL3$Longitud^x
plot(JAL3$Tiempo_crianza, JAL3$PESP_L)

JAL4 <- na.omit(JAL4)
JAL4$PESP_L <- JAL4$PESP / JAL4$Longitud^x
plot(JAL4$Tiempo_crianza, JAL4$PESP_L)

JAL5 <- na.omit(JAL5)
JAL5$PESP_L <- JAL5$PESP / JAL5$Longitud^x
plot(JAL5$Tiempo_crianza, JAL5$PESP_L)

JAL6 <- na.omit(JAL6)
JAL6$PESP_L <- JAL6$PESP / JAL6$Longitud^x
plot(JAL6$Tiempo_crianza, JAL6$PESP_L)

JAL7 <- na.omit(JAL7)
JAL7$PESP_L <- JAL7$PESP / JAL7$Longitud^x
plot(JAL7$Tiempo_crianza, JAL7$PESP_L)

JAL11 <- na.omit(JAL11)
JAL11$PESP_L <- JAL11$PESP / JAL11$Longitud^x
plot(JAL11$Tiempo_crianza, JAL11$PESP_L)

JAL14 <- na.omit(JAL14)
JAL14$PESP_L <- JAL14$PESP / JAL14$Longitud^x
plot(JAL14$Tiempo_crianza, JAL14$PESP_L)

JAL18 <- na.omit(JAL18)
JAL18$PESP_L <- JAL18$PESP / JAL18$Longitud^x
plot(JAL18$Tiempo_crianza, JAL18$PESP_L)

JAL19 <- na.omit(JAL19)
JAL19$PESP_L <- JAL19$PESP / JAL19$Longitud^x
plot(JAL19$Tiempo_crianza, JAL19$PESP_L)

JAL20 <- na.omit(JAL20)
JAL19$PESP_L <- JAL20$PESP / JAL20$Longitud^x
plot(JAL20$Tiempo_crianza, JAL20$PESP_L)

# Agrega tablas seg√∫n la variable PESP_L
JAL2_PESPagg <- aggregate(PESP_L ~ Tiempo_crianza, JAL2, mean)
JAL3_PESPagg <- aggregate(PESP_L ~ Tiempo_crianza, JAL3, mean)
JAL4_PESPagg <- aggregate(PESP_L ~ Tiempo_crianza, JAL4, mean)
#JAL5_PESPagg <- aggregate(PESP_L ~ Tiempo_crianza, JAL5, mean)
JAL6_PESPagg <- aggregate(PESP_L ~ Tiempo_crianza, JAL6, mean)
JAL7_PESPagg <- aggregate(PESP_L ~ Tiempo_crianza, JAL7, mean)
JAL11_PESPagg <- aggregate(PESP_L ~ Tiempo_crianza, JAL11, mean)
JAL14_PESPagg <- aggregate(PESP_L ~ Tiempo_crianza, JAL14, mean)
JAL18_PESPagg <- aggregate(PESP_L ~ Tiempo_crianza, JAL18, mean)
JAL19_PESPagg <- aggregate(PESP_L ~ Tiempo_crianza, JAL19, mean)
#JAL20_PESPagg <- aggregate(PESP_L ~ Tiempo_crianza, JAL20, mean)

# Plot: Tiempo Crianza vs Peso Engordado Sobre el Promedio
plot(JAL2_PESPagg$Tiempo_crianza, JAL2_PESPagg$PESP_L, col="blue", ylim=c(-0.1,0.01), xlim=c(0, 350),
     xlab="Tiempo de crianza [dias]", ylab="Peso engordado / Longitud [Kg/cm]")
points(JAL3_PESPagg$Tiempo_crianza, JAL3_PESPagg$PESP_L, col="green")
points(JAL4_PESPagg$Tiempo_crianza, JAL4_PESPagg$PESP_L, col="red")
#points(JAL5_PESPagg$Tiempo_crianza, JAL5_PESPagg$PESP_L, col="black")
points(JAL6_PESPagg$Tiempo_crianza, JAL6_PESPagg$PESP_L, col="yellow")
points(JAL7_PESPagg$Tiempo_crianza, JAL7_PESPagg$PESP_L, col="purple")
points(JAL11_PESPagg$Tiempo_crianza, JAL11_PESPagg$PESP_L, col="orange")
points(JAL14_PESPagg$Tiempo_crianza, JAL14_PESPagg$PESP_L, col="darkslateblue")
points(JAL18_PESPagg$Tiempo_crianza, JAL18_PESPagg$PESP_L, col="deeppink1")
points(JAL19_PESPagg$Tiempo_crianza, JAL19_PESPagg$PESP_L, col="olivedrab")
#points(JAL20_PESPagg$Tiempo_crianza, JAL20_PESPagg$PESP_L, col="saddlebrown")

DFSAM_PEagg <- rbind(JAL2_PESPagg, JAL3_PESPagg, JAL4_PESPagg, JAL6_PESPagg, JAL7_PESPagg, JAL11_PESPagg, JAL14_PESPagg, JAL18_PESPagg, JAL19_PESPagg)

mod <- lm(DFSAM_PEagg$PESP_L ~ DFSAM_PEagg$Tiempo_crianza)
abline(mod)

##### 6. MUESTRAS JAULA 7 DESPUES DE 170 DIAS VS OTRAS JAULAS
