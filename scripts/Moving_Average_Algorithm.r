
# Moving average

i=6 # selecciona el indice de la jaula el jal_order_seq_dates
v <- alimento_diario_byjal[[i]]
v0 <- v # original vector
v1 <- v; v1[v==0] <- NA # vector without 0s

plot(v0, las=1, xlab="Fecha alimentacion", ylab="Alimento [Kg]", col="gray", tck=.03)
points(v1, pch=19)

m1 <- moving_average(vec=v1,
                     runn_mean=7,
                     na.rm=TRUE,
                     exclude_central_value=TRUE)

lines(m1, col="red", lwd=2)

m2 <- moving_average(vec=v1,
                     runn_mean=7,
                     na.rm=TRUE,
                     exclude_central_value=FALSE)

lines(m2, col="blue", lwd=3)

legend("topright", legend=c("Media movil 7", "Exclusión valor central", "Valores 0 (excluidos)"),
       col=c("blue", "red", "gray"), lty=c(1,1), lwd=2, box.col = "white",
       pch=1, merge=FALSE)
box()


#Algoritmo

plot(m1,m2, xlab="Media sin permutación", ylab="Media con permutación")
abline(c(1,100000), c(1,100000), col="red", lwd=2)

# Diferencia entre m2 y m1
dif <- m2-m1
plot(dif/m1*100, ylab="Porcentaje de diferencia al permutar", ylim=c(-200, 200))
abline(h=50, lwd=3, lty=5, col="blue")

# Porcentaje de diferencia respecto a m1 (base: sin suavizar)
per <- dif/m1



