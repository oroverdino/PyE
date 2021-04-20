# Title     : Cap 7 - Ejercicio 7.21
# Objective : TODO
# Created by: leandro
# Created on: 4/14/21

poblacion <- c(1, 3, 5, 6, 7)

u <- mean(poblacion)

v <- sqrt(4 / 5) * sd(poblacion)

muestras <- read.csv('/home/leandro/Documents/TUPED/PyE/Unidades/7-21', header = FALSE)

dfMuestras <- table(muestras)/length(muestras)