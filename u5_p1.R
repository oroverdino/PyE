# Title     : Unidad 5 - P1
# Objective : TODO
# Created by: leandro
# Created on: 3/25/21

# ejercicio 7.60
x <- c(6, 1, 3 ,2)

muestras <- combn(x, 2, sample)

# muestreo aleatorio - caso 1
poblacion <- c(3,6,9,12,15)

muestras <- t(combn(poblacion, 3, sample))

# X_: V.A. de las medias
medias <- rowMeans(muestras)

# M: V.A. de las medianas
medianas <- apply(muestras, 1, median)

# frecAbs de medias
dfMedias <- as.data.frame(table(medias)/length(medias))

# frecAbs de medianas
dfMedianas <- as.data.frame(table(medianas)/length(medianas))

# histogramas
plot(dfMedias, ylim = c(0, .3), str = 3)
plot(dfMedianas, ylim = c(0, .45), str = 3)

# muestreo aleatorio - caso 2

x <- 1:15

set.seed(2021)

#rm(muestras)
muestras <- list()

for (i in 1:350) {
  muestras[[i]] <- sample(x, 5)
}

muestras <- do.call(rbind, muestras)

muestras <- unique(muestras)

mediasMuestrales <- rowMeans(muestras)

medianasMuestrales <- apply(muestras, 2, median)

minMedDis <- min(mediasMuestrales)
maxMedDis <- max(mediasMuestrales)

minMediana <- min(medianasMuestrales)
maxMediana <- max(medianasMuestrales)

# graficas
par(mfrow = c(3, 1))

mediasDiscretizadas_25 <- cut(mediasMuestrales,
                           breaks = seq(minMedDis, maxMedDis, by = .25))
mediasDiscretizadas_50 <- cut(mediasMuestrales,
                           breaks = seq(minMedDis, maxMedDis, by = .50))
mediasDiscretizadas_100 <- cut(mediasMuestrales,
                           breaks = seq(minMedDis, maxMedDis, by = 1))

dfMedias_25 <- as.data.frame(
  table(mediasDiscretizadas_25) / length(mediasDiscretizadas_25)
)
dfMedias_50 <- as.data.frame(
  table(mediasDiscretizadas_50) / length(mediasDiscretizadas_50)
)
dfMedias_100 <- as.data.frame(
  table(mediasDiscretizadas_100) / length(mediasDiscretizadas_100)
)

barplot(dfMedias_25$Freq)
barplot(dfMedias_50$Freq)
barplot(dfMedias_100$Freq)

# graficas mediana
par(mfrow = c(1, 1))

dfMedianas <- as.data.frame(
  table(medianasMuestrales) / length(medianasMuestrales)
)

barplot(dfMedianas$Freq)

# confirmando las aproximaciones

rm(poblacion)

poblacion <- t(combn(x, 5, sample))

XTotales <- rowMeans(poblacion)

MTotales <- apply(poblacion, 2, median)

minXtotales <- min(XTotales)
maxXtotales <- max(XTotales)

minMtotales <- min(MTotales)
maxMtotales <- max(MTotales)

XTotalesDiscretizadas_25 <- cut(XTotales,
                             breaks = seq(
                               from = minXtotales,
                               to = maxXtotales,
                               by = .10)
)
dfXtotales_25 <- as.data.frame(
  table(XTotalesDiscretizadas_25) / length(XTotalesDiscretizadas_25))

XTotalesDiscretizadas_50 <- cut(XTotales,
                             breaks = seq(
                               from = minXtotales,
                               to = maxXtotales,
                               by = .20)
)
dfXtotales_50 <- as.data.frame(
  table(XTotalesDiscretizadas_50) / length(XTotalesDiscretizadas_50))

XTotalesDiscretizadas_100 <- cut(XTotales,
                                breaks = seq(
                                  from = minXtotales,
                                  to = maxXtotales,
                                  by = .30)
)
dfXtotales_100 <- as.data.frame(
  table(XTotalesDiscretizadas_100) / length(XTotalesDiscretizadas_100))

par(mfrow=c(3,1))

barplot(dfXtotales_25$Freq,
        main = 'poblacion total',
        xlab = 'by = .1',
)

barplot(dfXtotales_50$Freq,
        main = '',
        xlab = 'by = .2'
)

barplot(dfXtotales_100$Freq,
        main = '',
        xlab = 'by = .3'
)
