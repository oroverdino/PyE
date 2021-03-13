# Title     : Trabajo Integrador
# Created by: leandro
# Created on: 3/11/21

# librerias
library(xtable)

# cargamos el archivo csv
df_raw <- as.data.frame(read.csv('./PlagasSoja.csv'))

# analizamos sus tipos de variables
sapply(df_raw, class)

# antes de corregir los formatos debemos filtrar la variable
# numerica DEFOLIACION, que tiene valores negativos
df <- subset(df_raw, DEFOLIACION >= 0)

# formatos apropiados
df$Productor <- as.character(df$Productor)
df$DESARROLLO <- as.factor(df$DESARROLLO)

# creamos una tabla con medidas resumen apropiadas
# y el codigo latex para su presentacion
orugas <- cbind(
  as.matrix(summary(df$ORUGAS[df$DESARROLLO == 'FLORACION'])),
  as.matrix(summary(df$ORUGAS[df$DESARROLLO == 'FRUCTIFICACION']))
)

colnames(orugas)[1] <- 'FLORACION'
colnames(orugas)[2] <- 'FRUCTIFICACION'

defoliacion <- cbind(
  as.matrix(summary(df$DEFOLIACION[df$DESARROLLO == 'FLORACION'])),
  as.matrix(summary(df$DEFOLIACION[df$DESARROLLO == 'FRUCTIFICACION']))
)

colnames(defoliacion)[1] <- 'FLORACION'
colnames(defoliacion)[2] <- 'FRUCTIFICACION'

print(xtable(orugas,
             type = 'latex',
             caption = 'ORUGAS - medidas resumen',
             label = 'table:summary_orugas'),
      file = 'summary.tex')

print(xtable(defoliacion,
            type = 'latex',
            caption = 'DEFOLIACION- medidas resumen',
            label = 'table:summary_defoliacion'),
      append = TRUE,
      file = 'summary.tex')

# calculamos el coeficiente de variacion para comparar
# con la std dev en una tabla
CV <- apply(df[,c(-1,-2)], 2, function (X){
  cv <- sd(X) / mean(X)
  return(cv)
})

stdDev <- apply(df[,c(-1,-2)], 2, function (X){
  stddev <- sd(X)
  return(stddev)
})

desviacionesStd <- data.frame(stdDev, CV)

print(xtable(desviacionesStd,
             type = 'latex',
             caption = 'Desviacion estandar y sus
               respectivos coeficientes de variacion',
             label = 'table:stdDevs'),
      file = 'stdDevs.tex')

# diagrama caja con bigotes

# para evitar largas encadenaciones de codigo
df.floracion <- subset(df, DESARROLLO == 'FLORACION')
df.fructificacion <- subset(df, DESARROLLO == 'FRUCTIFICACION')

par(mfrow = c(1, 1))

boxplot(c(df.floracion[3], df.fructificacion[3]),
        col = c('red', 'blue'),
        main = 'ORUGAS',
        ylab = 'cantidad por metro lineal de surco',
        names = c('FLORACION', 'FRUCTIFICACION')
)

boxplot(c(df.floracion[4], df.fructificacion[4]),
        col = c('red', 'blue'),
        main = 'DEFOLIACION',
        ylab = 'porcentaje',
        names = c('FLORACION', 'FRUCTIFICACION')
)

