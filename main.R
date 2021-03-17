# Title     : Trabajo Integrador
# Created by: leandro
# Created on: 3/11/21

## EJERCICIO 1

# librerias
library(xtable)

# cargamos el archivo csv
if(file.exists('./PlagasSoja.csv')){
  df_raw <- as.data.frame(read.csv('./PlagasSoja.csv'))
}

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

colnames(orugas) <- c('FLORACION', 'FRUCTIFICACION')

defoliacion <- cbind(
  as.matrix(summary(df$DEFOLIACION[df$DESARROLLO == 'FLORACION'])),
  as.matrix(summary(df$DEFOLIACION[df$DESARROLLO == 'FRUCTIFICACION']))
)

colnames(defoliacion) <- c('FLORACION', 'FRUCTIFICACION')

print(xtable(orugas,
             type = 'latex',
             caption = 'ORUGAS - medidas resumen',
             label = 'table:summary_orugas'),
      file = 'summary.tex')

print(xtable(defoliacion,
            type = 'latex',
            caption = 'DEFOLIACION - medidas resumen',
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

plot(ORUGAS ~ DESARROLLO,
     data = df,
     col = c('red', 'cyan'))

plot(DEFOLIACION ~ DESARROLLO,
     data = df,
     col = c('red', 'cyan'))

# graficos de dispersion

library(ggplot2)

desarrollo <- as.factor(
  ifelse(df$DESARROLLO == 'FLORACION', 'FLORACION', 'FRUCTIFICACION'))

scatter_df <- data.frame(x = df$ORUGAS, y = df$DEFOLIACION, group = desarrollo)

ggplot(scatter_df, aes(x = x, y = y)) +
  geom_point(aes(colour = group)) +
  scale_color_discrete('DESARROLLO') +
  xlab('ORUGAS por metro lineal de surco') +
  ylab('DEFOLIACION %') +
  theme_bw() +
  theme(legend.position = 'top', legend.justification = 'center')

## EJERCICIO 2
#
# FLORACION: DEFOLIACION >= 30 & ORUGAS >= 20
# FRUCTIFICACION: DEFOLIACION > 8 & ORUGAS >= 10

# 1. creamos un subconjunto con las condiciones necesarias
# 2. eliminamos las filas NaN (Not a Number) resultantes de
#    no cumplir con las condiciones
# 3. lo convertimos a un tipo data.frame para 4.
# 4. creamos un subconjunto con la variable DESARROLLO de interes
# entonces:

# 4.
aFumigar_floracion <- subset(
  # 3.
  data.frame(
    # 2.
    na.exclude(
      # 1.
      subset(df, DEFOLIACION > 29 & ORUGAS > 19)
    )
  ),
  DESARROLLO == 'FLORACION',
  select = Productor)

aFumigar_fructificacion <- subset(
  data.frame(
    na.exclude(
      subset(df, DEFOLIACION > 8 & ORUGAS > 9)
    )
  ),
  DESARROLLO == 'FRUCTIFICACION',
  select = Productor)

FUMIGACION <- rep(FALSE, dim(df)[1])
df$FUMIGACION <- FUMIGACION

# ningun Productor tiene los dos estados de DESARROLLO
df$FUMIGACION <- is.element(df$Productor, c(
  aFumigar_floracion$Productor,
  aFumigar_fructificacion$Productor))

tabla_plantaciones <- cbind(c('FLORACION', 'FRUCTIFICACION'),
                            c(13, 28))

colnames(tabla_plantaciones) <- c('Desarrollo',
                                  'Cantidad')

print(xtable(tabla_plantaciones,
             type = 'latex',
             caption = 'Cantidad de plantaciones que deben
             fumigarse por estado de DESARROLLO',
             label = 'table:plantacionesAfumigar'),
      file = 'plantaciones.tex')

#####
install.packages(data.table)
library(data.table)

dftable <- as.data.table(df)

aFumigar_floracion <- dftable[ORUGAS > 19 & DEFOLIACION > 29, .N, DESARROLLO == 'FLORACION']
aFumigar_fructificacion <- dftable[ORUGAS > 9 & DEFOLIACION > 8, .N, DESARROLLO == 'FRUCTIFICACION']
