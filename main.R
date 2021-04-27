# Title     : Trabajo Integrador
# Created on: 3/11/21

## EJERCICIO 1

# libreria para la creacion de tablas en latex
library(xtable)

# cargamos el archivo csv
if(file.exists('./PlagasSoja.csv')){
  df_raw <- as.data.frame(read.csv('./PlagasSoja.csv'))
} else {
  print('El archivo csv no está en este directorio')
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
CV_floracion <- apply(cbind(df$ORUGAS[df$DESARROLLO == 'FLORACION'], df$DEFOLIACION[df$DESARROLLO == 'FLORACION']), 2,
function (X){
  cv <- sd(X) / mean(X)
  return(cv)
})

CV_fructificacion <- apply(cbind(df$ORUGAS[df$DESARROLLO == 'FRUCTIFICACION'], df$DEFOLIACION[df$DESARROLLO == 'FRUCTIFICACION']), 2,
                      function (X){
                        cv <- sd(X) / mean(X)
                        return(cv)
                      })

stdDev_floracion <- apply(cbind(df$ORUGAS[df$DESARROLLO == 'FLORACION'], df$DEFOLIACION[df$DESARROLLO == 'FLORACION']), 2,
function (X){
  stddev <- sd(X)
  return(stddev)
})

stdDev_fructificacion <- apply(cbind(df$ORUGAS[df$DESARROLLO == 'FRUCTIFICACION'], df$DEFOLIACION[df$DESARROLLO == 'FRUCTIFICACION']), 2,
function (X){
  stddev <- sd(X)
  return(stddev)
})

desviacionesStd_flo <- data.frame(c('ORUGAS', 'DEFOLIACION'), stdDev_floracion, CV_floracion)
colnames(desviacionesStd_flo) <- c('', 's_floracion', 'CV_floracion')

desviacionesStd_fru <- data.frame(c('ORUGAS', 'DEFOLIACION'), stdDev_fructificacion, CV_fructificacion)
colnames(desviacionesStd_fru) <- c('', 's_fructificacion', 'CV_fructificacion')

print(xtable(desviacionesStd_flo,
             type = 'latex',
             caption = 'Desviacion estandar y sus
               respectivos coeficientes de variacion
               durante la FLORACION',
             label = 'table:stdDevs_flo'),
      file = 'stdDevs_flo.tex')

print(xtable(desviacionesStd_fru,
             type = 'latex',
             caption = 'Desviacion estandar y sus
               respectivos coeficientes de variacion
               durante la FRUCTIFICACION',
             label = 'table:stdDevs_fru'),
      file = 'stdDevs_fru.tex')

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

## histogramas orugas

# 1. creamos los histogramas
hist_flor <- hist(df$ORUGAS[df$DESARROLLO == 'FLORACION'],
     breaks = 8)

hist_fruc <- hist(df$ORUGAS[df$DESARROLLO == 'FRUCTIFICACION'],
                  breaks = 8)

# 2. determinamos el rango del eje x combinados
range(c(hist_flor$breaks, hist_fruc$breaks))

# 3. determinamos el rango del eje y combinados
max(c(hist_flor$counts, hist_fruc$counts))

# preparamos un plot de 3 filas en una columna
par(mfrow = c(2, 1))

# graficamos el histograma floracion
plot(hist_flor,
     col = 'red',
     xlim = c(2, 30),
     ylim = c(0, 18),
     xlab = 'FLORACION',
     main = 'cantidad ORUGAS por metro lineal de surco'
)

# luego el de fructificacion
plot(hist_fruc,
     add = FALSE,
     xlim = c(2, 30),
     ylim = c(0, 18),
     xlab = 'FRUCTIFICACION',
     main = '',
     col = 'cyan')

## histogramas defoliacion

hist_flor <- hist(df$DEFOLIACION[df$DESARROLLO == 'FLORACION'],
                  breaks = 8)

hist_fruc <- hist(df$DEFOLIACION[df$DESARROLLO == 'FRUCTIFICACION'],
                  breaks = 8)

range(c(hist_flor$breaks, hist_fruc$breaks))

max(c(hist_flor$counts, hist_fruc$counts))

par(mfrow = c(2, 1))

plot(hist_flor,
     col = 'red',
     xlim = c(0, 60),
     ylim = c(0, 20),
     xlab = 'FLORACION',
     main = 'porcentaje de DEFOLIACION'
)

plot(hist_fruc,
     add = FALSE,
     xlim = c(0, 60),
     ylim = c(0, 20),
     xlab = 'FRUCTIFICACION',
     main = '',
     col = 'cyan')

## EJERCICIO 2
#
# FLORACION: DEFOLIACION >= 30 & ORUGAS >= 20
# FRUCTIFICACION: DEFOLIACION > 8 & ORUGAS >= 10

# # # # # usando data.table # # # #
# db <- fread('./PlagasSoja.csv')
# db[, FUMIGACION := ifelse(
#   ((ORUGAS > 19 & DEFOLIACION > 29) & DESARROLLO == 'FLORACION') |
#   ((ORUGAS > 9 & DEFOLIACION > 8) & DESARROLLO == 'FRUCTIFICACION'),
#   TRUE, FALSE)]
#
# * sin filtrar tenemos la misma cantidad
#
# db[, .N, .(DESARROLLO, FUMIGACION)][order(DESARROLLO, FUMIGACION)]
# # # # # # #

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

# el pqt data.table nos limpia el codigo
#
#install.packages(data.table)
library(data.table)

dftable <- as.data.table(df)

# creamos la tabla para imprimir
tabla_plantaciones <- dftable[FUMIGACION == TRUE, .N, by = .(DESARROLLO)]

print(xtable(tabla_plantaciones,
             type = 'latex',
             caption = 'Cantidad de plantaciones que deben fumigarse',
             label = 'table:plantacionesAfumigar'),
      file = 'plantaciones.tex')

# encuadramos la tabla completa
# *** en el script latex debimos hacer modificaciones ***
print(xtable(head(dftable),
             type = 'latex',
             caption = 'Base de datos modificada',
             label = 'table:dftable'),
      file = 'dftable_full.tex')

## EJERCICIO 3
#
# Asumimos Poisson, con lambda = 9

P12 <- 1 - ppois(12, 9)

# para encontrar la proporcion de campos en fructificacion
# donde se encuentran mas de 12 orugas

P12_fruct <- dftable[DESARROLLO == 'FRUCTIFICACION', .N, ORUGAS > 12]

proporcion <- P12_fruct$N[2] / sum(P12_fruct$N)

## EJERCICIO 5
#
n <- 137
p0 <- .21
pS <- .3

z_obs <- (pS - p0) / sqrt(p0 * (1 - p0) / n)

p_value <- 1 - pnorm(z_obs, 0, 1)
