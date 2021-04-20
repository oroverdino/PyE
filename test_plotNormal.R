# Title     : TODO
# Objective : TODO
# Created by: leandro
# Created on: 3/19/21

# Ejemplo 6.9 (pp 255)
mu <- 25.5
desvStd <- 4.5
x0 <- 30

rango <- seq(mu - 4 * desvStd, mu + 4 * desvStd, 0.01)

y <- dnorm(rango, mu, desvStd)

plot(rango, y,
     main = '',
     type = 'l',
     ylim = c(0, max(y) + 0.01),
     axes = FALSE
     )

axis(1, at = seq(mu - 3 * desvStd, mu + 3 * desvStd, desvStd))

## ahora vamos el area
#cord.a <- c(0, seq(min(rango), x0, .01), x0)
#cord.b <- c(0, dnorm(seq(min(rango), x0, .01), mu, desvStd), 0)
#
#polygon(cord.a, cord.b, col = 'yellow')
#
#cord.c <- c(x0, seq(x0, max(rango), .01), 0)
#cord.d <- c(0, dnorm(seq(x0, max(rango), .01), mu, desvStd), 0)

polygon(cord.c, cord.d, col = 'yellow')

## finalmente el area bajo la curva
P_30 <- 1 - pnorm(x0, mu, desvStd)

# Ejemplo 6.10 (pp256)
## el percentil 95
Q_95 <- qnorm(.95, mu, desvStd)
