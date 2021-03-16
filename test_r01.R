# Title     : script de prueba
# Objective : test
# Created by: leandro
# Created on: 11/26/20

puto <- data.frame(c(1:9), c('a','b','c','d','e','f','g','h','i'))

apply(puto, 1, function(x){
  ifelse(x[y,1] < 5, x[y,3] <- TRUE, x[y,3] <- FALSE)
})

puto$fum <- ifelse(puto[1] > 5, 'corregir', 'nosepo')