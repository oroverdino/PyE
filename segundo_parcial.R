# Title     : TODO
# Objective : TODO
# Created by: leandro
# Created on: 4/21/21

# 4-b.ii

(.6133 - .63) / (.01247 / sqrt(1500))

pnorm(.6667, .63, .01247) - pnorm(.6133, .63, .01247)

dnorminv<-function(y) sqrt(-2*log(sqrt(2*pi)*y))

plot(x, y)
points(dnorminv(y),y,pch=3)