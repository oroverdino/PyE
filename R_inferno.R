# Title     : R inferno
# Created by: leandro
# Created on: 3/28/21

quadratic.formula <- function(a, b, c) {
  rad <- b^2 - 4*a*c
  if (is.complex(rad) || all(rad >= 0)) {
    rad <- sqrt(rad)
  } else {
    rad <- sqrt(as.complex(rad))
  }
  cbind(-b -rad, -b + rad) / (2*a)
}

