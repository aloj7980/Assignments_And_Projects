##Charles R Doss
library(astsa)
library(lattice)
pdfpath <- "../../slides/figures/"


## Quadratic formula
quadform <- function(a,b,c){
    disc <- sqrt(b^2 - 4*a*c)
    (-b + c(disc, -disc))/(2*a)
}

quadform(-.45, -.4,1)
1/quadform(-.45, -.4,1)


## polyroot
polyroot(c(1, -.4, -.45))
1/polyroot(c(1, -.4, -.45))
