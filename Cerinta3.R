par(mfrow=c(1,2)) #impartim in doua coloane plot-ul
#Distributia aleasa este distributia Borel
no <- 3
miu <- 0.5 # miu intre 0 si 1

###
###Functia de masa###
###

functia_de_masa_single_value <- function(no, miu=0.5) {
  res <-0
  if(no >= 1)
  {
  up <- exp(-1*miu*no)*(miu*no)^(no-1)
  down <- factorial(no) 
  res <- up/down
  }
  sprintf("Rezultatul pentru functia de masa pentru numarul %f, valoarea lui miu egala cu %f este %f",no,miu,res)
}

functia_de_masa <- function(no, miu=0.5) {
  res <-0
    up <- exp(-1*miu*no)*(miu*no)^(no-1)
    down <- factorial(no) 
    res <- up/down
  return(res)
}

#Calculul efectiv pentru o singura valoarea intr-un punct al functiei de masa
functia_de_masa_single_value(no,miu)

#Functie ce primeste un interval si reprezinta functia de masa pe interval
functia_de_masa_pe_interval <- function(st, dr) {
  xcoords = st:dr
  ycoords = st:dr
  for(i in xcoords){
    ycoords[i]=functia_de_masa(i)
  }
  plot(xcoords,ycoords,col="magenta", type="s", pch=15,xlab="Intervalul Valorilor",ylab="Functia de Masa")
}

#Reprezentare grafica pentru functia de masa
functia_de_masa_pe_interval(1,10)

###
###Functia de repartitie###
###

functia_de_repartitie_single_value <- function(no, miu=0.5) {
  res <- 0
  t <- floor(no)
  i <- 1
  for(i in 1:t) {
      res <- res + functia_de_masa(i,miu)
  }
  sprintf("Rezultatul pentru functia de repartitie pentru numarul %f, valoarea lui miu egala cu %f este %f",no,miu,res)
}

functia_de_repartitie <- function(no, miu=0.5) {
  res <- 0
  t <-floor(no)
  for(i in 1:t) {
    k <- functia_de_masa(i,miu)
    res <- res+k
    }
  return(res)
}

#Calculul efectiv pentru o singura valoarea intr-un punct al functiei de repartitie
functia_de_repartitie_single_value(no,miu)

#Functie ce primeste un interval si reprezinta functia de repartitie pe interval
functia_de_repartitie_pe_interval <- function(st, dr) {
xcoords = st:dr
ycoords = st:dr
for(i in xcoords){
     ycoords[i]=functia_de_repartitie(i)
}
plot(xcoords,ycoords,col="green", type="s", pch=16,xlab="Intervalul Valorilor",ylab="Functia de Repartitie")
}

#Reprezentare grafica pentru functia de repartitie
functia_de_repartitie_pe_interval(1,10)

