alp = function(pH, pKa){
  n = length(pKa)
  num = 10^(-(pH*(n:0) + cumsum(c(0, pKa))))
  num / sum(num)
}
pH <- 5.085
pKa <- c(1.268, 4.268)
carga= c(0, -1, -2)
c.HClO4 = 0.4747 

problema = function(C.oxal){
  H3O <- 10^-pH
  HO <- 1e-14 / H3O
  ClO4 <- c.HClO4 *0.010 / (0.500)
  K <- 2*C.oxal  
  a.oxal <- alp(pH, pKa)
  oxal <- C.oxal * a.oxal 
  H3O + K - ClO4 - HO + sum(carga * oxal)
}
C.oxal = uniroot(problema, interval = c(0, 1000), tol= 1e-14)$root
C.oxal
OxalK <- C.oxal * 166.2156
OxalK 
## Verificación
pKa <- c(1.268, 4.268)
carga= c(0, -1, -2)
c.HClO4 = 0.4747 
C.oxal = 0.07170586
problema = function(pH){
  H3O <- 10^-pH
  HO <- 1e-14 / H3O
  ClO4 <- c.HClO4 *0.010 / (0.500)
  K <- 2*C.oxal  
  a.oxal <- alp(pH, pKa)
  oxal <- C.oxal * a.oxal 
  H3O + K - ClO4 - HO + sum(carga * oxal)
}
pH = uniroot(problema, interval = c(0, 1000), tol= 1e-14)$root
pH