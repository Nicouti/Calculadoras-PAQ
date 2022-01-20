alp = function(pH, pKa){
  n = length(pKa)
  num = 10^(-(pH*(n:0) + cumsum(c(0, pKa))))
  num / sum(num)
}
## PUNTO 3
pH <- 8.371
c.NH3 <- 0.262 
pKa <- 9.244
Ka <- 10^-pKa
V.Total <- 1
problema = function(c.NH4SO4){
  H3O <- 10^-pH
  HO <- 1e-14 / H3O
  SO4 <- c.NH4SO4
  a.NH4 <- H3O / (H3O + Ka)
  NH4 <- c.NH3 + (2*c.NH4SO4*a.NH4)
  H3O + NH4 - 2*SO4 - HO
}
c.NH4SO4 = uniroot(problema, interval = c(0, 1000), tol= 1e-14)$root
c.NH4SO4
## c.NH4SO4 = 1.108838
## Calculando la masa de (NH4)2SO4 al 85.3 %
NH4SO4 <- c.NH4SO4 * V.Total * 132.13952 
NH4SO4 * 85.3/100
## Masa de (NH4)2SO4 al 85.3 % = 37.49402
# Capacidad amortiguadora en bases
pH <- 9.371
c.NH3 <- 0.262 
c.NH4SO4 <- 1.108838
pKa <- 9.244
Ka <- 10^-pKa
V.Total <- 1
problema.2=function(BCb){
  H3O <- 10^-pH
  HO <- 1e-14 / H3O
  SO4 <- c.NH4SO4
  a.NH4 <- H3O / (H3O + Ka)
  NH4 <- c.NH3 + (2*c.NH4SO4*a.NH4)
  H3O + NH4 - 2*SO4 - HO + BCb
}
BCb = uniroot(problema.2, interval = c(0, 1000), tol= 1e-14)$root
BCb

0.9 * H3O + c.NH3 * (9*0.4274095*0.572905/(0.4274095 + 10*0.572905))+9*HO
# Capacidad amortiguadora en ácidos
pH <- 7.371
c.NH3 <- 0.262 
c.NH4SO4 <- 1.108838
pKa <- 9.244
Ka <- 10^-pKa
V.Total <- 1
  problema.3=function(BCa){
  H3O <- 10^-pH
  HO <- 1e-14 / H3O
  SO4 <- c.NH4SO4
  a.NH4 <- H3O / (Ka + H3O)
  NH4 <- c.NH3 + (2*c.NH4SO4*a.NH4)
  H3O + NH4 - 2*SO4 - HO - BCa
  }
BCa = uniroot(problema.3, interval = c(0, 1000), tol= 1e-14)$root
BCa

9 * H3O + c.NH3 * (9*0.4274095*0.572905/(10*0.4274095 + 0.572905))+0.9*HO

## Verificación
c.NH3 <- 0.262 
c.NH4SO4 <- 1.108838
pKa <- 9.244
Ka <- 10^-pKa
BCa <- 0.06980491
ver.1 <- function(pH){
  H3O <- 10^-pH
  HO <- 1e-14 / H3O
  SO4 <- c.NH4SO4
  a.NH4 <- H3O / (Ka + H3O)
  NH4 <-  c.NH3 + (2*c.NH4SO4*a.NH4)
  H3O + NH4 - 2*SO4 - HO - BCa
}
pH = uniroot(ver.2, interval = c(0, 1000), tol= 1e-14)$root
pH
##
c.NH3 <- 0.262 
c.NH4SO4 <- 1.108838
pKa <- 9.244
Ka <- 10^-pKa
BCb <- 0.3023616
ver.1 <- function(pH){
  H3O <- 10^-pH
  HO <- 1e-14 / H3O
  SO4 <- c.NH4SO4
  a.NH4 <- H3O / (Ka + H3O)
  NH4 <-  c.NH3 + (2*c.NH4SO4*a.NH4)
  H3O + NH4 - 2*SO4 - HO + BCb
}
pH = uniroot(ver.2, interval = c(0, 1000), tol= 1e-14)$root
pH

