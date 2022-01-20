c.HNO3=0.002557
c.NaOH=0.000003893
problema <- function(pH){
  H3O = 10^-pH
  HO = 1e-14 / H3O
  Na = c.NaOH
  NO3 = c.HNO3
  H3O + Na - NO3 - HO}
pH <- uniroot(problema, interval = c(0,14), tol = 1e-14)$root
pH
## [1] 1.92739
C.NaOH = 0.1
C.NH3 = 0.05
pKa=9.24
Ka = 10^-pKa
problema.2 = function(pH){
  H3O = 10^-pH
  HO = 1e-14 / H3O
  Na = C.NaOH
  a.NH4 = H3O / (H3O + Ka)
  NH4 = a.NH4 * C.NH3
  H3O + NH4 + Na - HO
}
pH = uniroot(problema.2, interval = c(0,14), tol = 1e-14)$root
pH
## Para volumen determinado v * conc.
C.NH3 = 0.05 
C.metan = 0.1 
pKa.NH4 = 9.24
pKa.metan = 10.66
Ka.NH4 = 10^-pKa.NH4
Ka.metan = 10^-pKa.metan
problema.3=function(pH){
  H3O = 10^-pH
  HO = 1e-14 / H3O
  a.NH4 = H3O / (H3O + Ka.NH4)
  a.metan = H3O / (H3O + Ka.metan)
  NH4 = a.NH4 * C.NH3
  metan = a.metan * C.metan
  metan + H3O + NH4 - HO
}
pH = uniroot(problema.3, interval = c(0,14), tol = 1e-14)$root
pH