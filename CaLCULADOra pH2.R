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
