Kw = 1e-14
c.NaOH = 0.01442 * 0.054 / (200)
CHNO3 = 0.02557 * 20 / (200)
problema = function(pH){
  H3O = 10^-pH
  HO = Kw / H3O
  NO3 = CHNO3
  Na = c.NaOH
  H3O + Na - NO3 - HO 
}
uniroot(problema, interval = c(0,14), tol= 1e-14)$root

Kw = 1e-14
problema.2 = function(pH, V.HNO3){
  H3O = 10^-pH
  HO = Kw / H3O
  c.NaOH = 0.01442 * 0.054 / (200)
  CHNO3 = 0.02557 * V.HNO3 / (200)
  NO3 = CHNO3
  Na = c.NaOH
  H3O + Na - NO3 - HO 
}
uniroot(problema.2, pH=2.592931, interval = c(0,1000), tol= 1e-14)$root


