C.ACOH=0.010
C.ACONa=0.010
Kw = 1e-14
Ka = 1.75e-5
C.Total = C.ACOH + C.ACONa
problema = function(pH){
  H3O = 10^-pH
  HO = Kw / H3O
  Na = C.ACONa
  a.ACO=Ka / (Ka+H3O)
  a.COH= H3O / (H3O+Ka)
  ACO = a.ACO * C.Total
  ACOH = a.COH * C.Total
  H3O + Na - ACO - HO
}
pH = uniroot(problema, c(0,14), tol=1e-14)$root
pH
-log10(Ka)

C.ACOH=0.010
C.ACONa=0.010
C.KOH = 0.5
Kw = 1e-14
Ka = 1.75e-5
C.Total = C.ACOH + C.ACONa
problema.2 = function(pH.2){
  H3O = 10^-pH.2
  HO = Kw / H3O
  Na = C.ACONa
  K = C.KOH
  a.ACO=Ka / (Ka+H3O)
  a.COH= H3O / (H3O+Ka)
  ACO = a.ACO * C.Total
  ACOH = a.COH * C.Total
  H3O + Na + K - ACO - HO
}
pH.2 = uniroot(problema.2, c(0,14), tol=1e-14)$root
pH.2
