Ka = 10^-4.76
problema=function(pH, v.NaOH){
  H3O = 10^-pH
  HO = 1e-14 / H3O
  a.COO=Ka / (H3O + Ka)
  V.COO = 1 - v.NaOH
  C.NaOH = 3 * v.NaOH / 1
  C.COOH = 1 * V.COO / 1
  COO = a.COO * C.COOH
  Na = C.NaOH
  H3O + Na - COO - HO
}
v.NaOH = uniroot(problema, pH = 4,interval= c(0, 14), tol=1e-14 )$root
v.NaOH
uniroot(problema, v.NaOH = v.NaOH, interval= c(0,14), tol=1e-14)$root
