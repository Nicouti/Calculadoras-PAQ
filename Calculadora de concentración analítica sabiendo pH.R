pH=4.82
pKa=9.24
Ka = 10^-pKa
problema= function(C.NH4Cl){
  H3O = 10^-pH
  HO = 1e-14 / H3O
  a.NH4 = H3O / (H3O + Ka)
  Cl = C.NH4Cl
  NH4 = C.NH4Cl * a.NH4
  H3O + NH4 - Cl - HO
}
C.NH4Cl = uniroot(problema, c(0,14), tol= 1e-14)$root
C.NH4Cl
## Concentración para bases
pH=10.5
pKa=9.24
Ka = 10^-pKa
problema= function(C.NH3){
  H3O = 10^-pH
  HO = 1e-14 / H3O
  a.NH3 = H3O / (H3O + Ka)
  NH3 = C.NH3 * a.NH3
  H3O + NH3 - HO
}
C.NH3 = uniroot(problema, c(0,14), tol= 1e-14)$root
C.NH3