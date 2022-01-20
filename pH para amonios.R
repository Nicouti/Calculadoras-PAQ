pKa=9.24
Ka = 10^-pKa
C.NH4NO3 = 0.1
problema=function(pH){
  H3O = 10^-pH
  HO = 1e-14/H3O
  a.NH4 = H3O / (H3O+Ka)
  NH4 = a.NH4 * C.NH4NO3
  NO3 = C.NH4NO3
  H3O + NH4 - HO - NO3
}
pH = uniroot(problema, c(0,14), tol= 1e-14)$root
pH