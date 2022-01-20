pKa=4.75
Ka = 10^-pKa
Kw = 1e-14
CHCl=0.1
problema = function(pH){
  H3O = 10^-pH
  HO = Kw / H3O
  Cl = CHCl*Ka/(H3O+Ka)
  H3O - Cl - HO
}
problema(pH=9)
pH = uniroot(problema, c(0,14), tol= 1e-14)$root
pH
problema(pH=1.694981)
H3O = 10^-pH
HO = Kw / H3O
Cl = CHCl*Ka/(H3O+Ka)
H3O - Cl - HO
data.frame(H3O, HO, Cl, H3O - Cl - HO, HCl=CHCl - Cl )
