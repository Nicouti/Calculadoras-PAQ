Pka=9.24
Ka=10^-Pka
Kb=1e-14/Ka
c.B=1
b=Kb
c=-2*c.B*Kb
x = (-b+sqrt(b^2-4*c))/2
x 
H3O=1e-14/x
pH = -log10(H3O)
pH
problema = function(pH.2){
  H3O = 10^-pH.2
  HO  = 1e-14 / H3O
  a.HB = H3O / (H3O + Ka)
  a.B  = Ka / (H3O + Ka)
  HB = a.HB * c.B
  B  = a.B * c.B 
  Ba = c.B
  H3O - B - HO+ Ba
}
pH.2 = uniroot(problema, c(0, 14), tol = 1e-14)$root
pH.2

