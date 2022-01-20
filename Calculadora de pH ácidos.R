Ka = 6.7e-4
Pka=9.24
C.HA=0.01
b=Ka
c=-CHA*Ka
x = (-b+sqrt(b^2-4*c))/2
x
-log10(x)
y=sqrt(Ka*CHA)
-log10(y)

Pka=9.24
Ka =10^-Pka
c.HA=1
problema = function(pH){
  H3O = 10^-pH
  HO  = 1e-14 / H3O
  a.HA = H3O / (H3O + Ka)
  a.A  = Ka / (H3O + Ka)
  HA = a.HA * c.HA
  A  = a.A * c.HA
  H3O - A - HO 
}
pH = uniroot(problema, c(0, 14), tol = 1e-14)$root
pH

