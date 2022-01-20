Kw = 1e-14
Ka.HA=1e-3
Ka.HB=1e-3
C.HA=0.0200
C.HB=0.0250
problema = function(pH){
  H3O = 10^-pH
  HO = Kw / H3O
  a.A = Ka.HA / (H3O + Ka.HA)
  a.B = Ka.HB / (H3O + Ka.HB)
  A = C.HA * a.A
  B = C.HB * a.B
  H3O - HO - A - B
}
pH = uniroot(problema, c(0,14), tol=1e-14)$root
pH


H3O = 10^-pH
HO = Kw / H3O
a.HA = H3O/ (H3O + Ka.HA)
a.HB = H3O / (H3O + Ka.HB)
A = C.HA * a.A
B = C.HB * a.B
HA = C.HA * a.HA
HB = C.HB * a.HB
data.frame(pH, H3O, HA, A, HB, B)


Kw = 1e-14
Ka.HA = 1e-3
c.HA = 0.0200
c.HB = 0.0250

problema.2 = function(pH.2, pKa.HB){
  H3O = 10^-pH.2
  HO  = Kw / H3O
  Ka.HB = 10^-pKa.HB
  a.A = Ka.HA / (H3O + Ka.HA)
  a.B = Ka.HB / (H3O + Ka.HB)
  A = c.HA * a.A
  B = c.HB * a.B
  H3O - A - B - HO}
pKa.HB = seq(3, 9, by = 1)
pH.2 = sapply(pKa.HB, function(x)
  uniroot(problema.2, pKa.HB = x, c(0, 14), tol = 1e-14)$root)
pH.2

Ka.HB = 10^-pKa.HB
H3O = 10^-pH.2
HO  = Kw / H3O
a.HA = H3O / (H3O + Ka.HA)
a.HB = H3O / (H3O + Ka.HB)
a.A = Ka.HA / (H3O + Ka.HA)
a.B = Ka.HB / (H3O + Ka.HB)
A = c.HA * a.A
B = c.HB * a.B
HA = c.HA * a.HA
HB = c.HB * a.HB
data.frame(pH.2, pKa.HB, a.HA, a.A, a.HB, a.B)

