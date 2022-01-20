
pH = seq(0, 14, by=0.01)
H3O = 10^-pH
Ka = 1.8e-7
pKa= -log10(Ka)
a.HCOO = Ka / (H3O+Ka)
a.HCOOH = H3O / (H3O + Ka)
plot(pH, a.HCOOH, type = "l")
lines(pH, a.HCOO, col="red")
abline(v=pKa)
abline(h=0.5, col="blue")
HCOO = a.HCOO * 0.01
HCOOH = a.HCOOH * 0.01
HO = 1e-14 / H3O
plot(pH, H3O, type = "l", log = "y")
lines(pH, HO, col="red")
lines(pH, HCOO, col="blue")
lines(pH, HCOOH, col="green")
