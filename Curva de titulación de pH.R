Kw = 1e-14
V.A = 10.00
V.H2O = 0
problema = function(pH, V.T){
  H3O = 10^-pH
  HO = Kw / H3O
  V.total = V.T + V.A + V.H2O
  C.BaOH2= 0.00734 * V.A / V.total
  C.HCl = 0.02810 * V.T / V.total
  Ba = C.BaOH2
  Cl = C.HCl
  2*Ba - Cl - HO + H3O
}
V.T = seq(0, 100, by = 1)
pH = sapply(V.T, function(x)
uniroot(problema, V.T = x, interval = c (0,14), tol=1e-14)$root)
plot (V.T, pH)
lines(V.T, pH, col = "red")
V.H2O = 50.00
pH.2 = sapply(V.T, function(x)
  uniroot(problema, V.T = x, interval = c (0,14), tol=1e-14)$root)
lines(V.T, pH.2, col = "green")
V.H2O = 200.00
pH.3 = sapply(V.T, function(x)
  uniroot(problema, V.T = x, interval = c (0,14), tol=1e-14)$root)
lines(V.T, pH.3, col = "black")
V.total = V.T + V.A + V.H2O
y = V.total*10^-pH.3
x = V.T
plot (x[x>5.5], y[x>5.5])
new.x= x[x>5.5]
new.y= y[x>5.5]
lm(new.y ~ new.x)
V.eq = 0.1468 / 0.0281
uniroot(problema, pH=7, interval = c (0,14), tol=1e-14)$root
V.total = V.T + V.A + V.H2O
y = V.total*10^(-14+pH.3)
x = V.T
plot (x[x<5.5], y[x<5.5])
new.x= x[x<5.5]
new.y= y[x<5.5]
lm(new.y ~ new.x)
library(pracma)
p.d = gradient(pH.3, h1 = V.T)
s.d = gradient(p.d, h1 = V.T)
plot(V.T, p.d)
plot(V.T, s.d)
s.d.min = min(s.d)
s.d.max = max(s.d)
V.T.min= V.T[s.d == s.d.min]
V.T.max= V.T[s.d == s.d.max]
s.d.min
s.d.max
V.T.min
V.T.max

