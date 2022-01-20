## Problema 38
alp  <- function(pH, pKa){
  n  <- length(pKa)
  numerator  <- 10^(-pH * n:0 - cumsum(c(0, pKa)))
  numerator /  sum(numerator)
}
pKa.H2M<- c(1.92, 6.22)
pKa.NH4 <- c(9.24)
Ka <- 10^-9.24
c.H2M <- 2.5
c.NH3 <- 5
carga <- c(0, -1, -2)
problema <- function(pH){
  H3O <- 10^-pH
  HO <- 1e-14 / H3O
  a.H2M <- alp(pH, pKa.H2M)
  a.NH4 <- H3O / (Ka + H3O)
  NH4 <- c.NH3 * a.NH4
  H2M <- a.H2M * c.H2M 
  H3O + sum(carga * H2M) + NH4 - HO
}
pH = uniroot(problema, interval = c(0,14), tol = 1e-14)$root
pH
## Problema 40 disoluciones
alp  <- function(pH, pKa){
  n  <- length(pKa)
  numerator  <- 10^(-pH * n:0 - cumsum(c(0, pKa)))
  numerator /  sum(numerator)
}
pKa.HFtK<- c(2.95, 5.41)
c.HFtK <- 0.1 * 0.05 / (0.05+0.025)
c.HCl <- 0.1 * 0.025/ (0.05+0.025)
carga <- c(0, -1, -2)
problema <- function(pH){
  H3O <- 10^-pH
  HO <- 1e-14 / H3O
  a.HFtK <- alp(pH, pKa.HFtK)
  HFtK <- a.HFtK * c.HFtK 
  K <- c.HFtK 
  Cl <- c.HCl 
  H3O + sum(carga * HFtK)  - Cl - HO + K
}
pH = uniroot(problema, interval = c(0, 14), tol = 1e-14)$root
pH
## Problema 42
pH <- 5
pKa <- 4.75
Ka <- 10^-pKa
V.Hacet <- 0.5
problema.2 = function(V.NaOH){
  c.NaOH <- 6 * V.NaOH / (V.Hacet + V.NaOH)
  c.Hacet <- 0.250 * V.Hacet / (V.Hacet + V.NaOH)
  H3O <- 10^-pH
  HO <- 1e-14 / H3O
  a.Acet <- Ka / (Ka + H3O)
  Hacet <- c.Hacet * a.Acet
  Na <- c.NaOH
  H3O + Na - Hacet - HO
}
V.NaOH = uniroot(problema.2, interval = c(0, 100), tol = 1e-14)$root
V.NaOH
## Problema 50
# a
pH <- x + 0.5
pKa <- 4.75
Ka <- 10^-pKa
V.Hacet <- 0.025
V.HacetNa <- 0.025
problema.3 = function(V.NaOH, x){
  V.Total <- V.Hacet + V.NaOH + V.HacetNa
  c.NaOH <- 1 * V.NaOH / (V.Total)
  c.Hacet <- 0.2 * V.Hacet / (V.Total)
  C.HacetNa <- 0.4* V.HacetNa / (V.Total)
  H3O <- 10^-pH
  HO <- 1e-14 / H3O
  a.Acet <- Ka / (Ka + H3O)
  Hacet <- c.Hacet * a.Acet + C.HacetNa
  Na <- c.NaOH +  C.HacetNa
  H3O + Na - Hacet - HO
}
V.NaOH = uniroot(problema.3, interval = c(0, 100), tol = 1e-14)$root
V.NaOH

