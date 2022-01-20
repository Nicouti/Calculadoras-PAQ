alp = function(pH, pKa){
  n = length(pKa)
  num = 10^(-(pH*(n:0) + cumsum(c(0, pKa))))
  num / sum(num)
}
pH <- 7.350
pKa <- c(2.150, 7.210, 12.33)
carga= c(0, -1, -2, -3)

problema = function(pH, V.H3PO4){
  H3O <- 10^-pH
  HO = 1e-14 / H3O
  a.H3PO4 = alp(pH, pKa)
  V.NaOH = 1 - V.H3PO4
  c.H3PO4 = 0.2563 * V.H3PO4  
  c.NaOH = 1.345* V.NaOH
  H3PO4 <- c.H3PO4 * a.H3PO4
  Na = c.NaOH 
  H3O + Na + sum(carga * H3PO4) - HO
}
V.H3PO4 = uniroot(problema, pH = 7.350, interval= c(0, 1000), tol=1e-14 )$root
V.H3PO4
V.NaOH = 1 - V.H3PO4
V.NaOH
## a = 768.6023 mL b = 231.977 mL

pKa <- c(2.150, 7.210, 12.33)
carga= c(0, -1, -2, -3)
V.H3PO4 = 0.7686023
v.NaOH = 1 - V.H3PO4
problema = function(pH){
  H3O <- 10^-pH
  HO = 1e-14 / H3O
  a.H3PO4 = alp(pH, pKa)
  V.NaOH = 1 - V.H3PO4
  c.H3PO4 = 0.2563 * V.H3PO4  
  c.NaOH = 1.345* V.NaOH
  H3PO4 <- c.H3PO4 * a.H3PO4
  Na = c.NaOH 
  H3O + Na + sum(carga * H3PO4) - HO
}
pH = uniroot(problema, interval= c(0, 1000), tol=1e-14 )$root
pH 
