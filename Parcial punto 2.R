storeWarn<- getOption("warn")
options(warn = -1)
options(repr.plot.width=8, repr.plot.height=6)
if (!require("pracma")) install.packages("pracma")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("plotly")) install.packages("plotly")
if (!require("nleqslv")) install.packages("nleqslv")

library(RColorBrewer)
## PUNTO 2
alp = function(pH, pKa){
  n = length(pKa)
  num = 10^(-(pH*(n:0) + cumsum(c(0, pKa))))
  num / sum(num)
}
pKa = 6
carga= c(0, -1)
sum(alp(6.263, pKa) * carga)

pH= 6.263
H3O = 10^-pH
HO = 1e-14/H3O
H3O - HO 

pKa = 6
carga= c(0, -1)
sum(alp(7.263, pKa) * carga)

A=matrix(c(1, 1, -0.6469319, -0.9482486), ncol=2)
B=c(0, -0.191 - 10^-pH+10^(-14+pH))
solve(A, B)
## Verificaci?n con balance de cargas
pH = 6.263
C.Total = 0.6338863
C.NaOH = 0.4100813
Ka = 10^-pKa
Na = C.NaOH
a.HnA = Ka / (H3O + Ka)
A = C.Total * a.HnA
H3O + Na - A - HO 
## Capacidad amortiguadora frente a un ?cido
pH = 5.263
Ka = 10^-6
C.Total = 0.6338863
C.NaOH = 0.4100813

  H3O = 10^-pH
  HO = 1e-14 / H3O
  Na = C.NaOH
  a.HnA = Ka / (H3O + Ka)
  A = C.Total * a.HnA
  BCa = H3O + Na - A - HO 
  BCa
## Moles para cada especie 
  V.Total = 2.5
n.NaOH = C.NaOH * V.Total
n.NaOH
n.acido = C.Total * V.Total
n.acido