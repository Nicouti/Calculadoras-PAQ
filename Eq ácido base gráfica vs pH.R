Kw = 1e-14
problema = function(pH, CHCl){
  H3O = 10^-pH
  HO= Kw / H3O
  Cl = CHCl
  H3O - Cl - HO
  
}
CHCl= 10^-seq(1, 9, by = 0.1)
pH = sapply (CHCl, function(x)
  uniroot(problema, CHCl = x, interval = c(0,14), tol= 1e-14)$root)
plot(CHCl, pH, log = "x", type = "l")  

Kw = 1e-14
problema.2 = function(pH.2, C.BaOH2){
  H3O = 10^-pH.2
  HO = Kw / H3O
  Ba = C.BaOH2
  H3O - 2*Ba - HO
  
}
C.BaOH2= 0.0741
C.BaOH2= 10^-seq(1, 9, by = 0.1)
pH.2 = sapply (C.BaOH2, function(x)
  uniroot(problema.2, C.BaOH2 = x, interval = c(0,14), tol= 1e-14)$root)
plot(C.BaOH2, pH.2, log = "x", type = "l")
lines(C.BaOH2, pH.2, col ="red")
