c.RNO2 <- 0.0196
c.Total <- 0.380
pH <- 3.906
pKa <- 4.74
Ka <- 10^-pKa
problema = function(c.CH3COONa){
  H3O <- 10^-pH
  HO <- 1e-14 / H3O
  a.CH3COO <- H3O / (Ka + H3O)
  CH3COO <- c.Total * a.CH3COO
  Na <- c.CH3COONa
  H3O + Na - CH3COO - HO
}
c.CH3COONa = uniroot(problema, interval= c(0, 1000), tol=1e-14 )$root
c.CH3COONa
## B 
c.RNO2 <- 0.0196
c.Total <- 0.380
c.CH3COONa <- 0.3313035
c.Ag <- 0.02352
pKa <- 4.74
Ka <- 10^-pKa
problema = function(pH){
  H3O <- 10^-pH
  HO <- 1e-14 / H3O
  a.CH3COO <- H3O / (Ka + H3O)
  CH3COO <- c.Total * a.CH3COO
  Na <- c.CH3COONa
  Ag <- c.Ag
  RNH2OH <- c.RNO2
  H3O + Na - CH3COO - HO +  RNH2OH + Ag
}
pH = uniroot(problema, interval= c(0, 14), tol=1e-14 )$root
pH
