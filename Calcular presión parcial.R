
problema = function(K, p.PCl3, P.F){K-p.PCl3^2/(P.F-2*p.PCl3)} 
p.PCl3 = uniroot(problema, K = 3.13e-4, P.F = 1, interval = c(0, 1/2), tol = 1e-14)$root 
p.PCl3



