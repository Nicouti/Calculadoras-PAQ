P.A.O=0.4*0.08314466*373.15 / 2
P.B.O=0.2*0.08314466*373.15 / 2
K=50
problema = function(P.C){
  P.D = P.C
  P.A = P.A.O-P.C
  P.B = P.B.O-P.C
  K - P.C * P.D / (P.A * P.B)
  
}
P.C = uniroot (problema, interval = c(0,P.B.O), tol= 1e-14)$root
P.D = P.C
P.A = P.A.O-P.C
P.B = P.B.O-P.C
data.frame(P.D, P.A, P.B)
av = (P.B - P.B.O) / -P.B.O

