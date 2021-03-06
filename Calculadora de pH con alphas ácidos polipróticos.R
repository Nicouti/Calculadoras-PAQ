## -------------------------------------------------------------------------------------------------------------
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("scales")) install.packages("scales")


## -------------------------------------------------------------------------------------------------------------
alp  <- function(pH, pKa){
  n  <- length(pKa)
  numerator  <- 10^(-pH * n:0 - cumsum(c(0, pKa)))
  numerator /  sum(numerator)
}


## -------------------------------------------------------------------------------------------------------------
pKa <- c(2.148, 7.199, 12.15)
pH <- 11
alp(pH, pKa)


## -------------------------------------------------------------------------------------------------------------
carga <- c(0, -1, -2, -3)
carga


## -------------------------------------------------------------------------------------------------------------
calculo.pH <- function(pH){
  H3O <- 10^-pH
  HO <- 1e-14 / H3O
  a.HnA <- alp(pH, pKa)
  HnA <- c.Total * a.HnA
  K <- c.K
  H3O + K + sum(carga * HnA) - HO
} 


## -------------------------------------------------------------------------------------------------------------
c.K3PO4 <- 0.01611
pKa <- c(2.148, 7.199, 12.15)
carga <- c(0, -1, -2, -3)
c.Total <- c.K3PO4
c.K <- 3 * c.K3PO4
pH <- uniroot(calculo.pH, interval = c(0, 14), tol = 1e-14)$root
pH


## -------------------------------------------------------------------------------------------------------------
my.plot <- function(pKa){
  pH <- seq(0, 14, by = 0.01)
  a.HnA  <- data.frame(t(sapply(pH, alp, pKa)))
  names(a.HnA)  <- paste("alpha[", 0:(length(pKa)),"]", sep = "")
  df <- data.frame(x = pH, a.HnA) %>% 
    pivot_longer(names_to = "key", values_to = "y", -x)
  n.especies <- length(pKa) + 1
  nombres <- paste("alpha[", 0:n.especies,"]", sep = "")
  p  <- ggplot(df, aes(x, y, color = key)) + 
    geom_line(size = 1) +
    scale_color_brewer("", 
                      labels = parse(text = nombres),
                       palette = "Dark2") +
    geom_vline(xintercept = pKa, linetype = "dashed", size = 0.5) +
    scale_x_continuous("pH", breaks = 0:14) + 
    scale_y_continuous(bquote(alpha), breaks = seq(0, 1, by = 0.1)) +
    theme_bw() + 
    theme(text = element_text(size = 12),
          legend.background = element_blank())
return(p)
}


## -------------------------------------------------------------------------------------------------------------
a <- my.plot(pKa)
a


## -------------------------------------------------------------------------------------------------------------
c.K3PO4 <- 0.0
c.K2HPO4 <- 0.02021
c.KH2PO4 <- 0.0
c.H3PO4 <- 0.0
pKa <- c(2.148, 7.199, 12.15)
carga <- c(0, -1, -2, -3)
c.Total <- c.H3PO4 + c.KH2PO4 + c.K2HPO4 + c.K3PO4
c.K <- 1 * c.KH2PO4 + 2 * c.K2HPO4 + 3 * c.K3PO4
pH <- uniroot(calculo.pH, interval = c(0, 14), tol = 1e-14)$root
pH

## -------------------------------------------------------------------------------------------------------------
(7.199 + 12.15) / 2

## -------------------------------------------------------------------------------------------------------------
c.K3PO4 <- 0.0
c.K2HPO4 <- 0.0
c.KH2PO4 <- 0.02021
c.H3PO4 <- 0.0
pKa <- c(2.148, 7.199, 12.15)
carga <- c(0, -1, -2, -3)
c.Total <- c.H3PO4 + c.KH2PO4 + c.K2HPO4 + c.K3PO4
c.K <- 1 * c.KH2PO4 + 2 * c.K2HPO4 + 3 * c.K3PO4
pH <- uniroot(calculo.pH, interval = c(0, 14), tol = 1e-14)$root
pH

## -------------------------------------------------------------------------------------------------------------
(2.148 + 7.199) / 2

## -------------------------------------------------------------------------------------------------------------
pKa <- c(3.128, 4.761, 6.396)
a <- my.plot(pKa)
a

## -------------------------------------------------------------------------------------------------------------
c.H2Ft <- 0.00
c.KHFt <- 0.0
c.K2HFt <- 0.0824
c.H3Cit <- 0.0
pKa <- c(3.128, 4.761, 6.396)
carga <- c(0, -1, -2, -3)
c.Total <- c.H3Cit + c.KH2Cit + c.K2HCit + c.K3Cit
c.K <- 1 * c.KH2Cit + 2 * c.K2HCit + 3 * c.K3Cit
pH <- uniroot(calculo.pH, interval = c(0, 14), tol = 1e-14)$root
pH


## -------------------------------------------------------------------------------------------------------------
c.K2CO3 <- 0.0
c.KHCO3 <- 0.0824
c.H2CO3 <- 0.0
pKa <- c(6.352, 10.329)
carga <- c(0, -1, -2)
c.Total <- c.H2CO3 + c.KHCO3 + c.K2CO3 
c.K <- 1 * c.KHCO3 + 2 * c.K2CO3
pH <- uniroot(calculo.pH, interval = c(0, 14), tol = 1e-14)$root
pH

## -------------------------------------------------------------------------------------------------------------
pKa <- c(6.352, 10.329)
a <- my.plot(pKa)
a

## -------------------------------------------------------------------------------------------------------------
(6.352 + 10.329) / 2


## -------------------------------------------------------------------------------------------------------------
c.en <- 0.0100
pKa <- c(6.848, 9.928)
carga <- c(+2, +1, 0)
c.Total <- c.en
c.K <- 0
pH <- uniroot(calculo.pH, interval = c(0, 14), tol = 1e-14)$root
pH


## -------------------------------------------------------------------------------------------------------------
calculo.pH <- function(pH){
  H3O <- 10^-pH
  HO <- 1e-14 / H3O
  a.HnA <- alp(pH, pKa)
  HnA <- c.Total * a.HnA
  K <- c.K
  Cl <- c.Cl
  H3O + K + sum(carga * HnA) - Cl - HO
} 

## -------------------------------------------------------------------------------------------------------------
c.en <- 0
c.HenCl <- 0.0100
c.H2enCl2 <- 0.0100
pKa <- c(6.848, 9.928)
carga <- c(+2, +1, 0)
c.Total <- c.en + c.HenCl + c.H2enCl2
c.K <- 0
c.Cl <- c.HenCl + 2 * c.H2enCl2
pH <- uniroot(calculo.pH, interval = c(0, 14), tol = 1e-14)$root
pH

## -------------------------------------------------------------------------------------------------------------
(6.848 + 9.928) / 2

## -------------------------------------------------------------------------------------------------------------
pKa <- c(2.50, 9.778)
pH <- seq(0, 14, by = 0.01)
a <- my.plot(pKa)
a

