alp  <- function(pH, pKa){
  n  <- length(pKa)
  numerator  <- 10^(-pH * n:0 - cumsum(c(0, pKa)))
  numerator /  sum(numerator)
}
pKa <- c(-2.2)
c.Total <- 0.00840
c.NaOH <- 0.02
V.A <- 10
carga <- c(0, -1)
cal.pH <- function(pH, V.T){
  H3O <- 10^-pH
  HO <- 1e-14 / H3O
  a.HnA <- alp(pH, pKa)
  HnA <- a.HnA * c.Total * V.A / (V.A + V.T)
  Na <- c.NaOH * V.T / (V.A + V.T)
  H3O + Na + sum(carga * HnA) - HO
}
V.T <- seq(0, 100, by = 0.01)
pH <- sapply(V.T, function(x)
  uniroot(cal.pH, V.T = x, interval = c(0, 14), tol = 1e-14)$root)
df <- data.frame(x = V.T, y = pH)

myplot <- function(df){
  p <- ggplot(df) +
    aes(x = x, y = y) +
    geom_line(size = 1, colour = "#d95f02") +
    scale_y_continuous("pH", 
                       breaks = seq(2, 12, by = 1)) +
    scale_x_continuous("Volumen NaOH (mL)", 
                       breaks = seq(0, 40, by = 5)) +
    coord_cartesian(xlim = c(0, 40), ylim = c(2, 12)) +
    theme_bw() + 
    theme(text = element_text(size = 14))
  return(p)}

myplot(df)
## H3PO4 + NaOH
pKa <- c(2.12, 7.21, 12.67)
c.Total <- 0.01120
c.NaOH <- 0.02
V.A <- 10
carga <- c(0, -1, -2, -3)
cal.pH <- function(pH, V.T){
  H3O <- 10^-pH
  HO <- 1e-14 / H3O
  a.HnA <- alp(pH, pKa)
  HnA <- a.HnA * c.Total * V.A / (V.A + V.T)
  Na <- c.NaOH * V.T / (V.A + V.T)
  H3O + Na + sum(carga * HnA) - HO
}
V.T <- seq(0, 100, by = 0.01)
pH <- sapply(V.T, function(x)
  uniroot(cal.pH, V.T = x, interval = c(0, 14), tol = 1e-14)$root)
df <- data.frame(x = V.T, y = pH)
myplot(df)

## KH2PO4 + NaOH
pKa <- c(2.12, 7.21, 12.67)
c.Total <- 0.01120
c.NaOH <- 0.02
V.A <- 10
carga <- c(0, -1, -2, -3)
cal.pH <- function(pH, V.T){
  H3O <- 10^-pH
  HO <- 1e-14 / H3O
  a.HnA <- alp(pH, pKa)
  HnA <- a.HnA * c.Total * V.A / (V.A + V.T)
  K <-  c.Total 
  Na <- c.NaOH * V.T / (V.A + V.T)
  H3O + Na + K + sum(carga * HnA) - HO
}
V.T <- seq(0, 100, by = 0.01)
pH <- sapply(V.T, function(x)
  uniroot(cal.pH, V.T = x, interval = c(0, 14), tol = 1e-14)$root)
df <- data.frame(x = V.T, y = pH)
myplot(df)

## H3PO4 + HCl + NaOH
pKa <- c(2.12, 7.21, 12.67)
c.Total <- 0.01120
C.HCl <- 0.00840
c.NaOH <- 0.02
V.A <- 10
carga <- c(0, -1, -2, -3)
cal.pH <- function(pH, V.T){
  H3O <- 10^-pH
  HO <- 1e-14 / H3O
  a.HnA <- alp(pH, pKa)
  HnA <- a.HnA * c.Total * V.A / (V.A + V.T)
  Cl <- C.HCl * V.A / (V.A + V.T)
  Na <- c.NaOH * V.T / (V.A + V.T)
  H3O + Na + sum(carga * HnA) - HO - Cl
}
V.T <- seq(0, 100, by = 0.01)
pH <- sapply(V.T, function(x)
  uniroot(cal.pH, V.T = x, interval = c(0, 14), tol = 1e-14)$root)
df <- data.frame(x = V.T, y = pH)
myplot(df)
## KH2PO4 + H3PO4 + NaOH
pKa <- c(2.12, 7.21, 12.67)
pKa.2 <- c(2.12, 7.21, 12.67)
c.Total <- 0.01120
C.KH2PO4 <- 0.01120
c.NaOH <- 0.02
V.A <- 10
carga <- c(0, -1, -2, -3)
cal.pH <- function(pH, V.T){
  H3O <- 10^-pH
  HO <- 1e-14 / H3O
  a.KH2PO4 <- alp(pH, pKa.2)
  a.HnA <- alp(pH, pKa)
  H2PO4 <- a.KH2PO4 * C.KH2PO4  * V.A / (V.A + V.T)
  HnA <- a.HnA * c.Total * V.A / (V.A + V.T)
  K <-  c.Total 
  Na <- c.NaOH * V.T / (V.A + V.T)
  H3O + Na + K + sum(carga * HnA) + sum(carga * H2PO4)- HO
}
V.T <- seq(0, 100, by = 0.01)
pH <- sapply(V.T, function(x)
  uniroot(cal.pH, V.T = x, interval = c(0, 14), tol = 1e-14)$root)
df <- data.frame(x = V.T, y = pH)
myplot(df)
## 1 parcial
c.HnA <- 0.335
pKa <- 4.919
pKa.NH4 <- 9.24
Ka <- 10^-pKa
Ka.NH4 <- 10^-pKa.NH4
c.NH3 <- 0.308
V.A <- 20
V.H2O <- 50
cal.curva <- function(pH, V.T){
  H3O <- 10^-pH
  HO <- 1e-14 / H3O
  V.Total <- V.A + V.T + V.H2O
  a.HnA <- Ka / (H3O + Ka)
  A <- (c.HnA * a.HnA * V.A) / V.Total
  a.NH4 <- H3O / (Ka.NH4 + H3O)
  NH4 <- (c.NH3 * a.NH4 * V.T) / V.Total
  H3O - A + NH4 - HO
}
V.T <- seq(0, 100, by = 0.01)
pH <- sapply(V.T, function(x)
  uniroot(cal.curva, V.T = x, interval = c(0, 14), tol = 1e-14)$root)
df <- data.frame(x = V.T, y = pH)
myplot(df)

p.d <- gradient(df$y, h1 = df$x)
s.d <- gradient(p.d, h1 = df$x)
pf <- data.frame(df, p.d, s.d)
names(pf) <- c("x", "pH", "Primera derivada", "Segunda derivada")

pf <- pf %>% 
  pivot_longer(names_to = "key", values_to = "y", -x)

p <- ggplot(pf) +
  aes(x = x, y = y, colour = key) +
  geom_line() +
  geom_point() +
  scale_color_brewer("Analito", 
                     palette = "Set1") +
  labs(x = "Volumen de titulante", y = "pH") +
  theme_bw() + 
  theme(legend.text.align = 0,
        text = element_text(size = 12),
        legend.position = "none",
        legend.background = element_blank()) +
  facet_wrap(~key, scales="free_y",
             nrow = 3,
             strip.position = "left")
p
ggplotly(p)

x.1 <- c(21.73, 21.77)
y.1 = s.d[df$x == x.1]
inter.1 <- data.frame(x = x.1, y = y.1)
V.eq.1 <- predict(lm(x~y, inter.1), data.frame(y = 0))
V.eq.1
## el v.eq es aprox 21.75 mL