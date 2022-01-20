storeWarn<- getOption("warn")
options(warn = -1)
options(repr.plot.width=8, repr.plot.height=6)
if (!require("pracma")) install.packages("pracma")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("plotly")) install.packages("plotly")
if (!require("nleqslv")) install.packages("nleqslv")

library(RColorBrewer)

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
## Con ayuda del data frame encontraremos el pH de el ácido para ciertos volúmenes
# a
V.eq.a <- V.eq.1 * 0
V.T <- V.eq.a
pH.a <- sapply(V.T, function(x)
  uniroot(cal.curva, V.T = x, interval = c(0, 14), tol = 1e-14)$root)
pH.a
## pH = 2.971448
# b 
V.eq.b <- V.eq.1 * 0.25
V.eq.b
V.T <- V.eq.b
pH.b <- sapply(V.T, function(x)
  uniroot(cal.curva, V.T = x, interval = c(0, 14), tol = 1e-14)$root)
pH.b
## pH = 4.442724
# c
V.eq.c <- V.eq.1 * 0.5
V.eq.c
V.T <- V.eq.c
pH.c <- sapply(V.T, function(x)
  uniroot(cal.curva, V.T = x, interval = c(0, 14), tol = 1e-14)$root)
pH.c
## pH = 4.919082
# d
V.eq.d <- V.eq.1 * 0.75
V.eq.d
V.T <- V.eq.d
pH.d <- sapply(V.T, function(x)
  uniroot(cal.curva, V.T = x, interval = c(0, 14), tol = 1e-14)$root)
pH.d
## pH = 5.395733
# e
V.eq.e <- V.eq.1 * 1
V.eq.e
V.T <- V.eq.e
pH.e <- sapply(V.T, function(x)
  uniroot(cal.curva, V.T = x, interval = c(0, 14), tol = 1e-14)$root)
pH.e
## pH = 7.074762
# f
V.eq.f <- V.eq.1 * 1.25
V.eq.f
V.T <- V.eq.f
pH.f <- sapply(V.T, function(x)
  uniroot(cal.curva, V.T = x, interval = c(0, 14), tol = 1e-14)$root)
pH.f
## pH = 8.637894
# g
V.eq.g <- V.eq.1 * 1.5
V.eq.g
V.T <- V.eq.g
pH.g <- sapply(V.T, function(x)
  uniroot(cal.curva, V.T = x, interval = c(0, 14), tol = 1e-14)$root)
pH.g
## pH = 8.938727
# h
V.eq.h <- V.eq.1 * 1.75
V.eq.h
V.T <- V.eq.h
pH.h <- sapply(V.T, function(x)
  uniroot(cal.curva, V.T = x, interval = c(0, 14), tol = 1e-14)$root)
pH.h
## pH = 9.114762
# i
V.eq.i <- V.eq.1 * 2
V.eq.i
V.T <- V.eq.i
pH.i <- sapply(V.T, function(x)
  uniroot(cal.curva, V.T = x, interval = c(0, 14), tol = 1e-14)$root)
pH.i
## pH = 9.239656
# j
V.eq.j <- V.eq.1 * 2.25
V.eq.j
V.T <- V.eq.j
pH.j <- sapply(V.T, function(x)
  uniroot(cal.curva, V.T = x, interval = c(0, 14), tol = 1e-14)$root)
pH.j
## pH = 9.336522

