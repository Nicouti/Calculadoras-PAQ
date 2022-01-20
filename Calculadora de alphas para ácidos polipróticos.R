storeWarn<- getOption("warn")
options(warn = -1)
options(repr.plot.width=8, repr.plot.height=6)
if (!require("pracma")) install.packages("pracma")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("plotly")) install.packages("plotly")
library(RColorBrewer)
pH = 7
pKa = 8.075
Ka = 10^-pKa
H3O = 10^-pH
a.HTRIS = H3O / (H3O+Ka)
a.HTRIS
## Valor que acompaña a x en el balance de cargas, HO = H3O por tanto se cancelan
pH = 6
pKa = 8.075
Ka = 10^-pKa
H3O = 10^-pH
a.HTRIS = H3O / (H3O+Ka)
a.HTRIS
## Valor que acompaña el balance de cargas nuevo para plantear una ecuación 2
A = matrix(c(0.9223, 0.9917, -1, -1), ncol=2)
B = c(0, 0.5)
solve (A, B)
## La solución del sistema da para x y "y" respectivamente
## donde x es la c.TOTAL y "y" es Cl

pH = 7
H3O = 10^-pH
a.0=H3O / (H3O+Ka)
a.1=Ka / (H3O+Ka)
HO = 1e-14 / H3O
c.Total = 
  
Ka1= 10^-6.352
Ka2=10^-10.329
pH = 6
H3O = 10^-pH
a.0= H3O^2/(H3O^2+Ka1 * H3O + Ka1 * Ka2)
a.1= Ka1*H3O/(H3O^2+Ka1 * H3O + Ka1 * Ka2)
a.2= Ka1*Ka2/(H3O^2+Ka1 * H3O + Ka1 * Ka2)
c(a.0, a.1, a.2)

alp = function(pH, pKa){
  n = length(pKa)
  num = 10^(-(pH*(n:0) + cumsum(c(0, pKa))))
  num / sum(num)
}
pKa = c(3.036, 4.366)
alp(5, pKa)
pKa <- c(3, 4, 5)
pH <- seq(0, 14, by = 0.01)
a.HnA <- data.frame(t(sapply(pH, alp, pKa = pKa)))
names(a.HnA) <- paste0("a.", 0:length(pKa))
pKa <- c(3.128, 4.761, 6.396)
a.HnA <- data.frame(t(alp(8.5, pKa)))
names(a.HnA) <- paste0("a.", 0:length(pKa))
a.HnA
pH <- seq(0, 14, by = 0.01)
a.HnA <- data.frame(t(sapply(pH, alp, pKa = pKa)))
names(a.HnA) <- paste0("a.", 0:length(pKa))
df <- data.frame(x = pH, a.HnA) %>%
  
  pivot_longer(names_to = "key", values_to = "y", -x)
p <- ggplot(df, aes(x = x, y = y, color = key)) +
  
  geom_line(size = 1) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_continuous("pH", breaks = 0:14) +
  scale_y_continuous("capacidad amortiguadora (mol/L)") +
  theme_bw() +
  theme(text = element_text(size = 16),
        legend.position = c(0.75, 0.75))

p

