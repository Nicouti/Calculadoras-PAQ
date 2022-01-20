
---
{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("nleqslv")) install.packages("nleqslv")
```

$\require{mhchem}$

# Clase 2021-11-18
En esta clase estudiamos como preparar soluciones reguladors de pH utilizando ácidos y bases polipróticas.

## Función para calcular el $\alpha$ para cada especie



```{r}
alp  <- function(pH, pKa){
  n  <- length(pKa)
  numerator  <- 10^(-pH * n:0 - cumsum(c(0, pKa)))
  numerator /  sum(numerator)
}

## Ejercicio
> Preparar una disolución reguladora con el sistema de fosfatos de pH 7.00 con una capacidad regudora frente a bases igual a 0.0500 mol/L.

Como el ejercicio define el sistema de fosfatos automáticamente se definen las constantes de acidez ($pK_a$) y la carga de cada una de las especies ($\ce{H3PO4} = 0$; $\ce{H2PO4-} = -1$; $\ce{HPO4^{2-}} = -2$; $\ce{PO4^3-} = -3$).
{r}
pKa <- c(2.148, 7.199, 12.15)
carga <- c(0, -1, -2, -3)
Escribimos los valores el balance de cargas a pH = 7.00
$$
[\ce{H3O+}] + [\ce{K+}] - [\ce{H2PO4-}] - [\ce{HPO4^2-}] -  [\ce{PO4^3-}] - [\ce{HO-}] = 0
$$
Los términos que involucran los iones provenientes del $\ce{H3PO4}$.

$$
  -[\ce{H2PO4-}] - [\ce{HPO4^2-}] -  [\ce{PO4^3-}] = c_{Total}\times \left(-\alpha_1  - 2\times \alpha_2 - 3\times \alpha_3 \right) 
$$

### Iniciamos el calculo a pH = 7.00

Al tener el valor del pH podemos calcular los valores de $\alpha$ para cada especie
```{r}
pH <- 7
a.HnA <- alp(pH, pKa)
a.HnA
```
al multiplicar los valores de $\alpha$ por la carga y sumarlos obtenemos el termino que acompaña a $c_{Total}$ en la ecuación \@ref(eq:fosfatos).
```{r}
sum.a <- sum(a.HnA * carga)
sum.a
```

y también los valores de $[\ce{H3O+}]$ y $[\ce{HO-}]$
```{r}
H3O <- 10^-pH
HO <- 1e-14 / H3O
```
La $[\ce{K+}] = c_K$ y de esta forma obtenemos una ecuación con dos incógnitas:

$$`r H3O` + c_K `r sum.a`\times c_{total} - `r HO` = 0$$

$$
c_K `r sum.a`\times c_{total} = 0
$$

### Ahora a pH = 8.00
```{r}
pH <- 8
sum.a.8 <- sum(alp(pH, pKa) * carga)
sum.a.8
```
$$
[\ce{H3O+}] + [\ce{K+}] - [\ce{H2PO4-}] - [\ce{HPO4^2-}] -  [\ce{PO4^3-}] - [\ce{HO-}] + \color{red}{BC_B} = 0
$$

$$`r 10^-pH` + c_K `r sum.a.8`\times c_{total} - `r 10^(-14 + pH)` + \color{red}{0.05}= 0$$
$$
c_K `r sum.a.8`\times c_{total} = `r 10^(-14 + pH) - 10^-pH - 0.05`
$$

### Solución del sistema de ecuaciones
$$
\begin{array}{ll}
c_K `r sum.a`\times c_{total} & = `r 10^(-14 + 7) - 10^-7`\\
c_K `r sum.a.8`\times c_{total} & = `r 10^(-14 + 8) - 10^-8 - 0.05`\\
\end{array}
$$

$$
\left[\begin{array}{cc}
1 & `r sum.a`\\
1 & `r sum.a.8`\\
\end{array}
\right]\left[\begin{array}{c}
c_K\\
c_{Total}\\
\end{array}
\right] = 
\left[\begin{array}{c}
`r 10^(-14 + 7) - 10^-7`\\
`r 10^(-14 + 8) - 10^-8 - 0.05`\\
\end{array}
\right]
$$

```{r}
A <- matrix(c(1, 1, sum.a, sum.a.8), ncol = 2)
B <- c(- 10^-7 + 10^(-14+7), -0.05 - 10^-8 + 10^(-14+8))
resp <- solve(A,B)
resp
```
$$\begin{array}{c}
c_K = `r resp[1]` \text{ mol/L}\\
c_{Total} = `r resp[2]` \text{ mol/L}\\
\end{array}$$

### Seleción de reactivos

De acuerdo con las siguientes opciones seleccionamos una.

| Opción No. | Reactivo A | Reactivo B |
|:----------:|:----------:|:----------:|
|  1  | $\ce{HCl}$ | $\ce{K2HPO4}$ |
|  2  | $\ce{HCl}$ | $\ce{K3PO4}$ |
|  3  | $\ce{H3PO4}$ | $\ce{K2HPO4}$ |
|  4  | $\ce{H3PO4}$ | $\ce{K3PO4}$ |
|  5  | $\ce{H3PO4}$ | $\ce{KOH}$ |
|  6  | $\ce{KH2PO4}$ | $\ce{K2HPO4}$ |
|  7  | $\ce{KH2PO4}$ | $\ce{K3PO4}$ |
|  8  | $\ce{KH2PO4}$ | $\ce{KOH}$ |

Al escojer la opción 7, entonces debemos calcular las concentraciones de $\ce{KH2PO4}$ y $\ce{K3PO4}$, entonces:

$$
\begin{array}{rcl}
c_{K} = & `r resp[1]` & = c_{\ce{KH2PO4}} + 3\times c_{\ce{K3PO4}} \\
c_{Total} = & `r resp[2]` & = c_{\ce{KH2PO4}} + c_{\ce{K3PO4}} \\
\end{array}
$$
$$
\left[\begin{array}{cc}
1 & 3\\
1 & 1\\
\end{array}
\right]\left[\begin{array}{c}
c_{\ce{KH2PO4}}\\
c_{\ce{K3PO4}}\\
\end{array}
\right] = 
\left[\begin{array}{c}
`r resp[1]`\\
`r resp[2]`\\
\end{array}
\right]
$$

```{r}
A <- matrix(c(1, 1, 3, 1), ncol = 2)
B <- resp
conc <- solve(A,B)
conc 
```
En conclusión, $c_{\ce{KH2PO4}} = `r round(conc[1], 5)` \text{ mol/L}$ y $c_{\ce{K3PO4}} = `r round(conc[2], 5)` \text{ mol/L}$ 

### Verificación del resultado

Al calcular el pH de la solución resultante al mezclar el $\ce{KH2PO4}$ y $\ce{K3PO4}$ de tal forma que las concentraciones finales sean `r round(conc[1], 5)` y `r round(conc[2], 5)` mol/L, respectivamente.
```{r}
cal.pH <- function(pH){
  H3O <- 10^-pH
  HO <- 1e-14 / H3O
  a.HnA <- alp(pH, pKa)
  HnA <- a.HnA * c.Total
  K <- c.K
  H3O + K + sum(carga * HnA) - HO
}
```

```{r}
c.KH2PO4 <- 0.08467
c.K3PO4 <- 0.02034
c.Total <- c.KH2PO4 + c.K3PO4
c.K <- 1 * c.KH2PO4 + 3 * c.K3PO4
pH <- uniroot(cal.pH, interval = c(0, 14), tol = 1e-14)$root
pH
```
## Calculando la capacidad amortiguadora

Para calcular la capacidad reguladora solo debemos usar la función para verificar el pH de la solución reguladora a un pH igual a 8.00 y 6.00; el valor absoluto de este resultado obtendremos la capacidad amortiguadora frente a bases y ácidos, respectivamente.

```{r}
BC.B <- abs(cal.pH(8.00))
BC.B
```
```{r}
BC.A <- abs(cal.pH(6.00))
BC.A
```
Esto quiere decir que:

> 1. Para un litro de solución reguladora necesitamos 0.0500 mol de base fuerte ($\ce{KOH}$ o $\ce{NaOH}$) para que la solución cambien de pH 7.00 a pH 8.00. 
> 2. Para un litro de solución reguladora necesitamos 0.0344 mol de ácido fuerte ($\ce{HCl}$ o $\ce{HNO3}$) para que la solución cambien de pH 7.00 a pH 6.00.

## Otro método para resolver el ejercicio
Este segundo método se basa en la utilización de una librería llamada __nleqslv__ para resolver este sistema de 2 ecuaciones dos incognitas utilizando directamente el balance de cargas.

# Cargamos la librer�a de la siguiente forma
```{r}
if (!require("nleqslv")) install.packages("nleqslv")
```

# Este m�todo selecciona los reactivos antes de empezar a hacer los cálculos, por lo que seleccionaremos la opción 7 de reactivos para preparar la disolución reguladora.
{r}
pKa <- c(2.148, 7.199, 12.15)
carga <- c(0, -1, -2, -3)
pH <- c(7.0, 8.0)
BC.B <- c(0, 0.0500)
H3O <- 10^-pH
HO <- 1e-14 / H3O
a.HnA <- sapply(pH, alp, pKa)
problema <- function(conc) {
  c.KH2PO4 <- conc[1]
  c.K3PO4 <- conc[2]
  c.Total <- c.KH2PO4 + c.K3PO4
  c.K <- c.KH2PO4 + 3 * c.K3PO4
  y <- H3O + c.K + colSums(carga * a.HnA) * c.Total - HO + BC.B
  y
}
conc_start <- matrix(runif(10, min=0, max=2), ncol=2)
ans <- searchZeros(conc_start, problema, control = list(ftol = 1e-14))$x
ans
```
En donde el primer valor corresponde a $c_{\ce{KH2PO4}}$ y el segundo valor a $c_{\ce{K3PO4}}$. 

Ahora seleccionaremos la opción 4 para preparar la disolución, lo único que cambia son los parámetros dentro de la función. Los valores conocidos se mantienen constantes.

```{r}
problema.2 <- function(conc) {
  c.H3PO4 <- conc[1]
  c.K3PO4 <- conc[2]
  c.Total <- c.H3PO4 + c.K3PO4
  c.K <- 3 * c.K3PO4
  y <- H3O + c.K + colSums(carga * a.HnA) * c.Total - HO + BC.B
  y
}
ans.2 <- searchZeros(conc_start, problema.2, control = list(ftol = 1e-14))$x
ans.2
```
Finalmente, seleccionaremos la opción 1 para preparar la disolución, lo único que cambia son los parámetros dentro de la función. Los valores conocidos se mantienen constantes.

```{r}
problema.3 <- function(conc) {
  c.HCl <- conc[1]
  c.K2HPO4 <- conc[2]
  c.Total <- c.K2HPO4
  c.K <- 2 * c.K2HPO4
  y <- H3O + c.K + colSums(carga * a.HnA) * c.Total - HO  - c.HCl + BC.B
  y
}
ans.3 <- searchZeros(conc_start, problema.3, control = list(ftol = 1e-14))$x
ans.3
```
## Ecuación de la capacidad reguladora

$$BC_{\ce{A}} = 9\times[\ce{H3O+}] + c_{Total} \frac{\displaystyle \sum_{i = 0}^{n-1}\left( \sum_{j = i+1}^{n} (j-i)\times (10^{-i}-10^{-j})\alpha_i\alpha_j\right) }{\displaystyle \sum_{k = 0}^{n} 10^{-k}\alpha_k} + 0.9\times [\ce{HO^-}]
		$$
$$BC_{\text{B}} =  {0.9}\times[\ce{H3O+}] + c_{Total} \frac{\displaystyle \sum_{i = 0}^{n-1}\left( \sum_{j = i+1}^{n} (i-j)\times (10^{i}-10^{j})\alpha_i\alpha_j\right) }{\displaystyle \sum_{k = 0}^{n} 10^{k}\alpha_k} + {9}\times [\ce{HO^-}]$$

{r}
pKa <- c(2.148, 7.199, 12.15)
c.Total <- 0.100
BC <- function(pH){
  H3O <- 10^-pH
  HO  <- 1e-14 / H3O
  n <- length(pKa)
  numerator <- 10^(-pH * n:0 - cumsum(c(0, pKa)))
  a.HnA <- numerator /  sum(numerator)
  i.j <- t(combn(0:n, 2))
  i <- i.j[,1]
  j <- i.j[,2]
  A.HnA <- sum((j-i) * (10^-i - 10^-j) * a.HnA[i+1] * a.HnA[j+1]) / sum(10^(-(0:n))*a.HnA)
  B.HnA <- sum((i-j) * (10^i - 10^j) * a.HnA[i+1] * a.HnA[j+1]) / sum(10^((0:n))*a.HnA)
  BC.A <- c.Total * A.HnA
  BC.B <- c.Total * B.HnA
  c(BC.A, BC.B)
}
pH <- seq(0, 14, by = 0.01)
BC.A.B <- data.frame(t(sapply(pH, BC)))
names(BC.A.B) <- c("BC.A", "BC.B")
df <- data.frame(x = pH, BC.A.B) %>% 
  pivot_longer(names_to = "key", values_to = "y", -x)

p <- ggplot(df) +
 aes(x = x, y = y, colour = key) +
 geom_line(size = 1L) +
 scale_color_brewer(palette = "Dark2", direction = 1) +
  scale_x_continuous(breaks = 0:14) +
 labs(title = "Capacidad reguladora sin incluir el aporte del agua",
      x = "pH", y = "Capacidad reguladora (mol/L)") +
 theme_bw() +
 ylim(0, 0.1) +
  theme(text = element_text(size = 14),
        legend.position = c(0.75,0.75))
  
p


```









