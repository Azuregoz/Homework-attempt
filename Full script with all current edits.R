library(readxl)
Datos6 <- read_excel("Datos.xlsx", sheet = "6", 
                     na = "-99")
install.packages("quantmod")
library(quantmod)

BimboRS<-ROC(Datos6$Bimbo, type = "discrete")
SP500RS2<-ROC(Datos6$SP500, type = "discrete")
DatosC<-data.frame(BimboRS,SP500RS2)

plot(DatosC$SP500RS2~DatosC$BimboRS)

cor.test(DatosC$SP500RS2,DatosC$BimboRS)

cor.test1<-cor.test(DatosC$SP500RS2,DatosC$BimboRS)

## Inspect your structure here:
str(cor.test1)
## You see that the value you want, is probably the statistic one. So we extract that.
corr_stat <- cor.test1$statistic

#Fewer repetitions and legible code. 
corr_stat_abs <- abs(corr_stat)

#Legibility & removed upper limits. 
#Else if already means that everything above 0.80 is captured, so there's no need 
#To include it as an upper bound. 

if ((1 > corr_stat_abs) &
    (corr_stat_abs > 0.8)) {
  print("Very strong correlation")
} else if (0.60 < corr_stat_abs) {
  print("Strong correlation")
} else if (0.40 < corr_stat_abs) {
  print("Moderate correlation")
} else if (0.20 < corr_stat_abs) {
  print("Weak correlation")
} else if (0 < corr_stat_abs) {
  print("Very weak correlation")
} else if (1 == corr_stat_abs) {
  print("Perfect correlation")
} else if (0 == corr_stat_abs) {
  print("No correlation")
}

# Pedantic note here, if the correlation coefficient is 0 that does not mean there is 
# no correlation. That statement is one-directional. 

if (0 < corr_stat) {
  print("Positive correlation")
} else if ((0 > corr_stat)) {
  print("Negative correlation")
} else if ((0 == corr_stat)) {
  print("No correlation")
}

alpha<-0.05
pvalue<-cor.test1$p.value


if (pvalue>alpha) {
  print("reject H0")} else {
  print("accept H0")
}

