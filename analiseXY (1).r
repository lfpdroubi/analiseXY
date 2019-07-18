library(psych)
library(circular)
library(readxl)
library(fBasics)   # Rmetrics - Markets and Basic Statistics
library(randtests) # Several non parametric randomness tests for numeric sequences

# HISTOGRAM OF ERRORS) ----
# Histograms are presented
# Graphic approach
QCoH_HISTO_G <- function(errorespos){
  par(mfrow=c(1,2))
  # X component
  hist(errorespos[,1], main="X")
  # Y Component
  hist(errorespos[,2], main="Y")
}

# RANDOMNESS ----
# The library "randtests" have to be availabe
# Randomness is analyzed for each coordinate component
QCoH_RANDOMNESS <- function(errorespos){
  # X component
  s1 <- runs.test(errorespos[,1])
  print("El resultado del test de aleaoriedad para X: ")
  print(s1)  
  # Y component
  s2 <- runs.test(errorespos[,2])
  print("El resultado del test de aleaoriedad para Y: ")
  print(s2)  
}

# OUTLIERS ----
# Establish the K value
QCoH_OUTLIERS <- function(errorespos, ks=3) {
  if(missing(ks)) {ks<-3}
  Z.residual<-(scale(errorespos))
  # Number of errorespos outside the interval
  cx <- length(Z.residual[Z.residual[,1]>ks,1])
  cy <- length(Z.residual[Z.residual[,2]>ks,2])
  print(paste("El número de casos fuera de rango en X es: ", as.character(cx), sep=" "))
  print(paste("El número de casos fuera de rango en Y es: ", as.character(cy), sep=" "))
  
  # Boxplot representation
  boxplot(errorespos, notch=F)
}

# NORMALITY OF ERRORS) ----
# The library "fBasics" have to be availabe

# Graphic approach
QCoH_NORMALITY_G <- function(errorespos){
  # A 1 x 2 QQ-Plot display
  par(mfrow=c(1,2))
  # X component
  qqnorm(errorespos[,1], main="X")
  qqline(errorespos[,1], col=4)
  # Y Component
  qqnorm(errorespos[,2], main="Y")
  qqline(errorespos[,2], col=4)
}

# Analytic approach

# The Kolmogorov-Smirnov normality test
QCoH_NORMALITY_A_KS <- function(errorespos){
  s1 <- ksnormTest(errorespos[,1], title = "Normality test", description = "X coordinate")
  s2 <- ksnormTest(errorespos[,2], title = "Normality test", description = "Y coordinate")
  print(s1)  
  print(s2)  
}
  
# The general K-S test for one- or two-sample  
QCoH_NORMALITY_A_GKS <- function(errorespos){
  s1 <- ks.test(errorespos[1], "pnorm",0,1) # two-sided, exact
  s2 <- ks.test(errorespos[2], "pnorm",0,1) # two-sided, exact
  print(s1)  
  print(s2)  
}

# Shapiro Wilk Normality Test
QCoH_NORMALITY_A_SHA <- function(errorespos){
  s1 <- shapiroTest(errorespos[,1], title = NULL, description = NULL)
  s2 <- shapiroTest(errorespos[,2], title = NULL, description = NULL)
  print(s1)  
  print(s2)  
}


# Correlation) ----
# Analysis of the correlation

# Graphic approach
QCoH_CORRELATION_G <- function(errorespos){
  par(mfrow=c(1,1))
  plot (errorespos[,1], errorespos[,2], col=4)
  lm.out <- lm(errorespos$E_X ~ errorespos$E_Y, data=errorespos)
  abline(lm.out, col="red")
}

# Analytic approach
# Computation of the correlation between variables
QCoH_CORRELATION_A <- function(errorespos){
  R<-cor(errorespos)
  print(R)
}
QCoH_CORRELATION_A_SPR <- function(errorespos){
# Correlation test 
  cor.test(errorespos[,1], errorespos[,2], method="spearman")
}


# Homocedasticity ----
# Analysis of the homocedasticity
# Analytic approach
QCoH_HOMOCEDAS_BAR <- function(errorespos){
  varianceTest(errorespos[,1], errorespos[,2], method = "bartlett", title = NULL, description = NULL)
}

QCoH_HOMOCEDAS_VARF <- function(errorespos){
  varianceTest(errorespos[,1], errorespos[,2], method = "varf", title = NULL, description = NULL)
}

QCoH_HOMOCEDAS_FLIG <- function(errorespos){
  varianceTest(errorespos[,1], errorespos[,2], method = "fligner", title = NULL, description = NULL)
}



# Cargamos el fichero de datos
# Debe tener una estructura igual a este, es decir, que las coordenadas X e Y estén en las columnas 3 y 5 (la X) y
# 4 y 6 (la Y)
# Nombre del fichero de datos
fichero <- "E:/A_Trabajar_Proyectos_Activos/2018_Estancia_MarianaDaSosa/Programa_R_AnálisisEstadistico/IPGH_PAT_2018_Datos_Chile_ExPosicional.txt"

puncontrol<-read.csv(fichero, header=TRUE, sep=";", dec=".")
puncontrol


# Calculamos los errores en X e Y 
puncontrol[,7] <- puncontrol[,5]- puncontrol[,3]
puncontrol[,8] <- puncontrol[,6]- puncontrol[,4]



basicStats(puncontrol[,7])
basicStats(puncontrol[,8])

hist(puncontrol[,7], main="X", xlab="Errores en X", ylab="Frecuencia")
hist(puncontrol[,8], main="Y", xlab="Errores en Y", ylab="Frecuencia")


plot(puncontrol[,3], puncontrol[,4] , main="Distribución  espacial de los puntos de evaluación", xlab="X", ylab="Y")


# Cambia este factor según necesites
fescala <- 1000


plot(puncontrol[,3], puncontrol[,4], main="Campo de errores ", xlab="X", ylab="Y")
arrows(puncontrol[,3], puncontrol[,4],puncontrol[,3]+ fescala*puncontrol[,7],puncontrol[,4]+fescala* puncontrol[,8], col= 'dark red', length = 0.1, angle = 15)


fescalaCir <- 2
datos_cir2d <- circular(atan2(puncontrol[,8], puncontrol[,7]))
modulo2d <- sqrt(puncontrol[,7]^2+puncontrol[,8]^2)
plot.circular(datos_cir2d)
title(main="Distribución circular de erores", xlab="X", ylab="Y")
segments(0, 0, fescalaCir*puncontrol[,7],fescalaCir* puncontrol[,8], col= 'dark red')



QCoH_RANDOMNESS(puncontrol[c(7,8)])
QCoH_OUTLIERS(puncontrol[c(7,8)])
QCoH_NORMALITY_G(puncontrol[c(7,8)])
QCoH_NORMALITY_A_KS(puncontrol[c(7,8)])
QCoH_HOMOCEDAS_BAR(puncontrol[c(7,8)])
QCoH_CORRELATION_G(puncontrol[c(7,8)])
QCoH_CORRELATION_A(puncontrol[c(7,8)])
QCoH_CORRELATION_A_SPR(puncontrol[c(7,8)])

