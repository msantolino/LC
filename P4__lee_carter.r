setwd("C:/Users/msantolino/Desktop/lab4")

#install.packages('demography')
library(demography)

#Directamente de la web a R
spain <- hmd.mx("ESP", 'login', 'password', "Spain")

#Apartado 1
#Bajando previamente el *.txt
spain<-read.demogdata("Mx_1x1.txt", "Exposures_1x1.txt", type="mortality", label="Spain", skip = 2, popskip = 2)

#Apartado 2
#Gr�fico de las tasas brutas de mortalidad por sexo
par(mfrow=c(1,2),las=1)
plot(spain,series="male", plot.type="functions")
plot(spain,series="female", plot.type="functions")

#Apartado 3
#Seleccionando tasas de 1975 a  2018
spain.desde1975 <- extract.years(spain,1975:2018)

#Apartado 4
#Modelo Lee Carter  
spain.desde1975.LC<-lca(spain.desde1975, series="male",adjust="dt", max.age=105) 

names(spain.desde1975.LC)
#"label" "age" "year" "male" "ax" "bx" "kt" "residuals" "fitted" "varprop" "y" "mdev" "call" "adjust" "type"  

#Apartado 5.
#Par�metros  estimados
spain.desde1975.LC$ax
spain.desde1975.LC$bx
spain.desde1975.LC$kt


#Gr�fico de los par�metros estimados
plot(spain.desde1975 ,series="male", plot.type="functions")

op<-par(mfrow=c(3,1), las=1)
 plot(spain.desde1975.LC$age, spain.desde1975.LC$ax, xlab='Age (x)', ylab='Beta 1(x)', pch=19)
 plot(spain.desde1975.LC$age, spain.desde1975.LC$bx, xlab='Age (x)', ylab='Beta 2(x)', pch=19)
 plot(spain.desde1975.LC$year, spain.desde1975.LC$kt, xlab='Years (t)', ylab='Kappa 2 (t)', pch=19)

par(op)
 mtext('Males', side=3, line=2, font=2) 


#Apartado 6
#Tasas estimadas y residuales
spain.desde1975.LC$fitted$y
spain.desde1975.LC$residuals$y 

  plot(residuals(spain.desde1975.LC), type="fts")
 plot(residuals(spain.desde1975.LC), type="filled.contour")
  plot(fitted(spain.desde1975.LC))



#Gr�fico de las Tasas Estimadas
persp(spain.desde1975.LC$age, spain.desde1975.LC$year, spain.desde1975.LC$fitted$y, , phi=30, theta=-30, col ="chartreuse2",
      main=paste('Log Mortality rates LC, Males', 1975 , '-', 2018),
      xlab=paste('Age',0,'-',105), ylab=paste('Years',1975,'-',2018), zlab='Log Mortality rate')

#Apartado 7
#Forecasts hasta 2120 
spain.desde1975.LC.fcast<-forecast(spain.desde1975.LC, h=110, level=95)

#Grafico de las proyecciones
par( las=1)
plot(spain.desde1975.LC.fcast)

#Apartado 8
#Edad media de fallecimiento al nacer y a los 65 a�os
e0.LC.fcast<-e0(spain.desde1975.LC.fcast, PI=F, nsim=100, type='period')
e0.LC.fcast2<-e0(spain.desde1975.LC.fcast, PI=F, nsim=100, type='cohort')

e65.LC.fcast<-flife.expectancy(spain.desde1975.LC.fcast, PI=F, nsim=100, age=65)

par(mfrow=c(1,2), las=1)
plot(e0.LC.fcast, main='Forecast Life expectancy, age 0')
plot(e0.LC.fcast2, main='Forecast Life expectancy, age 0')
plot(e65.LC.fcast, main='Forecast Life expectancy, age 65')


#esperanza de vida

pxH_0_105<-exp(-spain.desde1975.LC.fcast$rate$male)

pxH_0_105<-(2-spain.desde1975.LC.fcast$rate$male)/(2+spain.desde1975.LC.fcast$rate$male)


#edad media de fallecimiento 2019
sum(cumprod(pxH_0_105[,1]))+0.5
e0.LC.fcast$mean[1]

#edad media de fallecimiento 2019 edad 65 a�os
sum(cumprod(pxH_0_105[66:106,102]))+0.5


#esperanza de vida de un hombre 65 a�os en a�o 2019
diag(pxH_0_105[66:106,])
sum(cumprod(diag(pxH_0_105[66:106,])))+0.5

sum(cumprod(diag(pxH_0_105)))+0.5
e0.LC.fcast2$mean[44]





