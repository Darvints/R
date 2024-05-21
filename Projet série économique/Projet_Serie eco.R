## Etude d'une série économique de NIPA

getwd() # verification dossier
list.files() # verifications fichiers 
remove(list = ls()) # nettoyage de l'environnement

#Importation des librairies
library(tseries)
library(forecast)
library(seastests)
library(astsa)
library(zoo)
library(lmtest)
library(TTR)

#Importation et  nettoyage de la bdd
data=read.csv("Table.csv",header=FALSE,sep=",",dec=".",skip=3)
data <- data[,-1]
c <- c(2,13,14,15,16,17)
data <- data[-c,]
data[1,1] = "Year"
rownames(data) <- data[,1]
data <- data[,-1]
colnames(data)<-data[1,]
data <- data[-1,]
data <- t(data)
c = colnames(data)
data <- data.frame(data)
level_ts=ts(data$X........Net.domestic.product,start=c(1929,1),end=c(2022,1),frequency=1)
level_ts
plot.ts(level_ts,xlab="Time",ylab=" série temporelle en niveau",main = "Produit net domestic selon le temps") # rajouter les memes arguments que la fonction plot 

## On se demande si la serie  est stationnaire ?
acf(level_ts,lag.max=20)
pacf(level_ts,lag.max=20)
adf.test(level_ts, alternative="stationary",k=0) 
##=> Le série est non stationnaire : Hypothese alternative : stationnarité, p-value = 0.99
## donc Hypothese alternative rejetté

#On regarde le différentiel logarithmique de notre série temporelle

growth_ts=diff(log(level_ts))
growth_ts
plot(growth_ts,xlab = "temps", ylab = "Rendements", main = "TS:Rendements selon temps")
acf(growth_ts,lag.max=20,main = "ACF des rendements")
pacf(growth_ts,lag.max=20,main = "PACF des rendements")
adf.test(growth_ts,k=0)
##=> P-value = 0.01 , donc l'hypothese alternative est validé
##=> Série stationnaire

##pmax = 2 qmax = 2
##on va donc modéliser un  ARMA(2,2)

#############################################
# 1ère  méthode Box and Jenkins  
#############################################

## on utilise la fonction arima pour estimer les differents modèles
## cons SARIMA (p,1,q) ==> sa_dc ARIMA(p,0,q)

model1=arima(growth_ts,order = c(2,0,2))
summary(model1)
## critère d'informations 
AIC(model1)
BIC(model1)
## coefficients estimés et variance 
model1$coef
model1$var.coef


#############################################
# 2ere méthode Box and Jenkins automatique 
#############################################
bestmodel=auto.arima(growth_ts,max.d=0,max.p=2,max.q=2,allowdrift = FALSE,seasonal = FALSE) ## methode Box and Jenkin
## cette commande permet d'estimer tous les modèles arma(p,q) avec p inférieur ou egal à pmax
## et q inférieur ou égale à qmax 
## et de selectionner le modèle qui minimise le critère d'information 
summary(bestmodel)
# Vérification // erreurs 
residus <- bestmodel$residuals 
plot.ts(residus,xlab = "temps",main = "Résidus au cours du temps")
acf(residus,lag.max=20,main = "ACF des résidus")
pacf(residus,lag.max=20,main = "PACF des résidus")
hist(residus,breaks = 20,prob=TRUE,col = "cornflowerblue",main = "Fonction de densité estimée",xlab = "Résidu estimé",ylab = "Densité")
lines(density(residus, na.rm = "TRUE"),lwd=2,col="orange")


# autocorrelation test 
Box.test(residus, lag = 20, type = c("Ljung-Box"), fitdf = 0)

# Homoscédasticité
## Test de Breush-Pagan
a <- bestmodel$fitted
l <- lm(residus ~ lag(lag(lag(lag(lag(growth_ts))))) + lag(lag(residus)) + lag(lag(lag(lag(residus))))+lag(lag(lag(lag(lag(residus))))))
bptest(l)

## test de normalité des résidus 
## Test de Jarque Bera
jarque.bera.test(residus)

#########  Les prévisions
previsions <- forecast(bestmodel, h = 4)
previsions
plot(previsions)

