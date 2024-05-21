#Script du projet sur les séries financières : SG et cac40 
#journalier et mensuel sur toutes la durée disponible


#chargement des librairies
install.packages(c("rugarch","rmgarch"))
install.packages("zoo")
library(tseries)
library(forecast)
library(seastests)
library(astsa)
library(zoo)
library(lmtest)
library(TTR)
library(timeDate)
library(rugarch)
library(rmgarch)
library(xts)
library(ggplot2)
remove(list = ls()) 

#Importation des données et nettoyage
GLE_day = read.csv2("GLE.PA_day.csv",sep=",")
GLE_day = GLE_day[,c(-2,-3,-4,-6,-7)]
GLE_day[,1] <- as.Date(GLE_day[,1])
GLE_day[,2] <- as.numeric(GLE_day[,2])
plot(GLE_day[,1],GLE_day[,2],type="l",xlab = "Temps", ylab = "Prix",main = "Prix au cours du temps de l'action SG")

GLE_mon = read.csv2("GLE.PA_month.csv",sep=",")
GLE_mon = GLE_mon[,c(-2,-3,-4,-6,-7)]
GLE_mon[,1] <- as.Date(GLE_mon[,1])
GLE_mon[,2] <- as.numeric(GLE_mon[,2])

cac_day = read.csv2("Cac_day.csv",sep=",")
cac_day = cac_day[,c(-2,-3,-4,-6,-7)]
cac_day[,1] <- as.Date(cac_day[,1])
cac_day[,2] <- as.numeric(cac_day[,2])

cac_mon = read.csv2("Cac_month.csv",sep=",")
cac_mon = cac_mon[,c(-2,-3,-4,-6,-7)]
cac_mon[,1] <- as.Date(cac_mon[,1])
cac_mon[,2] <- as.numeric(cac_mon[,2])

#On modélise nos rendements
RD_GLE <- (GLE_day[2:length(GLE_day[,1]),2]-GLE_day[1:length(GLE_day[,1])-1,2])/GLE_day[1:length(GLE_day[,1])-1,2]
RM_GLE <- (GLE_mon[2:length(GLE_mon[,1]),2]-GLE_mon[1:length(GLE_mon[,1])-1,2])/GLE_mon[1:length(GLE_mon[,1])-1,2]
RD_cac <- (cac_day[2:length(cac_day[,1]),2]-cac_day[1:length(cac_day[,1])-1,2])/cac_day[1:length(cac_day[,1])-1,2]
RM_cac <- (cac_mon[2:length(cac_mon[,1]),2]-cac_mon[1:length(cac_mon[,1])-1,2])/cac_mon[1:length(cac_mon[,1])-1,2]

#COnversion en DF
Rd_gle <- data.frame(Date = GLE_day[2:length(GLE_day[,1]),1], Rendement = RD_GLE ) 
Rm_gle <- data.frame(Date = GLE_mon[2:length(GLE_mon[,1]),1], Rendement = RM_GLE)
Rd_cac <- data.frame(Date = cac_day[2:length(cac_day[,1]),1], Rendement = RD_cac)
Rm_cac <- data.frame(Date = cac_mon[2:length(cac_mon[,1]),1], Rendement = RM_cac)
Rm_gle <- Rm_gle[-length(Rm_gle[,1]),]
Rm_cac <- Rm_cac[-length(Rm_cac[,1]),]
Rd_cac <- subset(Rd_cac, !is.na(Rd_cac$Rendement))
#1)Fait stylisé
  
  #1.1) Quelques graphiques
  
#1.1.a)graphique des rendements qui ressemble à un WN
plot(Rd_gle$Date,Rd_gle$Rendement,type = "l",xlab = "Dates", ylab = "Rendement",main = "Rendement journalier de la SG")
plot(Rm_gle$Date,Rm_gle$Rendement,type = "l",xlab = "Dates", ylab = "Rendement")
plot(Rm_cac$Date,Rm_cac$Rendement,type = "l",xlab = "Dates", ylab = "Rendement")
plot(Rd_cac$Date,Rd_cac$Rendement,type = "l",xlab = "Dates", ylab = "Rendement")

#Implication : Ressemble à une série stationnaire

#1.1.b) Distribution des différents rendements
#Graphique1
hist(Rd_gle$Rendement, breaks = 100, freq = FALSE, main = "Histogramme des rendements Journalier",xlab = "Rendements SG")
curve(dnorm(x, mean = mean(Rd_gle$Rendement), sd = sd(Rd_gle$Rendement)), 
      col = "blue", add = TRUE)

#Graphique2
hist(Rm_gle$Rendement, breaks = 100, freq = FALSE, main = "Histogramme des rendements mensuels",xlab = "Rendements SG")
curve(dnorm(x, mean = mean(Rm_gle$Rendement), sd = sd(Rm_gle$Rendement)), 
      col = "blue", add = TRUE)

#Graphique3
hist(Rd_cac$Rendement, breaks = 100, freq = FALSE, main = "Histogramme des rendements Journalier",xlab = "Rendement  cac")
curve(dnorm(x, mean = mean(Rd_cac$Rendement), sd = sd(Rd_cac$Rendement)), 
      col = "blue", add = TRUE)

#Graphique4
hist(Rm_cac$Rendement, breaks = 100, freq = FALSE, main = "Histogramme des rendements mensuels",xlab = "Rendement Cac")
curve(dnorm(x, mean = mean(Rm_cac$Rendement), sd = sd(Rm_cac$Rendement)), 
      col = "blue", add = TRUE)
#Implication : Suit une loi normal mais kurtosis et skewness font que la courbe semble différé

#1.2) ACF

#1.2.a) Acf journalier et mensuel
acf(Rd_gle$Rendement,main="ACF du Rendement journalier de la SG")
acf(Rm_gle$Rendement,main="ACF du Rendement mensuel de la SG")
acf(Rd_cac$Rendement,main="ACF du Rendement journalier du Cac")
acf(Rm_cac$Rendement,main="ACF du Rendement mensuel du cac")

#1.2.b) ACF des rendements au carré
acf((Rd_gle$Rendement)**2,main = "ACF de Rt²", xlab = "SG journalier")
acf((Rm_gle$Rendement)**2,main = "ACF de Rt²", xlab = "SG mensuel")
acf((Rd_cac$Rendement)**2,main = "ACF de Rt²", xlab = "cac journalier")
acf((Rm_cac$Rendement)**2,main = "ACF de Rt²", xlab = "cac mensuel")
#On voit une certaines significativité

#1.3)Skewness et Kurtosis
skewness(Rm_gle)
skewness(Rd_gle)
skewness(Rm_cac)
skewness(Rd_cac)
kurtosis(Rd_gle$Rendement)
kurtosis(Rm_gle$Rendement)
kurtosis(Rd_cac$Rendement)
kurtosis(Rm_cac$Rendement)

#1.4 ) Stat desc de nos séries
summary(Rm_gle$Rendement)
summary(Rd_gle$Rendement)
summary(Rm_cac$Rendement)
summary((Rd_cac$Rendement))

sqrt(var(Rd_gle$Rendement))
sqrt(var(Rm_gle$Rendement))
sqrt(var(Rd_cac$Rendement))
sqrt(var(Rm_cac$Rendement))

#2) Modèles MEDAF
#Mise à la même longueur des Rendements
Rd_gle <- Rd_gle[Rd_gle$Date %in% Rd_cac$Date,]
Rd_cac <- Rd_cac[Rd_cac$Date %in% Rd_gle$Date,]

cor(Rd_gle$Rendement,Rd_cac$Rendement)

modele <- lm(Rd_gle$Rendement~ Rd_cac$Rendement)
summary(modele)

#Test de la moyenne
t.test(Rd_gle$Rendement,mu=0)
t.test(Rm_gle$Rendement,mu=0)
t.test(Rd_cac$Rendement,mu = 0)
t.test(Rm_cac$Rendement,mu=0)

Residu <- residuals(modele)

hist(Residu,breaks = 100,freq = FALSE)
curve(dnorm(x, mean = mean(Residu), sd = sd(Residu)), 
      col = "blue", add = TRUE)
bptest(modele)

plot(Residu,type = "l")
acf(Residu)
acf(Residu^2)

hist(Residu,breaks = 100,freq = FALSE)
curve(dnorm(x, mean = mean(Residu), sd = sd(Residu)), 
      col = "blue", add = TRUE)

cor.test(Residu,Rd_gle$Rendement)
shapiro.test(Residu[1:5000])
ks.test(Residu, "pnorm", mean(Residu), sd(Residu))

#2)Estimation  modèle AR-Garch
#2.1)Estimation du AR
acf(Rd_gle$Rendement)
pacf(Rd_gle$Rendement)
#Implication => p=1 peut être 2
mod_ar <-ar(Rd_gle$Rendement,order.max=1)
Eps_t <- mod_ar$resid
Eps_t <- na.omit(Eps_t)
hist(Eps_t,breaks=1000)
t.test(Eps_t,mu=0)
Box.test(Eps_t)
## test de normalité des résidus 
## Test de Jarque Bera
jarque.bera.test(Eps_t)

shapiro.test(Eps_t[1:5000])

acf(Eps_t^2)
#2.2) Definition final de notre modèle

n = length(Rd_gle$Date)

#2.2.a) Quel GARCH?

spec1 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), mean.model = list(armaOrder = c(0,0), include.mean=FALSE))
garch1_1 <- ugarchfit(spec = spec1, data = Rd_gle$Rendement)
show(garch1_1)
#omega est  significatif au seuil de 5%
#AIC = -4.8399


spec2 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,2)), mean.model = list(armaOrder = c(0,0), include.mean=FALSE))
garch1_2 <- ugarchfit(spec = spec2, data = Rd_gle$Rendement)
show(garch1_2)
#AIC = -4.8395

spec3 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,1)), mean.model = list(armaOrder = c(0,0), include.mean=FALSE))
garch2_1 <- ugarchfit(spec = spec3, data = Rd_gle$Rendement)
show(garch2_1)
#AIC = -4.8395
#Seuil de Omega > 5%

spec4 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,2)), mean.model = list(armaOrder = c(0,0), include.mean=FALSE))
garch2_2 <- ugarchfit(spec = spec4, data = Rd_gle$Rendement)
show(garch2_2)
#AIC = -4.8392
#Seuil de Omega > 5%

#Le modèle choisi est le garch(1,1) soit spec1 car il est dans le seuil de confiance
#et minimise de critère AIC

#On affiche la volatilite dans le temps de notre modele
plot(Rd_gle$Date[5600:n],garch1_1@fit[["var"]][5600:n],type="l",main="Graphique representant la volatilite dans le temps",xlab="temps",ylab="volatilite")

#2.2.b) Quel TGARCH?

### TGARCH ###
spec5 <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1,1)), mean.model = list(armaOrder = c(0,0)))
tgarch1_1 <- ugarchfit(spec = spec5, data = Rd_gle$Rendement)
show(tgarch1_1)
#AIC = -4.8512

spec6 <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1,2)), mean.model = list(armaOrder = c(0,0)))
tgarch1_2 <- ugarchfit(spec = spec6, data = Rd_gle$Rendement)
show(tgarch1_2)
#AIC = -4.8508

spec7 <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(2,1)), mean.model = list(armaOrder = c(0,0)))
tgarch2_1 <- ugarchfit(spec = spec7, data = Rd_gle$Rendement)
show(tgarch2_1)
#AIC = -4.8507

spec8 <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(2,2)), mean.model = list(armaOrder = c(0,0)))
tgarch2_2 <- ugarchfit(spec = spec8, data = Rd_gle$Rendement)
show(tgarch2_2)
#AIC = -4.8507

# On choisit le TGARCH(1,1) car il minimise le criere AIC et est dans les seuils mais
#le paramètre mu ne semble pas signficatif

#On affiche la volatilite dans le temps de notre modele
plot(Rd_gle$Date[5600:n],tgarch1_1@fit[["var"]][5600:n],type="l",main="Graphique representant la volatilite dans le temps",xlab="temps",ylab="volatilite")

#2.2.c) Quel Igarch ?

### IGARCH ###
spec9 <- ugarchspec(variance.model = list(model = "iGARCH", garchOrder = c(1,1,1)), mean.model = list(armaOrder = c(0,0)))
igarch1_1 <- ugarchfit(spec = spec9, data = Rd_gle$Rendement)
show(igarch1_1)
#AIC = -4.8399

spec10 <- ugarchspec(variance.model = list(model = "iGARCH", garchOrder = c(1,1,2)), mean.model = list(armaOrder = c(0,0)))
igarch1_2 <- ugarchfit(spec = spec10, data = Rd_gle$Rendement)
show(igarch1_2)
#AIC = -4.8399

spec11 <- ugarchspec(variance.model = list(model = "iGARCH", garchOrder = c(2,1,1)), mean.model = list(armaOrder = c(0,0)))
igarch2_1 <- ugarchfit(spec = spec11, data = Rd_gle$Rendement)
show(igarch2_1)
#AIC = -4.8395

spec12 <- ugarchspec(variance.model = list(model = "iGARCH", garchOrder = c(2,1,2)), mean.model = list(armaOrder = c(0,0)))
igarch2_2 <- ugarchfit(spec = spec12, data = Rd_gle$Rendement)
show(igarch2_2)
#AIC = -4.8395

# On choisit le iGARCH(1,1,1) car il minimise le critere AIC
# Mais mu non significatif et beta1 = NA

#On affiche la volatilite dans le temps de notre modele
plot(Rd_gle$Date[5600:n],igarch1_1@fit[["var"]][5600:n],type="l",main="Graphique representant la volatilite dans le temps",xlab="temps",ylab="volatilite")

#2.2.d) Quel EGarch?

### eGARCH ###
spec13 <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1,1)), mean.model = list(armaOrder = c(0,0)))
egarch1_1 <- ugarchfit(spec = spec13, data = Rd_gle$Rendement)
show(egarch1_1)
#AIC = -4.8593

spec14 <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1,2)), mean.model = list(armaOrder = c(0,0)))
egarch1_2 <- ugarchfit(spec = spec14, data = Rd_gle$Rendement)
show(egarch1_2)
#AIC = -4.8589

spec15 <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(2,1)), mean.model = list(armaOrder = c(0,0)))
egarch2_1 <- ugarchfit(spec = spec15, data = Rd_gle$Rendement)
show(egarch2_1)
#AIC = -4.8592

spec16 <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(2,2)), mean.model = list(armaOrder = c(0,0)))
egarch2_2 <- ugarchfit(spec = spec16, data = Rd_gle$Rendement)
show(egarch2_2)
#AIC = -4.8589

# On choisit le eGARCH(1,1) car il minimise le critere AIC et seulement mu n'est pas significatif
#On affiche la volatilite dans le temps de notre modele
plot(Rd_gle$Date[5600:n],egarch1_1@fit[["var"]][5600:n],type="l",main="Graphique representant la volatilite dans le temps",xlab="temps",ylab="volatilite")

## Le modèle qui semble convenir est un garch(1,1)
plot(Rd_gle$Date[5600:n],Rd_gle$Rendement[5600:n],type = "l",xlab = "Dates", ylab = "Rendement", main = "Rendement journalier")
barplot(Rd_gle$Rendement[5600:n],names.arg = Rd_gle$Date[5600:n], xlab= "Dates", ylab = "Rendement",main = "Rendement Journalier")

#Prediction des volatilité future

prevision <- predict(garch1_1@fit[["var"]][5600:n],h=180)
forecast <- ugarchforecast(garch1_1,n.ahead=252/2)
D <-forecast@forecast[["sigmaFor"]][1]
W<-forecast@forecast[["sigmaFor"]][6]
M<-forecast@forecast[["sigmaFor"]][22]
Six_M<-forecast@forecast[["sigmaFor"]][126]
barplot(c(D,W,M,Six_M),names.arg = c(Rd_gle$Date[n]+1,Rd_gle$Date[n]+8,Rd_gle$Date[n]+31,Rd_gle$Date[n]+180),width = 0.001)

# Création d'un dataframe pour les prévisions
data <- data.frame(
  Horizon = c("1 jour", "1 semaine", "1 mois", "6 mois"),
  Volatilite = c(D, W, M, Six_M)
)
# Tracer le graphique de lignes avec des points
ggplot(data, aes(x = Horizon, y = Volatilite)) +
  geom_line(color = "blue") +
  geom_point(color = "red", size = 3) +
  labs(x = "Horizon de prévision", y = "Volatilité", title = "Prévision de volatilité future") +
  theme_minimal()

#Fin du programme