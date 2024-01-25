#Ouverture du csv avec les séparateur ","
data <- read.csv("FF_factors.csv",sep=",")

#On observe les données chargées
str(data)
typeof(data)
mode(data)
dim(data)
View(data)
summary(data)
#On utilise une nouvelle variable qu'on va manipuler
data_use <- data
#on convertit les dates
d1<- paste0(substr(data_use$X, 1, 4), "-", substr(data_use$X, 5, 6),"- 01")
data_use$date <- as.Date(d1)
class(data_use$date)

View(data_use)

data_use$Mkt <- data_use$Mkt.RF - data_use$RF

#on vérifie que toutes les données sont exploitables
length(which(is.na(data_use)))
#La base de donnée est propre, on peut désormais regarder les informations phares des différentes variables

summary(data_use$Mkt)
summary(data_use$RF)

plot(data_use$date,data_use$Mkt,"l",main = "Valeur de Mkt en fonction du temps", xlab = "Dates" , ylab = "Mkt")
plot(data_use$date,data_use$RF,"l",main = "Valeur de RF en fonction du temps", xlab = "Dates" , ylab = "RF")

ggplot(data=data_use)+ geom_line(mapping= aes(x=date, y=Mkt),size=0.01,colour="black") + ggtitle("Mkt en fonction du temps") 
ggplot(data=data_use)+ geom_line(mapping= aes(x=date, y=RF),size=0.1,colour="red") + ggtitle("RF en fonction du temps") 
