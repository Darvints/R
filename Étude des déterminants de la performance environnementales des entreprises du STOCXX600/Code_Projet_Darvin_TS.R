#Code du projet R de Darvin TS

#Importation des librairies
library(openxlsx)
library(FactoMineR)
library(corrplot)
library(ggplot2)
#Chargement des données & penser à se placer à la bonne racine
#J'ai renommé le fichier projet.xlsx , pensez à modifier le nom en focntion du nom de votre fichier
data <- read.xlsx("projet.xlsx")

#Création d'un doublon à manipuler

df <- data

#Combien de données comporte EPS et CIPEP?
EPS_NA = sum(is.na(df[22]))
CIPEP_NA = sum(is.na(df[27]))
#t() est la commande pour la transposé d'un vecteur
n = length(t(df[22]))

#On se rend compte qu'on a peu de données pour CIPEP
# Créer un diagramme à barres
N <- c(n-CIPEP_NA,n-EPS_NA,n)
barplot(N,col = c("red","lightgreen","green"), main = "Diagramme à Barres du nombre de réponses", names.arg = c("CIPEP", "EPS", "N"))
legend("topleft", legend = c("Observations totals", "Observations pour EPS", "Observations pour CIPEP"), fill = c("green", "lightgreen", "red"), cex = 0.3)

# On s'oriente vers l'usage d'une ACP
Prix <- df[5:12]
#On renomme les colonnes pour faciliter l'usage et la lisibilité
colnames(Prix) <- c("1D","5D","4W","13W","26W","YTD","52W","+52W")
res.acp<-PCA(Prix)
summary(res.acp)

cr<-cor(Prix[complete.cases(Prix), ])

corrplot(cr, title = "Matrice de corrélation imagée")

#A partir du graphe de l'acp on remarque une corrélation forte entre certaines variables
#Plutot que d'observer des données sur 8 dimensions , on va se ramener à quelque chose de beaucoup plus simple 
#L'usage de L'ACP nous le permet justement

View(res.acp)

#On récupère la projection sur la 1ère Dimension (qui résume 45% des datas)
Fi <- res.acp$ind$coord[,1]
View(Fi)
#Lègere ambiguïté sur la composante à extraire, il serait peut être intéressant de prendre aussi la 2ème dimension pour avoir un résumé de 60%

###DATAVIZ, Stat desc , RL

#PAramétrisation du dataframe

quanti <- c(22,18,19,21,4,24,25)
quali <- c(17,23)
df_quanti <- df[,quanti]
colnames(df_quanti)<-c("EPS","EMP","MARGIN","SALES","MCAP","RCAP","BOARD")
df_quali <- df[,quali]
colnames(df_quali)<-c("COD","CSR")

#On ajoute Fi

df_quanti["Fi"] <- Fi

# Stat desc

write.xlsx(summary(df_quanti),"sdesc.xlsx")
write.xlsx((table(df_quali[1])),"sdesc.xlsx")
write.xlsx((table(df_quali[2])),"sdesc.xlsx")

#Dataviz

# Initialisation d'une liste pour stocker les graphiques
plots <- list()

for (i in colnames(df_quanti)) {
  plots[[i]] <- ggplot(df_quanti, aes_string(x = df_quanti[, 1], y = df_quanti[, i])) +
    geom_point() +  geom_smooth(method = "lm", se = FALSE, color = "red") + labs(title = paste("Graphique de", i,"en fonction d'EPS" ), x = "EPS", y = i)
}

# Affichage des graphiques
for (plot in plots) {
  print(plot)
}

ggplot(df, aes_string(x = colnames(df)[23], y = colnames(df)[22]),fill = colnames(df)[23]) +
  geom_bar(stat = "summary", fun = "mean", position = position_dodge()) +
  labs(title = "Moyenne de EPS selon si CSR",
       x = "CSR",
       y = "Moyenne de EPS") +
  theme_minimal()

ggplot(df, aes_string(x = colnames(df)[17], y = colnames(df)[22]),fill = colnames(df)[17]) +
  geom_bar(stat = "summary", fun = "mean", position = position_dodge()) +
  labs(title = "Moyenne de EPS selon pays",
       x = "Pays",
       y = "Moyenne de EPS") +
  theme_minimal()+
theme(axis.text.x = element_text(size = rel(0.5)))

df_final <- df_quanti
df_final[,c("COD","CSR")] <- df_quali

# Estimation de la régression
modele <- lm(EPS ~ EMP + MARGIN + SALES + MCAP + RCAP + BOARD + Fi + COD + CSR, data = df_final)

# Afficher les résultats
summary(modele)
write.xlsx(modele,"sdesc.xlsx")
