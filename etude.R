#Chargement des données
bodyfat<-data

#########################
### Etude descriptive ###
#########################

#Représentation deux à deux des variables du jeu de données permettant d'inférer sur
#la corélation de certaines d'entre elles. 
library(corrplot)
M = cor(bodyfat)
corrplot(M, method = 'square', order = 'FPC', type = 'lower', diag = FALSE, main="Représensation de la corrélation entre les variables d'estimation de la masse graisseuse")
plot(bodyfat, main="Représensation des individus sur un plan de variables")
#Intuition sur des valeurs potentiellment hors normes
boxplot(bodyfat,main="Mise en évidence des valeurs extrêmes par variables")
summary(bodyfat)

###################################
### Analyse multidimensionnelle ###
###################################

library(PCAmixdata)
require(PCAmixdata)
res<-PCAmix(bodyfat)
#Pour extraire les valeurs abérrantes nous nous interressons à la normalité des 5 premières
#dimensions propre de l'ACP
round(res$eig,digit=2)
#On récupère la répartition des individus suivant les dimensions propres 
scoreIndividus<-res$ind$coord
#On regarde uniquement les dimensions 1 à 5
shapiro.test(scoreIndividus[,1])
#Normalement distribué pas de problème
shapiro.test(scoreIndividus[,2])
#Pas normalement distribué donc il y a des valeurs abérrantes à retirer
#Pour savoir quelles variables à retirer on regarde les valeurs qui peuvent
#être abérrantes dans les variables bien représenté suivant la dim 2
shapiro.test(scoreIndividus[,3])
#Normalement distribué pas de problème
shapiro.test(scoreIndividus[,4])
#Normalement distribué pas de problème
shapiro.test(scoreIndividus[,5])
#Pas normalement distribué donc il y a des valeurs abérrantes à retirer
#Pour savoir quelles variables à retirer on regarde les valeurs qui peuvent
#être abérrantes dans les variables bien représenté suivant la dim 5


#Valeurs abérrantes dim 2
round(res$quanti$cos2,digits=3)
plot(res,axes=c(1,2),choice="sqload")

#On regarde les hist pour nous aider
hist(bodyfat[,1],50, xlab='Pct.BF', main="Histogramme des individus suivant Pct.BF") # 4 valeurs à retirer (3 à droite) (entre 3 et 35)
hist(bodyfat[,2],50, xlab='Age', main="Histogramme des individus suivant Age") #Dépend du recrutement pas normalement distrubé normal
hist(bodyfat[,4],50, xlab='Height', main="Histogramme des individus suivant Height")# 2 valeurs à retirer (à droite) (entre 65 et 76)

#On retire les valeurs hors normes
bodyfat<-subset(bodyfat,bodyfat[,1]>=3)
bodyfat<-subset(bodyfat,bodyfat[,1]<=35)
bodyfat<-subset(bodyfat,bodyfat[,2]<=75)
bodyfat<-subset(bodyfat,bodyfat[,4]>=65)
bodyfat<-subset(bodyfat,bodyfat[,4]<=76)

#Valeurs abérrantes dim 5
round(res$quanti$cos2,digits=3)
plot(res,axes=c(1,5),choice="sqload")

#On regarde les hist pour nous aider
hist(bodyfat[,11],50,xlab='Ankle', main="Histogramme des individus suivant Ankle")#On supprime valeurs >30

#Suppression des valeurs
bodyfat<-subset(bodyfat,bodyfat[,11]<30)

#Nouvelle ACP avec le jeu de données nettoyé
res2<-PCAmix(bodyfat)
scoreIndividus<-res2$ind$coord
#Vérification de la normalité des dimensions propres
shapiro.test(scoreIndividus[,1])
shapiro.test(scoreIndividus[,2])
shapiro.test(scoreIndividus[,3])
shapiro.test(scoreIndividus[,4])
shapiro.test(scoreIndividus[,5])
#Jeu de données propres et dim normalisées => début de l'analyse

round(res$eig,digit=2)
round(res$quanti$cos2,digits=3)

plot(res2,axes=c(1,2),choice="sqload")
plot(res2,axes=c(1,2),choice="cor")
plot(res2,axes=c(1,2),choice="ind")

arrows(0,0,cor(bodyfat[,1],res2$scores)[1],cor(bodyfat[,1],res2$scores)[2],col=2,xlim=c(-1, 1) * 1.3,ylim=c(-1, 1) * 1.3,lwd=3)   # variable BF (en rouge) 
arrows(0,0,cor(bodyfat[,2],res2$scores)[1],cor(bodyfat[,2],res2$scores)[2],col=3,xlim=c(-1, 1) * 1.3,ylim=c(-1, 1) * 1.3,lwd=3)   # variable Age (en vert)

###########################
### Regression linéaire ###
###########################

# Modele 1 (modele complet)
#===========================
data<-data.frame(bodyfat[1:220,])
#Discuter du nombre de données pour avoir un entraînement du modèle suffisant

mod1<-lm(Pct.BF~.,data)
summary(mod1)
#Etudier la validité de ce modèle, potentiellment bcp de variables pas 
#utiles/pertinentes

# Selection de variables
#========================
step(mod1)

# Modele 2 (modele simplifie)
#============================
mod2 <- lm(formula = Pct.BF ~ Age + Height + Neck + Abdomen + Hip + Thigh + 
             Forearm + Wrist, data)
summary(mod2)

#Normalité des résidus
plot(mod2$fitted,mod2$residuals) 
abline(h=0)
shapiro.test(mod2$residuals)

# Prédiction/Test
#============================

#On peut entrainer un modèle sur 200 valeurs et regarder son comportement sur la prédiction
#des 39 autres valeurs

#Calcul des valeurs prédites
BFpredict<-predict(mod2,data.frame(bodyfat[220:239,]),interval="prediction",level=0.95)

#Représentations
plot(BFpredict[,1],bodyfat[220:239,1])
abline(0,1,col=2)
abline(-8,1,col=1)
abline(8,1,col=1)

#Pourcentage d'erreur de la prédiction
erreur<-(BFpredict[,1]-bodyfat[220:239,1])*100/bodyfat[220:239,1]
absErreur<-abs((BFpredict[,1]-bodyfat[220:239,1])*100/bodyfat[220:239,1])
summary(absErreur)

plot(erreur, xlab="Index",ylab="Erreur de prédiction (en %)", main="Graphique des erreurs de prédictions de notre modèle")
abline(h=-0,col=2)

