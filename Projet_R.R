#Chargement des données
bodyfat<-donneesProjet

#Représentation deux à deux des variables du jeu de données permettant d'inférer sur
#la corélation de certaines d'entre elles. 
plot(bodyfat)

boxplot(bodyfat)
summary(bodyfat)
#Les valeurs à l'extérieur des moustaches sont représenté par des points 
#On ne peut pas dire que se sont des observations abéreantes. Par contre cela
#indique qu'il faut étudier plus en détail ces obsevations. 
#Nous ne sommes pas des spécialistes des variables étudié. On va suivre une démarche
#analytique en étudiant les valeurs aberrantes dans leur globalité. On va 
#vérifier la normalité de chacune des dimensions propres de l'ACP.

hist(bodyfat[,1],50)
#On observe potentiellment des valeurs abérrantes
# Univarié pas suffisant pour conclure 

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
#être abérrantes dans les variables bien représenté sur dim 2
shapiro.test(scoreIndividus[,3])
#Normalement distribué pas de problème
shapiro.test(scoreIndividus[,4])
#Normalement distribué pas de problème
shapiro.test(scoreIndividus[,5])
#Pas normalement distribué donc il y a des valeurs abérrantes à retirer
#Pour savoir quelles variables à retirer on regarde les valeurs qui peuvent
#être abérrantes dans les variables bien représenté sur dim 5

#######
#Valeurs abérrantes dim 2
#######
round(res$quanti$cos2,digits=3)
plot(res,axes=c(1,2),choice="sqload")

#Bien représentées Age, Height, BF, "ankle, abdomen"
#On regarde les min et max de ces variables 
#=> pas de problèmes pour Age, Height

#On regarde les hist pour nous aider
hist(bodyfat[,1],50) # 4 valeurs à retirer (3 à droite) (entre 3 et 35)
hist(bodyfat[,2],50) #Dépend du recrutement pas narmalement distrubé normal
hist(bodyfat[,4],50)# 2 valeurs à retirer (à droite) (entre 65 et 76)

bodyfat<-subset(bodyfat,bodyfat[,1]>=3)
bodyfat<-subset(bodyfat,bodyfat[,1]<=35)
bodyfat<-subset(bodyfat,bodyfat[,4]>=65)
bodyfat<-subset(bodyfat,bodyfat[,4]<=76)

#######
#Valeurs abérrantes dim 5
#######
round(res$quanti$cos2,digits=3)
plot(res,axes=c(1,5),choice="sqload")
#Bien représenté Height,Ankle
#On a déja fait le tri dans Height donc on regarde uniquement Ankle

hist(bodyfat[,11],50)#On supprime valeurs >30
bodyfat<-subset(bodyfat,bodyfat[,11]<30)#Suppression des valeurs

#Nouvelle ACP
res2<-PCAmix(bodyfat)
scoreIndividus<-res2$ind$coord
#Vérification
shapiro.test(scoreIndividus[,1])
shapiro.test(scoreIndividus[,2])
shapiro.test(scoreIndividus[,3])
shapiro.test(scoreIndividus[,4])
shapiro.test(scoreIndividus[,5])

#Jeu de données propres (normalisé)=> début de l'analyse
round(res$eig,digit=2)
round(res$quanti$cos2,digits=3)
#Kaiser >1 donc dim 1 et 2 interessantes. Cos2 dans dim 3 <0.3 

plot(res2,axes=c(1,2),choice="cor")
plot(res2,axes=c(1,2),choice="ind")

arrows(0,0,cor(bodyfat[,1],res2$scores)[1],cor(bodyfat[,1],res2$scores)[2],col=2,xlim=c(-1, 1) * 1.3,ylim=c(-1, 1) * 1.3,lwd=3)   # variable BF (en rouge) 

arrows(0,0,cor(bodyfat[,2],res2$scores)[1],cor(bodyfat[,2],res2$scores)[2],col=3,xlim=c(-1, 1) * 1.3,ylim=c(-1, 1) * 1.3,lwd=3)   # variable Age (en vert)

#Regression linéaire

# Modele 1 (modele complet)
#===========================
data<-data.frame(bodyfat)

mod1<-lm(Pct.BF~data[,-1],data)
summary(mod1)
