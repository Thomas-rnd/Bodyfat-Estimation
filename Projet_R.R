bodyfat<-donneesProjet

plot(bodyfat)

boxplot(bodyfat)
summary(bodyfat)
#Les valeurs à l'extérieur des moustaches sont représenté par des points 
#On ne peut pas dire que se sont des observations abéreantes. Par contre cela
#indique qu'il faut étudier plus en détail ces obsevations. 
#Nous ne sommes pas des spécialistes des variables étudié. On va suivre une démarche
#analytique en étudiant les valeurs aberrantes dans leur globalité. On va 
#vérifier la normalité de chaque variable 
hist(bodyfat[,1],50)
#On observe potentiellment des valeurs abérrantes

library(PCAmixdata)
require(PCAmixdata)
res<-PCAmix(bodyfat)
#Kaiser on s'interesse à dim 1 et 2
round(res$eig,digit=2)
scoreIndividus<-res$ind$coord
#On regarde uniquement les dim
shapiro.test(scoreIndividus[,1])
#Normalement distribué pas de problème
shapiro.test(scoreIndividus[,2])
#Pas normalement distribué donc il y a des valeurs abérrantes à retirer
#Pour savoir quelles variables à retirer on regarde les valeurs qui peuvent
#être abérrantes dans les variables bien représenté sur dim 2
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

plot(res,axes=c(1,2),choice="cor")
plot(res,axes=c(1,2),choice="ind")

arrows(0,0,cor(bodyfat[,1],res$scores)[1],cor(bodyfat[,1],res$scores)[2],col=2,xlim=c(-1, 1) * 1.3,ylim=c(-1, 1) * 1.3,lwd=3)   # variable Rank (en rouge)

arrows(0,0,cor(bodyfat[,2],res$scores)[1],cor(bodyfat[,2],res$scores)[2],col=3,xlim=c(-1, 1) * 1.3,ylim=c(-1, 1) * 1.3,lwd=3)   # variable Points (en vert)



