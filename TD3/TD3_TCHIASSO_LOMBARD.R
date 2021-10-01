#1.1 Chargement des données et statistique élémentaires

#1 Téléchargement de la bibliothèque ade4

install.packages("ade4")
library("ade4")

#2. Installation de pokemon.csv
#Import des données
setwd(dir = "C:/Users/Rudyl/Desktop/ESIEA/Cours_5A/Data_Visualisation/Data_visualisation_tp/TD3")

pokemon <- read.csv("pokemon.csv", encoding = "UTF-8")

#3. Transformation du jeu de données en DataFrame

poke <- as.data.frame(pokemon) 

#4. Transformation de variable Generation en type factor
poke$Generation <- as.factor(poke$Generation)  
class(poke$Generation)

#5. Créez un sous jeu de données composé exclusivement des variables suivantes :Type_1, Generation et Legendary.
poke.x<-poke[,c(3,12,13)]
poke.x <- as.data.frame(poke.x)

#6. Application de la fonction summary

summary(poke.x)

#Nous avons 2 variables qualitatives et 1 variable qualitative


#1.2 ACM avec ade4

#7. À l’aide de la bibliothèque ade4 et de la bibliothèque adegraphics, appliquez la fonction dudi.acm() au jeu de données poke.x.

res.acm.poke<-dudi.acm(poke.x,scannf=FALSE)

#Nous devons convertir nos données en type factor avant de pouvoir appliquer l'acm sur poke.x :
poke.x$Type.1 <- as.factor(poke.x$Type.1)  
poke.x$Generation <- as.factor(poke.x$Generation)  
poke.x$Legendary <- as.factor(poke.x$Legendary)  

#Nous pouvons maintenant appliquer l'acm
res.acm.poke<-dudi.acm(poke.x,scannf=FALSE)

#Installation du package devetools et Factoextra pour analyser nos valeurs propres
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/factoextra")

library("factoextra")

fviz_screeplot(res.acm.poke)
val <- get_eig(res.acm.poke)

# Seuls les axes dont l'inertie est supérieure à l'inertie moyenne sont retenus

mean(val$variance.percent)
(val$variance.percent) > 4.347826

#On observe qu'il y à plusieurs dimensions qui répondent au critères plus haut, dans notre cas, nous conservons donc les 6 premières.


res.x<-val[c(1,2,3,4,5,6),]

score(res.acm.poke,xax=1)

head ( inertia.dudi(res.acm.poke)$tot )




