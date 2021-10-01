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

