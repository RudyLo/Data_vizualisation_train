#Import des données au format csv
menu <- read.csv("C:/Users/Rudyl/Desktop/ESIEA/Cours_5A/Data_Visualisation/Data_visualisation_tp/menu.csv")
menu <- as.data.frame(menu)
summary(menu)

#Intallation du package MVN
install.packages("MVN")
library(MVN)

#Détermination de la loi normale bivariée entre deux variables
couple_to_test <- c("Calories", "Total.Fat")
result = mvn(menu[couple_to_test], mvnTest = "mardia", univariateTest = "SW", univariatePlot = "histogram", multivariatePlot = "qq", multivariateOutlierMethod = "adj", showOutliers = TRUE, showNewData = TRUE)

#Test de corrélation linéaire de Pearson
cor.test(menu$Calories, menu$Total.Fat)


#Séparation du Dataframe menu
donnes_separees <- c("Calories", "Total.Fat", "Cholesterol", "Sodium", "Sugars", "Protein")
indices <- which(colnames(menu) %in% donnes_separees)
new_menu <- menu[,indices]
summary(new_menu)

#Affichage de la matrice de corrélation
correlation_mat <- cor(new_menu)

da <- c("Calories", "Total.Fat","Cholesterol","Sodium","Sugars","Protein")
m1 <- matrix(nrow = 6, ncol = 6)
dimnames(m1) <- list(da, da)
m1

#Remplissage de la matrice de corrélation
for (i in 0:36) {
  m1[i] <- correlation_mat[i]
}
m1


## Representation en 3D des trois variables : Calories, Total.Fat, Cholesterol
install.packages ("rgl")
library(rgl)
plot3d(menu$Calories, menu$Total.Fat,menu$Cholesterol, type="s")
