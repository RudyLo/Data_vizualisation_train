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


#Centrer les données avec scale et les réduire
list <- c("Calories", "Total.Fat", "Cholesterol")
menu.cr <- scale(menu[, list])
lims <- c(min(menu.cr),max(menu.cr))
plot3d(menu.cr, type = "s", xlim = lims, ylim = lims,zlim = lims)

#Représentation de l'ellispe de concentration
menu.cr_df <- as.data.frame(menu.cr)
plot3d(menu.cr, type = "s", xlim = lims, ylim = lims,zlim = lims)
plot3d(ellipse3d(cor(cbind(menu.cr_df$Calories, menu.cr_df$Total.Fat,menu.cr_df$Cholesterol))), col="grey",add=TRUE)

#Centrer les données avec scale et les réduire
list <- c("Sodium", "Sugars", "Protein")
menu.cr <- scale(menu[, list])
lims <- c(min(menu.cr),max(menu.cr))
plot3d(menu.cr, type = "s", xlim = lims, ylim = lims,zlim = lims)

#Représentation de l'ellispe de concentration
menu.cr_df <- as.data.frame(menu.cr)
plot3d(menu.cr, type = "s", xlim = lims, ylim = lims,zlim = lims)
plot3d(ellipse3d(cor(cbind(menu.cr_df$Sodium, menu.cr_df$Sugars,menu.cr_df$Protein))), col="grey",add=TRUE)
