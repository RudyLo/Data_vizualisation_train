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
