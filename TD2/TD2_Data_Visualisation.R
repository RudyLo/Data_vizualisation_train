#Import des données
setwd(dir = "C:/Users/Rudyl/Desktop/ESIEA/Cours_5A/Data_Visualisation/Data_visualisation_tp/TD2")

#Row_names=1 nous permet d'afficher nos départements en nom de ligne
president12 <- read.csv2("C:/Users/Rudyl/Desktop/ESIEA/Cours_5A/Data_Visualisation/Data_Visualisation_tp/TD2/President12.csv", encoding = "UTF-8", row.names=2)

#Convertion en DataFrame
president12 <- as.data.frame(president12) 

par(mar=c(5,10,4,2), mfrow=c(1,1))

#Affichage les nombres de voix obtenues par les différents candidats dans le département du Bas-Rhin
barplot(as.matrix(president12[67,2:11]),names.arg=colnames(president12)[2:11],col="purple3", border="white", main="Bas-Rhin", horiz=T, las=1,xlab="nombre de voix", cex.lab=1.2)

#Affichage les nombres de voix obtenues par lesdifférents candidats dans le département de Paris
barplot(as.matrix(president12[75,2:11]),names.arg=colnames(president12)[2:11],col="purple3", border="white", main="Paris", horiz=T, las=1,xlab="nombre de voix", cex.lab=1.2)

#Affichage les nombres de voix obtenues par lesdifférents candidats dans le département de Seine-Saint-Denis
barplot(as.matrix(president12[93,2:11]),names.arg=colnames(president12)[2:11],col="purple3", border="white", main="Seine-Saint-Denis", horiz=T, las=1,xlab="nombre de voix", cex.lab=1.2)

#Affichage les nombres de voix obtenues par lesdifférents candidats dans le département de Seine-Saint-Denis
barplot(as.matrix(president12[84,2:11]),names.arg=colnames(president12)[2:11],col="purple3", border="white", main="Seine-Saint-Denis", horiz=T, las=1,xlab="nombre de voix", cex.lab=1.2)


#Tableau de fréquences pour le Bas Rhin
transpose <- t(president12[68,2:11])
par(mar=c(5,10,4,2), mfrow=c(1,))
#Profil ligne du Bas-Rhin
barplot(as.matrix(president12[68,2:11])/sum(as.matrix(president12[68,2:11])),horiz = TRUE,border="white", col="blue4", beside=T, xlab="Bas-Rhin", cex.lab=1.5, las=1)
sum(transpose)
