library(scatterplot3d)
library("plot3D")
library("plot3Drgl")
library(psych)


mon_jeu_de_donnees <- read.delim("https://raw.githubusercontent.com/agusbudi/DataAnalysis/master/data1TP2.txt", header=TRUE, sep="\t")

#1. Tracez en dimension 3 le nuage de 10 points.
scatterplot3d(mon_jeu_de_donnees)
scatter3D(x=mon_jeu_de_donnees$Stature, y=mon_jeu_de_donnees$Poids, z=mon_jeu_de_donnees$Taille, type="p", add=FALSE, col="red")

#2. Écrivez le tableau centré B, et la matrice de covariance V.
x <-  mon_jeu_de_donnees$Stature
y <-  mon_jeu_de_donnees$Poids
z <-  mon_jeu_de_donnees$Taille

tab_b <- function(x,y,z) {
  moy_x=mean(x)
  moy_y=mean(y)
  moy_z=mean(z)
  for ( i in 1:10) { 
    x[i] = x[i] - moy_x
    y[i] = y[i] - moy_y
    z[i] = z[i] - moy_z
  }
  cbind(x,y,z)
}


tab_b(x,y,z)
B=scale(mon_jeu_de_donnees, center=TRUE, scale=FALSE)
B=scale(mon_jeu_de_donnees, scale=FALSE)
V=cov(mon_jeu_de_donnees)

#3. Déterminez la représentation spectrale (valeurs propres et vecteurs propres de V).
S = eigen(V)
Val= S$values # valeurs propres
Vec= S$vectors # vecteurs propres
print(Val)
print(Vec)

#4. Indiquez les axes principaux (dans l'ordre).
#Dans notre cas, les axes principaux sont ordonnées 
#Les valeurs propres correspondent aux axes principaux
#En effet, la stature est le plus grand en premier, le poids en second, et la taille (la plus petite).

#5. Générez le tableau C en multipliant B par les vecteurs propres de V
C=B %*% Vec
print(C)
princomp(mon_jeu_de_donnees)$scores

#6. Observez en dimension 3 le nuage de points avec tracé du premier axe principal.
scatterplot3d(C)
#print(c(0,-100*Vec[1,1]))
scatter3D(x=c(0,-300*Vec[1,1]),y=c(0,-300*Vec[2,1]),z=c(0,-300*Vec[3,1]),add=TRUE,type='l')

#7. Représentez le nuage de points en dimension 2, projetés des points de départ sur le plan formé des deux premiers axes principaux.
#xi <- c(1:length(C[,1]))
#print(C[,1])

projection = matrix(c(C[,1],C[,2]), ncol=2)
plot(projection[,1], projection[,2], main="Nuage de Point", xlab="Stature", ylab="Poids", pch=19)

#8. Interprétez les résultats obtenus.
#Les résultats obtenus semblent cohérents.


