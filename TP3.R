# Travaux Pratiques num�ro 3 - THOMAS GATTAZ

#BUT DE CE TP :
#On va g�n�rer diff�ramment des nuages de points et on va utiliser des lois diff�rents pour chacune

###### EXERCICE 1 : G�n�rer des points du plan (m points), sous la forme d'un tableau ou d'une matrice
###### (matrice � m lignes et 2 colonnes). Les classes pourront �tre represent�s par des valeurs binaires
###### Les distances entre classes seront simplement les distances euclidiennes entre les centres des classes.


##### 1 : G�n�ration de nuages de points (m = 300 points, 3 modes de g�n�ration)

nbpoints = 100

### a : g�n�ration de x et y uniformes sur [0;1]

X = runif(nbpoints, min = 0, max = 1)
Y = runif(nbpoints, min = 0, max = 1)
matrice <- cbind(X,Y)
#plot(X,Y,col = "red")

### b : g�n�ration de x et y gaussiennes (ind�pendantes) de variance 1 (moyenne de x 4, y centr�e) sur [0;1]

X = rnorm(nbpoints, mean = 4, sd = 1)
Y = rnorm(nbpoints, mean = 0, sd = 1)
matrice2 <- cbind(X,Y)
#plot(X,Y, col = "blue")

### c : g�n�ration de x et y gaussiennes (ind�pendantes) de variance 2 (moyenne de x 0,5, y de moyenne 6) sur [0;1]

X = rnorm(nbpoints, mean = 0.5, sd = 2)
Y = rnorm(nbpoints, mean = 6, sd = 2)
matrice3 <- cbind(X,Y)
#plot(X,Y, col = "green")


#On r�unit les 3 fois 100 valeurs dans une seule matrice
matrice_final <- rbind(matrice,matrice2,matrice3)
#On cr�e une matrice z qui permettra de colorier diff�ramment nos classes
colors <- matrix(1, nrow = 100, ncol = 1)
colors2 <- matrix(2, nrow = 100, ncol = 1)
colors3 <- matrix(3, nrow = 100, ncol = 1)
z <- rbind(colors,colors2,colors3)

#On visualisera l'ensemble des points obtenus :
#Pour cela on colorie diff�ramment les classes pour permettre de visualiser la qualit� de la classification effectu�e.
#plot(valeurs)
plot(matrice_final, col = c("red", "blue", "green")[z])


valeurs <- matrix(NA,nrow = 300, ncol = 2)
rownames(valeurs)<-c(1:300)

for (i in 1:300){
  valeurs[i,1] <- matrice_final[i,1]
  valeurs[i,2] <- matrice_final[i,2]
}

# fonction distance Euclidienne permettant d'avoir la distance entre deux points
distance_euclidienne<-function(x1,y1,x2,y2){
  X = cbind(x1,x2)
  Y = cbind(y1,y2)
  dist <- sqrt((X[,2]- Y[,2])^2 + (X[,1] - Y[,1])^2)
  return(dist)
}

###### EXERCICE 2 : FOnction de classification ascendante hi�rarchique, o� K (nombre de classe final) sp�cifi�e en entr�e
###### Sortie : matrice binaire (m lignes, K colonnes) repr�sentant la classification.

###### Gestion des classes faite par matrice carr�e C d'ordre �, initialement matrice identit�, pour laquelle
###### la fusion des classes Id(i, j) conduit � faire la somme des lignes, rempla�ant la ligne i, la ligne j �tant mise � 0
###### On calculera pour la nouvelle classe la distance aux autres classes, toutes les distances �tant sous forme d'un
###### tableau, qui sera donc �galement actualis� dans la boucle.

###### Les calculs de moyennes par classe sont calcul�s pour chaque nouvelle classe. A chaque classe, on associe
###### sa moyenne et son nombre de point. POur l'agr�gation des classes, on choisit les classes la distance minimale.

# Fonction de classification ascendante hi�rarchique
# Entr�e valeurs : tout les points
# Entr�e K : nombre de classe finale 
# Entr�e m : nombre de points
# Sortie : matrice_binaire de taille (m lignes, K colonnes) repr�sentant la classication



fonction_classification_ascendante_hierarchque<- function(K,m,valeurs){
  
  # C'est une matrice de distance qui contient au final les distance entre chaque points de la matrice
  dist <- dist(valeurs,method="euclidean")
  dist <- as.matrix(dist)
  dist[upper.tri(dist)] <- NA 
  diag(dist) <- NA
  
  # Quel est la distance minimal ?
  a_supprimer <- which(dist==min(dist,na.rm=T),arr.ind = T)
  a_supprimerX <- a_supprimer[,1]
  a_supprimerY <- a_supprimer[,2]
  meanX <- (valeurs[a_supprimerX,1]+valeurs[a_supprimerY,1])/2
  meanY <- (valeurs[a_supprimerX,2]+valeurs[a_supprimerY,2])/2
  
  
  dist <- dist(valeurs,method="euclidean")
  dist <- as.matrix(dist)
  dist[upper.tri(dist)] <- NA
  diag(dist) <- NA
  
  # On fait ce traitement tant que le nombre de lignes de distance est sup�rieur au nombre de classes K d�fini pour la fonction 
  while(nrow(dist) > K){
    # Quel est la distance minimal ?
    a_supprimer <- which(dist==min(dist,na.rm=T),arr.ind = T)
    a_supprimerX <- a_supprimer[1,1]
    a_supprimerY <- a_supprimer[1,2]
    meanX <- (valeurs[a_supprimerX,1]+valeurs[a_supprimerY,1])/2
    meanY <- (valeurs[a_supprimerX,2]+valeurs[a_supprimerY,2])/2
    
    # Remplacement par la moyenne
    valeurs[a_supprimerX,1] <- meanX
    valeurs[a_supprimerX,2] <- meanY
    rownames(valeurs)[a_supprimerX]<-paste(rownames(valeurs)[a_supprimerX],rownames(valeurs)[a_supprimerY])
    valeurs <- valeurs[-a_supprimerY,]
    dist <- dist[-a_supprimerY,]
    dist <- dist[,-a_supprimerY] 
    dist <- dist(valeurs,method="euclidean")
    dist <- as.matrix(dist)
    dist[upper.tri(dist)] <- NA
    diag(dist) <- NA
    
  }
  
  # retourne la matrice avec les centres des classes 
  return (valeurs)
}

# R�cup�ration du centre des classes
Classe_centres <- fonction_classification_ascendante_hierarchque(3,300,valeurs)
tableau <- rownames(Classe_centres)
first <- strsplit(tableau[1]," ")
second <- strsplit(tableau[2]," ")
third <- strsplit(tableau[3]," ")
first_class <- as.numeric(unlist(first))
second_class <- as.numeric(unlist(second))
third_class <- as.numeric(unlist(third))
result_class <- cbind(valeurs, c(0))
for(i in 1:length(first_class)){
  result_class[first_class[i],3] <- 1
}
for(i in 1:length(second_class)){
  result_class[second_class[i],3] <- 2
}
for(i in 1:length(third_class)){
  result_class[third_class[i],3] <- 3
}
Classe_centres <- cbind(Classe_centres,c(1,2,3))

# affichage nuages de points avec les centres de classes 
plot(result_class,pch=10, col=factor(result_class[,3]))
points(Classe_centres,pch=21,col="yellow",lwd=5 ,bg=factor(Classe_centres[,3]),cex=3)

###Programme permettant de v�rifier les r�sultats obtenus avec ce programme

#Calculer la distance Euclidean
D <- dist(valeurs, method = "euclidean")
#la fonction de classification ascendante hi�rarchique
AscHierarchique <- hclust(D, method = "complete") #complete ou ward
#visualisation par Dendrogramme
plot(AscHierarchique, cex=0.6, hang = -1)
#r�cup�rer les r�sultats
cluster = cutree(AscHierarchique,3)
plot(cluster)


