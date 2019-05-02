#mon_jeu_de_donnees <- readLines ("https://raw.githubusercontent.com/agusbudi/DataAnalysis/master/data1TP1.txt", warn=FALSE)
mon_jeu_de_donnees <- read.delim("https://raw.githubusercontent.com/agusbudi/DataAnalysis/master/data1TP1.txt", header=TRUE, sep="\t")
names(mon_jeu_de_donnees)
mon_jeu_de_donnees$Y
plot(mon_jeu_de_donnees$A, mon_jeu_de_donnees$Y,type="p")
#courbe décroissante pour A
#relation faible
#relation non linéaire négative
plot(mon_jeu_de_donnees$B, mon_jeu_de_donnees$Y,type="p")
#courbe croissante pour B
#relation faible
#relation non linéaire positive
plot(mon_jeu_de_donnees$C, mon_jeu_de_donnees$Y,type="p")
#bordel pour C
#abscence de relation
plot(mon_jeu_de_donnees$D, mon_jeu_de_donnees$Y,type="p")
#courbe exponentielle pour D
#relation faible
#relation non linéaire positive
plot(mon_jeu_de_donnees$E, mon_jeu_de_donnees$Y,type="p")
#courbe en cloche pour E
#relation faible

coeff_r <- function(x,y) {
  #ecart_type_x = sqrt(mean(x^2)-mean(x)^2)
  ecart_type_x = sd(x, na.rm = FALSE)
  #ecart_type_y = sqrt(mean(y^2)-mean(y)^2)
  ecart_type_y = sd(y, na.rm = FALSE)
  cov = cov(x, y, use = "everything", method = "pearson")
  cov/(ecart_type_x*ecart_type_y)
}
#coeff_r(mon_jeu_de_donnees$A,mon_jeu_de_donnees$Y)
#cor(mon_jeu_de_donnees$A,mon_jeu_de_donnees$Y)
#coeff_r(mon_jeu_de_donnees$B,mon_jeu_de_donnees$Y)
#cor(mon_jeu_de_donnees$B,mon_jeu_de_donnees$Y)
#coeff_r(mon_jeu_de_donnees$C,mon_jeu_de_donnees$Y)
#cor(mon_jeu_de_donnees$C,mon_jeu_de_donnees$Y)
#coeff_r(mon_jeu_de_donnees$D,mon_jeu_de_donnees$Y)
#cor(mon_jeu_de_donnees$D,mon_jeu_de_donnees$Y)
#coeff_r(mon_jeu_de_donnees$E,mon_jeu_de_donnees$Y)
#cor(mon_jeu_de_donnees$E,mon_jeu_de_donnees$Y)
#La variable E a la plus petite des corrélation

sum(rank(mon_jeu_de_donnees$D)[i]-rank(mon_jeu_de_donnees$Y)[i])

coeff_p <- function(x,y) {
  sum=0
  for (i in 1:15) {
    sum = sum+ (rank(x)[i]-rank(y)[i])^2
    p = 1 - ((6*sum)/(15^3-15))
    
  }
  p
}

#coeff_p(mon_jeu_de_donnees$A,mon_jeu_de_donnees$Y)
#cor(mon_jeu_de_donnees$A,mon_jeu_de_donnees$Y,method="spearman")
#coeff_p(mon_jeu_de_donnees$B,mon_jeu_de_donnees$Y)
#cor(mon_jeu_de_donnees$B,mon_jeu_de_donnees$Y,method="spearman")
#coeff_p(mon_jeu_de_donnees$C,mon_jeu_de_donnees$Y)
#cor(mon_jeu_de_donnees$C,mon_jeu_de_donnees$Y,method="spearman")
#coeff_p(mon_jeu_de_donnees$D,mon_jeu_de_donnees$Y)
#cor(mon_jeu_de_donnees$D,mon_jeu_de_donnees$Y,method="spearman")
#coeff_p(mon_jeu_de_donnees$E,mon_jeu_de_donnees$Y)
#cor(mon_jeu_de_donnees$E,mon_jeu_de_donnees$Y,method="spearman")

mon_jeu_de_donnees2 <- read.delim("https://raw.githubusercontent.com/agusbudi/DataAnalysis/master/data2TP1.txt", header=TRUE, sep="\t")
names(mon_jeu_de_donnees2)
mon_jeu_de_donnees2$Marseille
mon_jeu_de_donnees2$Aix

# Une variable non-linéaire et non-monotone, comme entre E et Y, peut être tout
# simplement un polynome, c'est à dire une relation entre plusieurs variables.
# Plusieurs solutions peuvent être utilisées pour résoudre ce problème :
# faire une transformation, utiliser une fonction non-linéaire...

score_t <- function(x) {
  abs(mean(x)-19)/(sd(x, na.rm = FALSE)/sqrt(length(x)))
}

score_t(mon_jeu_de_donnees2$Marseille)
#2.145, on rejette car résultat plus grand 2.177369

score_t_2 <- function(x1,x2) {
  abs(mean(x1)-mean(x2)) / sqrt( (sd(x1, na.rm = FALSE)^2 / length(x1)) + (sd(x2, na.rm = FALSE)^2 / length(x2)) )
}

score_t_2(mon_jeu_de_donnees2$Marseille,mon_jeu_de_donnees2$Aix)
#2.048, on rejette car résultat plus grand 2.321494
#2.468, on accepte car résultat plus petit 2.321494
# Observation normale, car le test est plus précis, donc engloble plus de valeurs. Avec une précision de 98%,
# la valeur obtenue par le test est inclu dans la courbe en cloche, alors qu'elle ne l'était pas avec 
# un test de niveau de signification d'alpha 0.05.

ratio = c(9,3,3,1)
val_obs = c(1528,106,117,381)
ratio_total = sum(ratio)
n_total = sum(val_obs)

val_theo <- function(x) {
  (x/ratio_total)*n_total
}

val_theorique = c()

for ( i in 1:4) { 
  val_theorique<-cbind(tab,val_theo(ratio[i]))
}

print(val_theorique)

khi_deux <- function(val_theorique, val_obs) {
  sum=0
  for (i in 1:4) {
    sum = sum+ ((val_obs[i]-val_theorique[i])^2/val_theorique[i])
    
  }
  sum
}

khi_deux(val_theorique,val_obs)
#On obtient 966, pour un degré de liberté de 3, on devrait avoir 7.81
#Soit beaucoup plus grand ! On en conclut que les valeurs de ratios ne sont pas bonnes

val_form = rbind(c(29,5,46), c(40,32,8), c(18,22,0))
val_precense = rbind(c(20,60), c(29,51), c(12,28))
print(val_form)
print(val_precense)
n_total=sum(val_form)
val_form_theo = rbind(c(0,0,0), c(0,0,0), c(0,0,0))
val_precense_theo = rbind(c(0,0), c(0,0), c(0,0))

khi_deux_matriciel <- function(mat) {
  val_theo <- matrix(nrow=nrow(mat), ncol=ncol(mat))
  khi_deux=0
  for (i in 1:nrow(mat)) {
    for (j in 1:ncol(mat)) {
      val_theo[i,j] = (sum(mat[i,])*sum(mat[,j]))/n_total
      khi_deux= khi_deux + ((mat[i,j]-val_theo[i,j])^2/val_theo[i,j])
    }
  }
  khi_deux
}

print(khi_deux_matriciel(val_form))
print(khi_deux_matriciel(val_precense))
#Cela ne vérifie pas H0, elle est dépendante, elle est donc importante pour détecter un mélanome
#Cela vérifie H0, donc elle est indépendante, elle n'est donc pas importante pour détecter un mélanome

#Les tests paramétriques se basent sur des distributions statistiques supposées dans les données. 
#Par conséquent, certaines conditions de validité doivent être vérifiées pour que le résultat d'un test paramétrique
#soit fiable. 

#D'après les questions précedentes, le test t de Student pour échantillons indépendants n'est fiable que si les
#données associées à chaque échantillon suivent une distribution normale et si les variances des échantillons sont homogènes.
#C'est pour cela qu'il est paramétrique.

#Les tests non-paramétriques ne se basent pas sur des distributions statistiques.
#Ils peuvent donc être utilisés même si les conditions de validité des tests paramétriques ne sont pas vérifiées.
#Il me semble qu'un test paramétrique est comme son nom l'indique basé sur l'estimation de paramètres des échantillons
#, or le test du Chi2 est une comparaison de proportions d'échantillons indépendants et des proportions (pourcentages...) 
#ne sont pas des paramètres à proprement parler
#C'est pour cela qu'il est non-paramétrique.


#Les données qualitatives sont des données auxquelles on ne peut pas attribuer une valeur ou une caractéristique.
#Exemples de propriétés physiques qualitatives : La couleur, la texture, le goût, l'odeur, l'état et la ductilité.
#Certains considèrent que toute donnée qui ne peut être qualifiée de quantitative est par défaut une donnée qualitative.
#Le coefficient de corrélation de Pearson permet d'analyser les relations linéaires et le coefficient de corrélation 
#de Spearman les relations non-linéaires monotones. Ce sont des méthodes de corrélation.
#La corrélation : permet de savoir s'il existe un lien entre deux variables quantitatives, si les valeurs des deux
#variables varient dans le même sens ou dans le sens contraire
#Entre une variable de type quantitative en lien avec une variable de type quantitative
#On ne peut donc pas les utiliser sur des données qualitatives.