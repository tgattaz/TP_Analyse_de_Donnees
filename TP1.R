#mon_jeu_de_donnees <- readLines ("https://raw.githubusercontent.com/agusbudi/DataAnalysis/master/data1TP1.txt", warn=FALSE)
mon_jeu_de_donnees <- read.delim("https://raw.githubusercontent.com/agusbudi/DataAnalysis/master/data1TP1.txt", header=TRUE, sep="\t")


#1. Tracez en dimension 2 le nuage de 15 points pour chaque variable. Que pouvez-vous observer ?

names(mon_jeu_de_donnees)
mon_jeu_de_donnees$Y
plot(mon_jeu_de_donnees$A, mon_jeu_de_donnees$Y,type="p")
#courbe d�croissante pour A
#relation faible
#relation non lin�aire n�gative

plot(mon_jeu_de_donnees$B, mon_jeu_de_donnees$Y,type="p")
#courbe croissante pour B
#relation faible
#relation non lin�aire positive

plot(mon_jeu_de_donnees$C, mon_jeu_de_donnees$Y,type="p")
#bordel pour C
#abscence de relation

plot(mon_jeu_de_donnees$D, mon_jeu_de_donnees$Y,type="p")
#courbe exponentielle pour D
#relation faible
#relation non lin�aire positive

plot(mon_jeu_de_donnees$E, mon_jeu_de_donnees$Y,type="p")
#courbe en cloche pour E
#relation faible

#2. PEARSON: Quelle variable a la plus petite corr�lation ? Pourquoi ?
#La variable E a la plus petite des corr�lation

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

#3. Cr�ez une fonction du coefficient de Spearman en consid�rant cette formule :
#SPEARMAN : Comparez le score obtenu au r�sultat de la question 2. Quelle est la diff�rence ?


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

#4) Comment calculer la relation non-lin�aire et non-monotone entre variables E et Y ? Proposez votre id�e.

# Une variable non-lin�aire et non-monotone, comme entre E et Y, peut �tre tout
# simplement un polynome, c'est � dire une relation entre plusieurs variables.
# Plusieurs solutions peuvent �tre utilis�es pour r�soudre ce probl�me :
# faire une transformation, utiliser une fonction non-lin�aire...


#5. Test d'ind�pendance pour une variable quantitative
mon_jeu_de_donnees2 <- read.delim("https://raw.githubusercontent.com/agusbudi/DataAnalysis/master/data2TP1.txt", header=TRUE, sep="\t")
names(mon_jeu_de_donnees2)
mon_jeu_de_donnees2$Marseille
mon_jeu_de_donnees2$Aix



score_t <- function(x) {
  abs(mean(x)-19)/(sd(x, na.rm = FALSE)/sqrt(length(x)))
}

score_t(mon_jeu_de_donnees2$Marseille)
#2.145, on rejette car r�sultat plus grand 2.177369


#6. Test d'ind�pendance pour deux variables quantitatives
score_t_2 <- function(x1,x2) {
  abs(mean(x1)-mean(x2)) / sqrt( (sd(x1, na.rm = FALSE)^2 / length(x1)) + (sd(x2, na.rm = FALSE)^2 / length(x2)) )
}

score_t_2(mon_jeu_de_donnees2$Marseille,mon_jeu_de_donnees2$Aix)
#2.048, on rejette car r�sultat plus grand 2.321494
#2.468, on accepte car r�sultat plus petit 2.321494
# Observation normale, car le test est plus pr�cis, donc engloble plus de valeurs. Avec une pr�cision de 98%,
# la valeur obtenue par le test est inclu dans la courbe en cloche, alors qu'elle ne l'�tait pas avec 
# un test de niveau de signification d'alpha 0.05.

#7. Test d'ind�pendance pour une variable qualitative (Non param�trique)

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
#On obtient 966, pour un degr� de libert� de 3, on devrait avoir 7.81
#Soit beaucoup plus grand ! On en conclut que les valeurs de ratios ne sont pas bonnes

#8. Test d'ind�pendance pour les variables qualitatives

val_form = rbind(c(29,5,46), c(40,32,8), c(18,22,0))
val_precense = rbind(c(20,60), c(29,51), c(12,28))
print(val_form)
print(val_precense)
n_total=sum(val_form)
val_form_theo = rbind(c(0,0,0), c(0,0,0), c(0,0,0))
val_precense_theo = rbind(c(0,0), c(0,0), c(0,0))

khi_deux_matriciel <- function(mat) {
  val_theo <-matrix(nrow=nrow(mat),ncol=ncol(mat))
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
#Cela ne v�rifie pas H0, elle est d�pendante, elle est donc importante pour d�tecter un m�lanome
#Cela v�rifie H0, donc elle est ind�pendante, elle n'est donc pas importante pour d�tecter un m�lanome


#9. Selon les questions pr�c�dentes, pourquoi le test de Student/t est class� comme param�trique et le test du Khi
#Deux est class� comme non param�trique ? Pouvons-nous appliquer le test de Student/t aux donn�es qualitatives?
  
#Les tests param�triques se basent sur des distributions statistiques suppos�es dans les donn�es. 
#Par cons�quent, certaines conditions de validit� doivent �tre v�rifi�es pour que le r�sultat d'un test param�trique
#soit fiable. 

#D'apr�s les questions pr�cedentes, le test t de Student pour �chantillons ind�pendants n'est fiable que si les
#donn�es associ�es � chaque �chantillon suivent une distribution normale et si les variances des �chantillons sont homog�nes.
#C'est pour cela qu'il est param�trique.

#Les tests non-param�triques ne se basent pas sur des distributions statistiques.
#Ils peuvent donc �tre utilis�s m�me si les conditions de validit� des tests param�triques ne sont pas v�rifi�es.
#Il me semble qu'un test param�trique est comme son nom l'indique bas� sur l'estimation de param�tres des �chantillons
#, or le test du Chi2 est une comparaison de proportions d'�chantillons ind�pendants et des proportions (pourcentages...) 
#ne sont pas des param�tres � proprement parler
#C'est pour cela qu'il est non-param�trique.

#10.Pouvons-nous appliquer le coefficient de Pearson et le coefficient de Spearman aux donn�es qualitatives?

#Les donn�es qualitatives sont des donn�es auxquelles on ne peut pas attribuer une valeur ou une caract�ristique.
#Exemples de propri�t�s physiques qualitatives : La couleur, la texture, le go�t, l'odeur, l'�tat et la ductilit�.
#Certains consid�rent que toute donn�e qui ne peut �tre qualifi�e de quantitative est par d�faut une donn�e qualitative.
#Le coefficient de corr�lation de Pearson permet d'analyser les relations lin�aires et le coefficient de corr�lation 
#de Spearman les relations non-lin�aires monotones. Ce sont des m�thodes de corr�lation.
#La corr�lation : permet de savoir s'il existe un lien entre deux variables quantitatives, si les valeurs des deux
#variables varient dans le m�me sens ou dans le sens contraire
#Entre une variable de type quantitative en lien avec une variable de type quantitative
#On ne peut donc pas les utiliser sur des donn�es qualitatives.