install.packages("plyr")
install.packages("dplyr")
install.packages("FactoMineR")
install.packages("data.table")
install.packages("tidyverse")
install.packages("explor")
install.packages("questionr")
install.packages("igraph")


##Appel des packages
library(plyr)
library(dplyr)
library(FactoMineR)
library(explor)
library(questionr)
library(data.table)
library(tidyverse)



#importation de la base de données
BD <- read_xlsx("Base_de_donnees_projet.xlsx")
BD

#comptage du nombre de personnes presente dans la base de données
by(BD, BD$numero,nrow)
#renommage des numeros en lettres
BD$numero <- recode(BD$numero, '33 X X X X X'='A',
                   '33 X X X X X'='B',
                   '33 X X X X X'= 'C',
                   '33 X X X X X'= 'D',
                   '33 X X X X X'= 'E',
                   '33 X X X X X'= 'F',
                   '33 X X X X X'= 'G',
                   '33 X X X X X'= 'H',
                   '33 X X X X X'= 'I',
                   '33 X X X X X'= 'J',
                   '33 X X X X X'= 'K',
                   '33 X X X X X'= 'L',
                   '33 X X X X X'= 'M',
                   '33 X X X X X'= 'N',
                   '33 X X X X X'= 'O',
                   '33 X X X X X'= 'P',
                   '33 X X X X X'= 'Q',
                   '33 X X X X X'= 'R',
                   '33 X X X X X'= 'S',
                   '33 X X X X X'= 'T',
                   '33 X X X X X'= 'U',
                   '33 X X X X X'= 'V',
                   '33 X X X X X'= 'W',
                   '33 X X X X X'= 'X',
                   '33 X X X X X'= 'Y',
                   .default = NA_character_)
#renommage de la colonne
colnames(BD)[4] <- "Personne"

###creation de la matrice d'adjacence
MATRICE <- matrix(0, nrow = 25, ncol = 25, dimnames = list(LETTERS[1:25], LETTERS[1:25]))
rownames(MATRICE) <- c('A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y')
colnames(MATRICE) <- c('A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y')
MATRICE

#creation du vecteur IDpersonne pour j
IDpersonne <- c('A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y')


##boucle for qui prend une par une chaque personne
for (j in IDpersonne) {
  print(j)

###sous selection des toutes les données ou la personne 'j' apparait
A <- filter(BD, BD$Personne == j)
#creation de la base de stoquage des resultats
Abis <- A
#creation du vecteur A$id pour i
id <- rownames(A)
A <- cbind(id=id, A)
A$id
##sous selection de toutes les personnes ayant repondu dans les 20 minutes a la personne 'j'
for (i in A$id) {
  Lien <- filter(BD, BD$date == A[i,"date"], BD$`heure en minute` >= A[i,"heure en minute"], BD$`heure en minute`-20 < A[i,"heure en minute"] )
  Abis <- bind_rows(Abis,Lien)
}
##

#supression des doublons
Abis <- distinct(Abis)
#retirage des données ou la personne 'j' est presente
Abis <- filter(Abis, Abis$Personne != j)
##recreation d'un vecteur pour la boucle for
Abis$id = NULL
id <- rownames(Abis)
Abis <- cbind(id=id, Abis)

##codage des liens dans la matrice

for (i in Abis$id) {
  MATRICE[Abis[i,"Personne"],j] <- MATRICE[Abis[i,"Personne"],j]+1
}
##
}
##

#matrice resultat 1 qui quantifie les relations
MATRICE_relations <- MATRICE
#matrice resultat 0 qui montre les relations
MATRICE[MATRICE>0] <-1
MATRICE_0 <- MATRICE


########################################################################################

##exportation de la matrice 0 
write.table(MATRICE_0,
            file = "MATRICE_0.csv",
            sep = "\t",
)
##exportation de la matrice 1 
write.table(MATRICE_relations,
            file = "MATRICE_relations.csv",
            sep = "\t",
)
