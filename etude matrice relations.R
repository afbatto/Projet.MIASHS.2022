library(plyr)
library(dplyr)
library(FactoMineR)
library(explor)
library(questionr)
library(data.table)
library(tidyverse)
library(igraph)


BDD1<-read.csv("MATRICE_relations.csv",sep="\t")

# Transformation de la base en une matrice
data1 <- as.matrix(BDD1,nrow=25)

poids <- data1
g1 <- graph.adjacency(data1, weighted=TRUE)
set.edge.attribute(g1,
                   name = "poids",
                   value = g1$poids)
g1 <- graph.adjacency(poids)
V(g1)$color <- ifelse(V(g1)$name == "A" | V(g1)$name == "C" |V(g1)$name == "E" | V(g1)$name == "H"| V(g1)$name == "I"|V(g1)$name == "K" |V(g1)$name == "M"|V(g1)$name == "N"|V(g1)$name == "Q"|V(g1)$name == "U", "red", "lightblue")
plot(g1 , edge.size = 0.1, edge.arrow.size = 0.3,  main = "Réseau du groupe L3 MIASHS",  sub = "Pondéré par le nombre d'interactions")


plot(g,
     # Paramètres des sommets
     vertex.size = 15 * degree(g1) / max(degree(g1)) + 3,
     vertex.color = as.character(
       factor(betweenness(g1),
              labels = rev(heat.colors(
                length(unique(betweenness(g1)))
              )))
     ),
     vertex.label = V(g1)$name,
     vertex.label.cex =0.8,
     vertex.label.color = "blue",
     vertex.shape = "circle",
     vertex.label.color="blue",
     #
     # Paramètres des arêtes
     edge.arrow.size = 0.3,
     edge.size = 0.1,
     edge.color = rgb(.1, .1, .1, .8),
     # Paramètres généraux
     layout = g1$layout,
     main = "Réseau du groupe de L3 MIASHS",
     sub = "Pondéré par le nombre d'interactions")


###analyse
#nombre de sommets
vcount(g1)
V(g1)
#nombre d’arêtes
ecount(g1)
E(g1)
#densitée
graph.density(g1)
#diametre
diameter(g1)
#coeficient de clutering
transitivity(g1)
#degré orienté
degree(g1, mode=c("in"))
degree(g1, mode=c("out"))
degree(g1, mode=c("total"))
degrée1 = degree(g1, mode=c("total"))
degrée1
max(degrée1)
mean(degrée1)
quantile(degrée1)  
#intermediarité
betweenness(g1)
#reciprocité
reciprocity(g1)
#moyenne de la longueur des chemins (appelée également distance moyenne)
mean_distance(g1)


# sous-graphes des sommets de degrés > 279 :
g1279 <- induced_subgraph(g,
                        V(g)[ degree(g1) > 279 ]
)
plot(g1279, edge.arrow.size = 0.3, edge.size = 0.1, vertex.label.cex =1.2,
     main="Sous-graphe des sommets de degré superieur à la mediane",
     sub = "Pondéré par le nombre d'interactions"
)

# sous-graphes des sommets de degrés > 418 :
g1418 <- induced_subgraph(g,
                        V(g)[ degree(g1) > 418 ]
)
plot(g1418, edge.arrow.size = 0.3, edge.size = 0.1,
     vertex.label.cex =1.2,
     main="Sous-graphe des sommets de degré superieur au 3ème quartile",
     sub = "Pondéré par le nombre d'interactions"
)


