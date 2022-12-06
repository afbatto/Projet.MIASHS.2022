library(plyr)
library(dplyr)
library(FactoMineR)
library(explor)
library(questionr)
library(data.table)
library(tidyverse)
library(igraph)
library(stringr)


BDD<-read.csv("MATRICE_0.csv",sep="\t")

# Transformation de la base en une matrice
data <- as.matrix(BDD,nrow=25)

# Construction d'un objet de type réseau à partir de la matrice d'adjacence en ajoutant les couleurs 
g <- graph.adjacency(data, weighted=TRUE)
V(g)$color <- ifelse(V(g)$name == "A" | V(g)$name == "C" |V(g)$name == "E" | V(g)$name == "H"| V(g)$name == "I"|V(g)$name == "K" |V(g)$name == "M"|V(g)$name == "N"|V(g)$name == "Q"|V(g)$name == "U", "red", "lightblue")
plot(g, edge.arrow.size = 0.3, edge.size = 0.1, main = "Modélisation du réseau du groupe de L3 MIASHS")

plot(g,
     # Paramètres des sommets
     vertex.size = 22 * degree(g) / max(degree(g)) + 3,
     vertex.color = as.character(
       factor(betweenness(g),
              labels = rev(heat.colors(
                length(unique(betweenness(g)))
              )))
     ),
     vertex.label =V(g)$name,
     vertex.label.cex = .8,
     vertex.label.color = "blue",
     vertex.shape = "circle",
     vertex.label.color="blue",
     #
     # Paramètres des arêtes
     edge.arrow.size = 0.3,
     edge.width = E(g)$weight,
     edge.size = 0.1,
     edge.color = rgb(.1, .1, .1, .8),
     # Paramètres généraux
     layout = g$layout,
     main = "Modélisation du réseau du groupe de L3 MIASHS")


###analyse
#nombre de sommets
vcount(g)
V(g)
#nombre d’arêtes
ecount(g)
E(g)
#densitée
graph.density(g)
#diametre
diameter(g)
#coeficient de clutering
transitivity(g)
#degré orienté
degree(g, mode=c("in"))
degree(g, mode=c("out"))
degree(g, mode=c("total"))
degrée = degree(g, mode=c("total"))
mean(degrée)
quantile(degrée)  
#intermediarité
betweenness(g)
#reciprocité
reciprocity(g)
#moyenne de la longueur des chemins (appelée également distance moyenne)
mean_distance(g)
#plus large clique
largest_cliques(g)


# sous-graphes des sommets de degrés > 35 :
g35 <- induced_subgraph(g,
                                   V(g)[ degree(g) > 35 ]
)
plot(g35, edge.arrow.size = 0.3, edge.size = 0.1,
     main="Sous-graphe des sommets de degré superieur à la mediane"
)

# sous-graphes des sommets de degrés > 40 :
g40 <- induced_subgraph(g,
                        V(g)[ degree(g) > 40 ]
)
plot(g40, edge.arrow.size = 0.3, edge.size = 0.1,
     main="Sous-graphe des sommets de degré superieur au 3ème quartile"
)


