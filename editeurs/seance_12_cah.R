#####################################################################
###Séance 12  : Faire une classification ascendante hiérarchique  ###
#####################################################################

#On reprend l'ACM faite dans la séance 11, ACM stocké dans l'objet res.acm

#1. On réalise la CAH avec la fonction HCPC
cah_eec <- HCPC(res.acm, nb.clust = 3)
#nb.clust = on spécifie le nombre de cluster qu'on souhaite
#Quand beaucoup d'individus, possibilité de faire de la pré-catégorisation par k-means : kk

#2. Visualisation des résultats

# Dendrogramme
plot(cah_eec, choice = "tree")

# Graphe des individus
plot(cah_eec, choice = "map", draw.tree = F)

## 4. Description des clusters

# Quelles sont les variables et modalités qui contribuent le plus aux différents clusters ?
cah_eec$desc.var 

# Quels sont les individus qui sont proches du centre d’un cluster et loin du centre des autres clusters ?
cah_eec$desc.ind

# Quelles sont les variables qui contribuent le plus à la classification ?
cah_eec$desc.axes

# Fréquence par clusters 
freq(cah_eec$data.clust$clust)


# Intégrer une variable qualitative "cluster" dans la base de données d'origine 
#qui precise dans quel cluster se situe l'individu 
d2_acm$cluster <- cah_eec$data.clust$clust
