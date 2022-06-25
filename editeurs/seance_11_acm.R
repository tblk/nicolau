####################
# Séance 11 : ACM ##
####################

# Notre objectif ici est d'objectiver l'articulation entre divers modes de segmentation du marche du travail salarié :
# segmentation statutaire (insiders/outsiders)
# segmentation entre secteurs d'activité
# segmentation genrée 


#### Chargement des packages et des donnees ####
library(questionr)
library(tidyverse)
library(FactoMineR)
library(explor)
library(Factoshiny)
library(factoextra)


eec2018 <- read.csv2("C:/Users/marin/Documents/cours EHESS/CoursR_2021-22/Debutant+/Data/eec2018_extrait.csv")
d <- eec2018

#### On ne sélectionne que le groupe des salarié.es actif.ves occupé.es (i.e. qui reçoivent un salaire)
d2 <- subset(d, !is.na(d$SALRED))

###############################################
# 1. Recodage et preparation du jeu de donnees#
###############################################

# A. Caracteristiques de l'emploi : type de contrat, anciennete dans l'emploi, niveau de qualification (PCS), types d'horaires

##Recodage CSP  
freq(d2$CSER) #CSP
d2$CSP <- as.character(d2$CSER)
d2$CSP <- fct_recode(d2$CSP,
                     "Cadres" = "3",
                     "Intermédiaires" = "4",
                     "Employés" = "5",
                     "Ouvriers" = "6")
freq(d2$CSP)

##Niveau de rémunération
summary(d2$SALRED) #tranche de rémunération mensuelle 
d2$SALAIRE <- cut(d2$SALRED,
                  include.lowest = TRUE,
                  right = TRUE,
                  breaks = c(0, 500, 1000, 1500, 2000, 2500,30000))
d2$SALAIRE <- factor(d2$SALAIRE, labels=c("0-500E", "500-1000E", "1000-1500E", "1500-2000E", "2000-2500E", "2500-30000E"))
freq(d2$SALAIRE)

##Ancienneté dans l'entreprise
freq(d2$ANCENTR4)
d2$ANCIENNETE <- as.character(d2$ANCENTR4)
d2$ANCIENNETE <- fct_recode(d2$ANCIENNETE,
                            "Inf_1an" = "1",
                            "1-5ans" = "2",
                            "5-10ans" = "3",
                            "Plus_10ans" = "4")
freq(d2$ANCIENNETE) #4,4 % de NA

#Horaires de travail
freq(d2$HORAIC)
## Recodage de d2$HORAIC en d2$HORAIRE
d2$HORAIRE <- as.character(d2$HORAIC)
d2$HORAIRE <- fct_recode(d2$HORAIRE,
                         "Horaires_fixe" = "1",
                         "Horaires_alternés" = "2",
                         "Horaires_variables" = "3",
                         "Autre" = "4")
freq(d2$HORAIC) # 2,1 % de NA

##Variable temps de travail 
d2$TPSTRAVAIL <- as.character(d2$TPPRED)
d2$TPSTRAVAIL <- fct_recode(d2$TPSTRAVAIL,
                            "complet" = "1",
                            "partiel" = "2")
freq(d2$TPSTRAVAIL)

#HHC6 : Nombre d'heures travaillées en moyenne par semaine dans l'emploi principal, heures 
#supplémentaires comprises 
freq(d2$HHC6)
d2$TPSTRAVDETAILLE <- as.character(d2$HHC6)
d2$TPSTRAVDETAILLE <- fct_recode(d2$TPSTRAVDETAILLE,
                                 "Inf_15h" = "1",
                                 "15-30h" = "2",
                                 "30-35h" = "3",
                                 "35h-40h" = "4",
                                 "Sup_40h" = "5") 
freq(d2$TPSTRAVDETAILLE) #2,1 % de NA

##Variable contrat de travail CCONTR
## NA : personnes non actives occupées, travailleurs informels, travailleurs intérimaires,
## en activité temporaire ou d'appoint, indépendants, chefs d'entreprise, aides familiaux, salariés de la fonction publique, de
## la sécurité sociale, d'un particulier
d2$CONTRAT <- as.character(d2$CCONTR)
d2$CONTRAT <- fct_recode(d2$CONTRAT,
                         "Saisonnier_interim" = "0",
                         "CDI" = "1",
                         "CDD" = "2",
                         "Saisonnier_interim" = "3",
                         "Saisonnier_interim" = "4",
                         "Saisonnier_interim" = "5")
d2$CONTRAT <- fct_explicit_na(d2$CONTRAT, "CDI") #Les NA correspondent en grande majorité à des salariés de la fonction publique
freq(d2$CONTRAT)

# B. Caracteristiques de l'employeur ou de l'espace de travail : public ou prive et secteur d'activite

##Recodage des secteurs d'activité 
d2$SECTEUR <- fct_recode(d2$NAFG004N,
                         "agri" = "ES",
                         "indus" = "ET",
                         "construc" = "EU",
                         "tert" = "EV")
freq(d2$SECTEUR)

##Salarié de l'Etat ou du privé 
d2$EMPLOYEUR <- as.character(d2$CHPUB)
d2$EMPLOYEUR <- fct_recode(d2$EMPLOYEUR,
                           "Entreprise" = "1",
                           "Entreprise" = "2",
                           "Fonction_publique" = "3",
                           "Fonction_publique" = "4",
                           "Hopitaux" = "5",
                           "Entreprise" = "6",
                           "Employeur_particulier" = "7")
freq(d2$EMPLOYEUR) #0,5 % de NA


#C. Variables supplémentaires 

## Recodage de d2$AGE en d2$AGEGO
freq(d2$AGE)
d2$AGEGO <- cut(d2$AGE, include.lowest=TRUE,  right=TRUE,
                breaks=c(15, 30, 45, 60, 83))
freq(d2$AGEGO)


## Recodage de d2$SEXE en d2$SEXEGO
d2$SEXEGO <- as.character(d2$SEXE)
d2$SEXEGO <- fct_recode(d2$SEXEGO,
                        "Homme" = "1",
                        "Femme" = "2")

## Recodage du diplome 
freq(d2$DDIPL)
d2$DIPLOME <- as.character(d2$DDIPL)
d2$DIPLOME <- fct_recode(d2$DIPLOME,
                         "sup_bac" = "1",
                         "sup_bac" = "3",
                         "bac" = "4",
                         "inf_bac" = "5",
                         "inf_bac" = "6",
                         "inf_bac" = "7")
freq(d2$DIPLOME)

##Recodage statut face à l'immigration
freq(d2$IMMI)
d2$IMMIEGO <- as.character(d2$IMMI)
d2$IMMIEGO <- fct_recode(d2$IMMIEGO,
                         "Immigré" = "1",
                         "Non_immigré" = "2")


#D. Création d'une sous-base de données avec les variables recodées ci-dessus pour l'ACM

# Selectionner les individus et les variables qui entrent dans l'ACM, et verifier le format des variables.
d_acm <- subset(d2,select=c("AGEGO", "SEXEGO", "IMMIEGO", "DIPLOME","CSP", "TPSTRAVAIL","TPSTRAVDETAILLE", "CONTRAT","SALAIRE","ANCIENNETE", "HORAIRE", "SECTEUR","EMPLOYEUR"))

# si l'on veut afficher d'un seul coup tous les tris a plat des variables contenues dans le jeu de donnees :
lapply(d_acm, freq)

# La fonction MCA n'accepte que des variables de type "factor". Est-ce le cas ?
str(d_acm) # Oui ! # Si cela n'avait pas ete le cas, exemple de manip a realiser : d_acm <- as.data.frame(lapply(d_acm, factor))

#NB : on réalise des tris croisés pour étudier les relations entre variables


########################################################
# 2. Réalisation de l'ACM et exploration des résultats #
########################################################

#Première tentative : 
res.acm <- MCA(d_acm, quali.sup=c(1:4), level.ventil=0.03) 
#les 4 premières valeurs sont les variables supplémentaires (de 1 à 4)
#level.ventil permet de ventiler aléatoirement dans l'échantillon les modalités dont les effectifs sont faibles (ici < = 3 %)

#Option explor pour visualiser facilement et rapidement les résultats d’une analyse géométrique, dans une fenêtre graphique interactive. 
explor(res.acm)

##PB 1 : nuages des individus et des modalités bizarres... Les faibles effectifs de NA tendent à tirer vers la droite l'axe 1 (regroupement des NA en haut à droite, info potentiellement redondante)
## Quelle solution ? On peut décider de les supprimer, mais solution à utiliser avec parcimonie !
## Si les NA constituent notre objet d'étude, on peut en faire une modalité à part entière 
## Autres possibilités : quand beaucoup de NA => package MissMDA pour imputer les valeurs ou fonction speMDA() du package GDAtools
## on peut les recoder en une modalité spécifique et utiliser la fonction level.ventil
## Comme les NA représentent des parts faibles de l'échantillon, on décide de les supprimer
d2_acm <- subset(d_acm, complete.cases(d_acm)) #On enlève les NA

# PB 2 : Attention aux variables "redondantes" i.e. ne portant que sur l'une ou l'autre des deux sous-populations
# Ex travail partiel / tps de travail détaillé => on peut décider d'enlever la catégorie temps de travail détaillé 
# Deuxième solution : refaire l'analyse sur l'une ou l'autre des deux sous-populations
# Ex : les personnes à temps partiel vs les personnes à temps complet

res.acm <- MCA(d2_acm, ncp=5, quali.sup=c(1:4,7), level.ventil=0.03) 
explor(res.acm)

##############
# FactoShiny #
##############

#Option MCAshiny du Package factoshiny qui permet de générer automatiquement les codes :
MCAshiny(res.acm)


###############
# "A la main" #
###############

# Avoir un apercu des resultats (nombre d'individus, de variables, et objets crees) 
summary(res.acm)

# Afficher les valeurs propres
res.acm$eig 
#On trouve une colonne avec les valeurs propres, 
#une avec le pourcentage de variance imputable à l'axe 
#et une avec le pourcentage cumulé de variance des axes ordonnés dans l'ordre décroissant 
#Diagramme en barres des valeurs propres ou des inerties associées à chaque axe
barplot(res.acm$eig[,2], main="Histogramme des valeurs propres", 
        xlab="Axes", ylab="Pourcentage d'inertie", cex.axis=0.8, font.lab=3,
        col="orange")

#Informations sur les variables : contributions, cos2 et coordonnees
res.acm$var
res.acm$var$contrib #contribution des variables actives
res.acm$var$coord #coordonnées des variables actives
res.acm$var$cos2 

# Representation graphique de l'ACM
plot(res.acm) #la fonction fournit par défaut le graphique des individus et des modalités

# Representation graphique des variables :
plot(res.acm, choix="var", axes=c(1,2))  # on ne prend que le 1er et le 2e axe
plot(res.acm, choix="var", axes=c(2,3))  # on prend le 2e et le 3e axe

# Nuage des modalites
plot(res.acm, invisible=c("ind"), cex=0.8, axes=c(1,2))
# ici, on lui demande de rendre invisible les individus. 
# on affiche les modalites des variables actives et supplementaires.

# Nuage des individus
plot(res.acm, choix=c("ind"), invisible=c("var", "quali.sup"), cex=0.7, axes=c(1,2))
# ici, on lui demande de rendre invisible les modalites et les variables qualitatives supplementaires. 
# on n'affiche que les individus.

# On peut décider de représenter le nuage des modalités les plus contributives :

# On calcule la contribution moyenne des modalités, et on sélectionne les modalités pour lesquelles la contribution est supérieure au seuil
seuil <- 100/nrow(res.acm$var$contrib)
seuil

#contribution des variables
res.acm$var$contrib

#on sélectionne les modalités qui contribuent le plus à l'axe
moda <- which(res.acm$var$contrib[, 1]>seuil 
              | res.acm$var$contrib[, 2]>seuil)
moda

# Graphique avec les modalités les plus contributives
plot.MCA(res.acm, invisible=c("ind","quali.sup"), 
         title="Nuage des modalites actives Plan 1-2", axes=c(1,2),
         selectMod=c(moda), cex=0.8)

# axes : les axes qu'on represente
# choix: le graph qu'on veut representer ("ind" pour individus et modalites, "var" pour les variables, "quanti.sup" pour les variables supp)
# invisible : indique les points qui ne doivent pas etre dessines 
# col.ind : une couleur pour les individus ; col.var : couleur des categories ; col.quali.sup : couleur des variables supplementaires
# label : imprime les labels des points
# title : titre de notre graph
# habillage : none = couleurs par defaut, quali= une couleur pour chaque variable (colore les modalites)
# palette : couleurs utilisees pour les points ex: palette=palette(rainbow(30))
# autoLab : optimise la position des labels pour ne pas qu'ils se chevauchent (="yes"), ou ne le fait pas (="no")
# cex : la taille des labels (1 est la taille standard)
# select : ne dessine qu'une selection d'elements
# selectMod : ne dessine qu'une selection de modalites ex: selectMod ="contrib 10"



##############################
# Graphiques avec factoextra##
##############################

fviz_mca_ind(res.acm, axes = c(1, 2), geom = c("point", "text"),
             label = "none", labelsize = 3,
             pointsize = 2, col.ind = "red", alpha.ind = "contrib")

fviz_mca_var(res.acm, col.var="contrib", geom = c("point", "text"))

fviz_mca_var(res.acm, col.var="contrib", geom.var=c("point", "text"), repel=TRUE,
             select.var = list(names=names(moda))) # on ne represente que les modalites le plus contribuant aux axes 1 et 2
#repel = TRUE : éviter que les labels se chevauchent

fviz_mca_var(res.acm, col.var="contrib", geom.var=c("point", "text"), repel=TRUE, 
             select.var = list(contrib=20)) 


#############################################################################
#3. Mise en forme et export des resultats des contributions/cos2 axe par axe#
#############################################################################
dim1 <- cbind(res.acm$var$contrib[,1], res.acm$var$coord[,1], res.acm$var$cos2[,1])
colnames(dim1) <- paste(c("Contrib.", "Coord.", "Cos2"), "(Axe 1)")
dim1

dim2 <- cbind(res.acm$var$contrib[,2], res.acm$var$coord[,2], res.acm$var$cos2[,2])
colnames(dim2) <- paste(c("Contrib.", "Coord.", "Cos2"), "(Axe 2)")
dim2

# Mise en forme des resultats pour le premier plan factoriel (axes 1 et 2)
dim12 <- round(cbind(dim1, dim2), 1)
dim12 <- dim12[moda, ]

#copier-coller les contributions dans excel/openoffice
copie(dim12) 

