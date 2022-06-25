# Packages-------

library(readxl)

library(tidyverse)

library(questionr)

library(FactoMineR)

library(GDAtools)

library(knitr)

# Import & Recodages-----

d <- read_excel("data/pop_enq.xlsx") %>%
  
  mutate(pcs_pere = fct_recode(csp_pere,
                               "Agriculteur" = "1",
                               "Artisan" = "2",
                               "Cadre" = "3",
                               "Profession intermédiaire" = "4",
                               "Employé" = "5",
                               "Ouvrier" = "6",
                               "Autre personne sans activité" = "8",
                               NULL = "NA"),
         pcs_mere = fct_recode(csp_agr_mere,
                               "Artisane" = "2",
                               "Cadre" = "3",
                               "Profession intermédiaire" = "4",
                               "Employée" = "5",
                               "Ouvrière" = "6",
                               "Autre personne sans activité" = "8",
                               NULL = "NA"),
         parent_cadre = if_else(csp_pere == "3" | csp_agr_mere == "3",
                                "Au moins un parent cadre",
                                "Aucun parent cadre"))

# Sexe et âge----

ggplot(d, aes(x = sexe, y = age, color = sexe), alpha = 0.7) +
  
  geom_boxplot(varwidth = TRUE) +
  
  geom_jitter() +
  
  theme_minimal()

# PCS des parents--------

table(d$csp_pere, d$csp_agr_mere) %>%
  
  prop()

table(d$parent_cadre) %>%
  
  freq(digits = 0, valid = FALSE)

# Origine scolaire et sociale----

table(d$filiere_bac, d$csp_pere) %>%
  
  cprop(digits = 0)

# Sexe et temps de travail-----

ggplot(d, aes(x = sexe, y = heures_travail, color = sexe), alpha = 0.7) +
  
  geom_boxplot(varwidth = TRUE) +
  
  geom_jitter() +
  
  ggtitle("Les femmes travaillent plus à côté") +
  
  theme_minimal()

# Classe prépa et âge-------

ggplot(d, aes(x = classes_prepa, y = age, color = classes_prepa), alpha = 0.7) +
  
  geom_violin() +
  
  geom_jitter() +
    
  ggtitle("Les anciens élèves de prépa sont à l'heure scolaire") +
  
  theme_minimal()

# Age, temps de travail et sexe-------

ggplot(d, aes(x = age, y = heures_travail, colour = sexe)) +
  
  geom_point(size = 2) +
  
  ggtitle("Le travail à côté : les femmes et les plus jeunes") +
  
  theme_minimal()

# ACM--------

## Réalisation-----

d_acm = d %>%
  
  drop_na() %>% # on perd 6 individus, ça va encore
  
  select(tuteur_hdr, sexe, age_cat, classes_prepa, travail, pcs_pere, pcs_mere,
         filiere_bac) %>%
  
  mutate(tuteur_hdr = as_factor(tuteur_hdr))
  
res_acm = MCA(d_acm, quali.sup = 1, level.ventil = 0.03, graph = FALSE)

## Valeurs propres---------

# Combien d'axes conserver ?

barplot(res_acm$eig[,2], main="Histogramme des valeurs propres", 
        xlab="Axes", ylab="Pourcentage d'inertie", cex.axis=0.8, font.lab=3,
        col="orange")

# Le saut est après le 2e on dirait...

res_acm$var$contrib

## Nuage des modalités les plus contributives-----

seuil <- 100/nrow(res_acm$var$contrib) # contribution supérieure

moda <- which(res_acm$var$contrib[, 1]>seuil 
              | res_acm$var$contrib[, 2]>seuil)

plot.MCA(res_acm, invisible=c("ind","quali.sup"), 
         title="Nuage des modalites actives Plan 1-2", axes=c(1,2),
         selectMod=c(moda), cex=0.8)

## Nuage des individus---------

plot(res_acm, choix=c("ind"), invisible=c("var", "quali.sup"), cex=0.7, axes=c(1,2))

# CAH-------

## Réalisation------

res_cah <- HCPC(res_acm, nb.clust = 3, graph = FALSE)

## Visualisation des résultats---------

plot(res_cah, choice = "tree")

plot(res_cah, choice = "map", draw.tree = F)

## Description des clusters----------

# Quelles sont les variables et modalités qui contribuent le plus aux différents clusters ?

res_cah$desc.var

# Groupe 1 : enfants de cadre à l'heure scolairement
# Groupe 2 : enfants d'ouvriers et PI qui travaillent, légèrement "en retard"
# Groupe 3 : plus âgés, en reprise d'études, hommes, père cadre ou employé

# Quels sont les individus qui sont proches du centre d’un cluster et loin du centre des autres clusters ?

res_cah$desc.ind

# Fréquence par clusters 

freq(res_cah$data.clust$clust, digits = 0)

# Intégrer une variable qualitative "cluster" dans la base de données d'origine 
#qui precise dans quel cluster se situe l'individu 

d_acm$cluster <- res_cah$data.clust$clust
