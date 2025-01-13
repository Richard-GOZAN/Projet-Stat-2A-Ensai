# Chargement des packages

library(stringi)
library(dplyr)
library(tidyverse)
library(summarytools)
library(gridExtra)
library(purrr)

# Importation des données de démonstration

demo <- read.csv("data/demof2.csv", sep = ";", dec=",")
View(demo)
str(demo)
names(demo)[names(demo) == "Libellé"] <- "libelle_maj"

## Fonction prenant en entrée un base et nettoie les noms des colonnes

nettoyer_noms_colonnes <- function(data){
  names(data) <- names(data) %>%
    stri_trans_general("Latin-ASCII") %>% # Suppression des accents
    gsub("\\s+", "_", .) %>% # Remplacement des espaces par des underscores
    gsub("\\.+", "_", .) %>% # Remplacement des points par des underscores
    tolower() # Conversion en minuscules
  return (data) 
}

## Nettoyage des colonnes de la base demo
demo <- nettoyer_noms_colonnes(demo)
names(demo)

## Filtrons les communes n'appartenant pas du département 97
demo <- demo %>% filter(departement != 97)



# Statistiques descriptives

descr(demo)



# Fusion des bases et création des varaiables

## Importation de la base generalise
generalise <- read.csv("data/generalise.csv", sep=";")

## Nettoyage dans les noms des colonnes
generalise <- nettoyer_noms_colonnes(generalise)

## Fusion
data <- demo %>% 
  inner_join(generalise, by ="code")

nrow(demo)
nrow(generalise)
nrow(data)

## Création de la variable taux de visites
data <- data %>% 
  mutate(taux_visites = nb_visite/population_municipale_2021_x)

## Création de la variabe taux de visites pour les plus de 19 ans
data <- data %>%
  mutate(pop_19_ans_ou_plus = pop_15_ans_ou_plus - pop_15_19_ans,
       taux_visites_19_ans_ou_plus = nb_visite / pop_19_ans_ou_plus)

summary(data$taux_visites)
summary(data$taux_visites_19_ans_ou_plus)


## Exportation de la base finale 
write.csv(data, "data/data.csv", row.names = FALSE)

