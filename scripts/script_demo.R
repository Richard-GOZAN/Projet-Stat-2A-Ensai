# Chargement des packages

library(stringi)
library(dplyr)
library(tidyverse)
library(summarytools)
library(gridExtra)
library(purrr)
library(skimr)

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


# Fusion des bases et création des varaiables

## Importation de la base generalise
generalise <- read.csv("data/generalise.csv", sep=";")
str(generalise)

## Importation de la base pour les lon et lat manquantes
donnees_manquantes <- read.csv(
  "data/communes_manquantes_latitudes_longitudes.csv", sep=";", dec=".")
str(donnees_manquantes)

donnees_manquantes$longitude <- donnees_manquantes$longitude %>%
  str_replace_all(",", "") %>%  # Supprime les virgules
  as.numeric()

## Nettoyage dans les noms des colonnes
generalise <- nettoyer_noms_colonnes(generalise)
donnees_manquantes <- nettoyer_noms_colonnes(donnees_manquantes)

## Fusion des bases
data <- demo %>% 
  inner_join(generalise, by ="code") %>%
  left_join(donnees_manquantes, by = "code") %>%
  mutate(
    longitude = ifelse(is.na(longitude.x), longitude.y, longitude.x),
    latitude = ifelse(is.na(latitude.x), latitude.y, latitude.x)
  ) %>%
  select(-longitude.x, -longitude.y, -latitude.x, -latitude.y)


nrow(demo)
nrow(generalise)
nrow(data)

## Filtrons les communes n'appartenant pas au département 97
data <- data %>% filter(departement != 97)



## Création de la variable taux de visites
data <- data %>% 
  mutate(taux_visites = nb_visite/population_municipale_2021_x)

## Création de la variabe taux de visites pour les plus de 19 ans
data <- data %>%
  mutate(pop_19_ans_ou_plus = pop_15_ans_ou_plus - pop_15_19_ans,
       taux_visites_19_ans_ou_plus = nb_visite / pop_19_ans_ou_plus)

summary(data$taux_visites)
summary(data$taux_visites_19_ans_ou_plus)

skim(data)

## Exportation de la base finale 
write.csv(data, "data/data.csv", row.names = FALSE)

## Statistiques descriptives sur le nombre de visite

summary(data$nb_visite)

ggplot(data) +
  aes(x = nb_visite) +
  geom_histogram(bins = 30L, fill = "gray") +
  theme_minimal() +
  ggtitle(label = "Distribution du nombre de visites par commune") +
  ylab("") +
  xlab("")
