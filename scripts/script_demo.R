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

# Statistiques descriptives

descr(demo)

# Visualisations


## Colonnes à visualiser
numeric_cols <- demo %>% select(where(is.numeric))

numeric_plots <- map(names(numeric_cols), ~{
  ggplot(demo, aes(x=.data[[.x]]))+
    geom_histogram(fill="blue", color="black")+
    theme_minimal()+
    labs(x=.x, y="Fréquence")
})

## Divisons les plots en groupes de 6 et appliquons grid.arrange sur chaque groupe

split_plots <- split(numeric_plots, ceiling(seq_along(numeric_plots) / 6))

lapply(split_plots, function(plots) {
  grid.arrange(grobs = plots, ncol = 2)
})

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

## Données pour les cartes
selected_data <- data %>%
  select(code, libelle, longitude, latitude, nb_visite, taux_visites, taux_visites_19_ans_ou_plus, population_municipale_2021_x, pop_19_ans_ou_plus)

## Exportation
write.csv(selected_data, "data/data_communes.csv", row.names = FALSE)

