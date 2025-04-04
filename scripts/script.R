######## 1.Chargement des packages #######
library(stringi)
library(dplyr)
library(knitr)
library(kableExtra)
library(tidyr)
library(tidyverse)
library(summarytools)
library(gridExtra)
library(purrr)
library(skimr)
library(spdep)
library(geosphere)
library(ggplot2)
library(readr)
library(reshape2)
library(spatialreg)
library(car)
library(MASS)
library(ggrepel)
library(ggplot2)

###### 2.Importation des données de démonstration ######
demo <- read.csv("../data/demof2.csv", sep = ";", dec=",")
#View(demo)
#str(demo)
names(demo)[names(demo) == "Libellé"] <- "libelle_maj"

###### 3.Traitement ######

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
#names(demo)

# Fusion des bases et création des varaiables

## Importation de la base generalise
generalise <- read.csv("../data/generalise.csv", sep=";")
#str(generalise)

## Importation de la base pour les lon et lat manquantes
donnees_manquantes <- read.csv(
  "../data/communes_manquantes_latitudes_longitudes.csv", sep=";", dec=".")
#str(donnees_manquantes)

donnees_manquantes$longitude <- donnees_manquantes$longitude %>%
  str_replace_all(",", "") %>%  # Suppression les virgules
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


#nrow(demo)
#nrow(generalise)
#nrow(data)

## Filtrons les communes n'appartenant pas au département 97
data <- data %>% filter(departement != 97)

## Création de la variable taux de visites
data <- data %>% 
  mutate(taux_visites = nb_visite/population_municipale_2021_x)

## Création de la variabe taux de visites pour les plus de 19 ans
data <- data %>%
  mutate(pop_19_ans_ou_plus = pop_15_ans_ou_plus - pop_15_19_ans,
       taux_visites_19_ans_ou_plus = nb_visite / pop_19_ans_ou_plus)


## Exportation de la base finale 
write.csv(data, "../data/data.csv", row.names = FALSE)


df = data
colnames(df) <- gsub("homme_", "hommes_", colnames(df))
colnames(df) <- gsub("hommes_70_47$", "hommes_70_74", colnames(df))

## Sélection des colonnes de la pyramide des âges
age_groups <- c("0_4", "5_9", "10_14", "15_19", "20_24", "25_29", "30_34", "35_39",
                "40_44", "45_49", "50_54", "55_59", "60_64", "65_69", "70_74",
                "75_79", "80_84", "85_89", "90_94", "95_plus")

## Restructuration des données pour la visualisation
hommes_vars <- intersect(colnames(df), paste0("hommes_", age_groups))
femmes_vars <- intersect(colnames(df), paste0("femmes_", age_groups))


######## 4.Analyse descriptive #########

## Pyramide des âges avec les hommes d'abord, puis les femmes
pyramide <- data.frame(
  Age = rep(age_groups, 2),  # Liste tous les âges d'abord pour les hommes, puis pour les femmes
  Sexe = c(rep("Homme", length(age_groups)), rep("Femme", length(age_groups))),
  Population = c(-colSums(df[paste0("hommes_", age_groups)], na.rm=TRUE),
                 +colSums(df[paste0("femmes_", age_groups)], na.rm=TRUE))  # Hommes en négatif
)


## Conversion de l'Age en facteur ordonné pour garantir le bon ordre
pyramide$Age <- factor(pyramide$Age, levels = age_groups, ordered = TRUE)

ggplot(pyramide, aes(x=Age, y=Population, fill=Sexe)) +
  geom_bar(stat="identity", width=0.8) +
  coord_flip() +
  scale_y_continuous(labels = abs) +
  labs(
       x="Tranche d'âge",
       y="Population",
       fill="Sexe") +
  theme_minimal() +
  scale_fill_manual(values=c("pink", "blue"))  # Couleurs pour Femme/Homme


## Vérification et conversion des variables avant de tracer
## Vérification et conversion du taux de mortalité
if (class(data$taux_de_mortalite_annuel_moyen_2015_2021) != "numeric") {
  data$taux_de_mortalite_annuel_moyen_2015_2021 <- as.numeric(as.character(data$taux_de_mortalite_annuel_moyen_2015_2021))
}

## Vérification et conversion du taux de natalité
if (class(data$taux_de_natalite_annuel_moyen_2015_2021) != "numeric") {
  data$taux_de_natalite_annuel_moyen_2015_2021 <- as.numeric(as.character(data$taux_de_natalite_annuel_moyen_2015_2021))
}

##  Histogramme du taux de mortalité
p1 <- ggplot(data, aes(x = taux_de_mortalite_annuel_moyen_2015_2021)) +
  geom_histogram(bins = 30, fill = "red", alpha = 0.7, color = "black") +
  labs( 
       x = "Taux de mortalité", 
       y = "Nombre de communes") +
  theme_minimal()

##  Histogramme du taux de natalité
p2 <- ggplot(data, aes(x = taux_de_natalite_annuel_moyen_2015_2021)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7, color = "black") +
  labs(
       x = "Taux de natalité", 
       y = "Nombre de communes") +
  theme_minimal()

##  Nuage de points pour voir la relation entre mortalité et natalité
p3 <- ggplot(data, aes(x = taux_de_mortalite_annuel_moyen_2015_2021, 
                     y = taux_de_natalite_annuel_moyen_2015_2021)) +
  geom_point(alpha = 0.7, color = "purple") +
  geom_smooth(method = "lm", color = "black", linetype = "dashed") +  # Ajout d'une tendance linéaire
  labs(
       x = "Taux de mortalité",
       y = "Taux de natalité") +
  theme_minimal()

##  Courbes de densité pour mieux voir la distribution
p4 <- ggplot(data) +
  geom_density(aes(x = taux_de_mortalite_annuel_moyen_2015_2021, fill = "Mortalité"), alpha = 0.5, color = "red") +
  geom_density(aes(x = taux_de_natalite_annuel_moyen_2015_2021, fill = "Natalité"), alpha = 0.5, color = "blue") +
  labs(
       x = "Taux",
       y = "Densité") +
  scale_fill_manual(values = c("Mortalité" = "red", "Natalité" = "blue")) +
  theme_minimal()

##  Affichage de tous les graphiques ensemble
grid.arrange(p1, p2, p3, p4, ncol = 2)

## Création des groupes d'âge
data$pop_femmes <- data$femmes_0_4 + data$femmes_5_9 + data$femmes_10_14 + data$femmes_15_19 + data$femmes_20_24 + 
                    data$femmes_25_29 + data$femmes_30_34 + data$femmes_35_39 + data$femmes_40_44 + data$femmes_45_49 + 
                    data$femmes_50_54 + data$femmes_55_59 + data$femmes_60_64 + data$femmes_65_69 + data$femmes_70_74 + 
                    data$femmes_75_79 + data$femmes_80_84 + data$femmes_85_89 + data$femmes_90_94 + data$femmes_95_plus

data$femmes_0_24 <- (data$femmes_0_4 + data$femmes_5_9 + data$femmes_10_14 + data$femmes_15_19 + data$femmes_20_24) / data$pop_femmes
data$femmes_25_45 <- (data$femmes_25_29 + data$femmes_30_34 + data$femmes_35_39 + data$femmes_40_44) / data$pop_femmes
data$femmes_45_60 <- (data$femmes_45_49 + data$femmes_50_54 + data$femmes_55_59) / data$pop_femmes
data$femmes_60_plus <- (data$femmes_60_64 + data$femmes_65_69 + data$femmes_70_74 + data$femmes_75_79 + 
                         data$femmes_80_84 + data$femmes_85_89 + data$femmes_90_94 + data$femmes_95_plus) / data$pop_femmes

data$taux_natalite <- data$taux_de_natalite_annuel_moyen_2015_2021

## ---- Création des graphiques ----

g1 <- ggplot(data, aes(x = femmes_0_24, y = taux_natalite)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "0-24 ans",
       x = "Proportion de Femmes 0-24 ans",
       y = "Taux de Natalité") +
  theme_minimal()

g2 <- ggplot(data, aes(x = femmes_25_45, y = taux_natalite)) +
  geom_point(color = "red", alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "25-45 ans",
       x = "Proportion de Femmes",
       y = "Taux de Natalité") +
  theme_minimal()

g3 <- ggplot(data, aes(x = femmes_45_60, y = taux_natalite)) +
  geom_point(color = "green", alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "green") +
  labs(title = "45-60 ans",
       x = "Proportion de Femmes",
       y = "Taux de Natalité") +
  theme_minimal()

g4 <- ggplot(data, aes(x = femmes_60_plus, y = taux_natalite)) +
  geom_point(color = "purple", alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "purple") +
  labs(title = "60+ ans",
       x = "Proportion de Femmes",
       y = "Taux de Natalité") +
  theme_minimal()

## ----  Affichage en grille (2x2) ----
grid.arrange(g1, g2, g3, g4, ncol = 2, nrow = 2)


## Premier graphique pour nb_visite
plot1 <- ggplot(data) +
  aes(x = nb_visite) +
  geom_histogram(bins = 30L, fill = "gray") +
  theme_minimal() +
  ylab("Nombre de communes") +
  xlab("Nombre de consultations")

## Deuxième graphique pour taux_visites_19_ans_ou_plus
plot2 <- ggplot(data) +
  aes(x = taux_visites_19_ans_ou_plus) +
  geom_histogram(bins = 30L, fill = "gray") +
  theme_minimal() +
  ylab("Nombre de communes") +
  xlab("Taux de consultations")

## Combinaison des deux graphiques
plot1 + plot2
# Stats desc sur le nombre de visites
stats <- summary(data$nb_visite) 

## Sous forme de data frame
summary_df <- data.frame(
  Statistique = names(stats),
  Valeur = as.numeric(stats)
  ) %>%
  pivot_wider(names_from = Statistique, values_from = Valeur) 

## Génération du tableau en LaTeX
summary_df %>% kable(format = "latex", 
                     booktabs = TRUE,
                     caption = "Résumé statistique du nombre de visites") %>%
  kable_styling(latex_options = c("striped",
                                  "HOLD_position"))
## Calcul des quantiles
quantiles <- quantile(data$population_municipale_2021_x, probs = c(1/3, 2/3), na.rm = TRUE)

## Créeation des classes avec les bornes des intervalles
data_pop <- data %>%
  mutate(taille_commune = case_when(
    population_municipale_2021_x <= quantiles[1] ~ paste0("Petite (<= ", round(quantiles[1]), ")"),
    population_municipale_2021_x <= quantiles[2] ~ paste0("Moyenne (", round(quantiles[1] + 1), " - ", round(quantiles[2]), ")"),
    TRUE ~ paste0("Grande (> ", round(quantiles[2]), ")")
  )) %>%
  group_by(taille_commune) %>%
  summarise("Taux de consulations"= mean(taux_visites, na.rm = TRUE))

## Génération du tableau en LaTeX
data_pop %>% kable(format = "latex", 
                     booktabs = TRUE,
                     caption = "Taux de consultations selon la taille de la commune") %>%
  kable_styling(latex_options = c("striped",
                                  "HOLD_position"))
## Calcul de la médiane
mediane <- median(data$nb_de_pers_agees_de_75_ans_ou_plus_2021, na.rm = TRUE)

## Créeation des classes avec les bornes des intervalles
data_age <- data %>%
  mutate(population_agee_importante = case_when(
    nb_de_pers_agees_de_75_ans_ou_plus_2021 <= mediane ~ paste0("Non (<= ", round(mediane), ")"),
    TRUE ~ paste0("Oui (> ", round(mediane), ")")
  )) %>%
  group_by(population_agee_importante) %>%
  summarise(consultations_moyennes = mean(taux_visites, na.rm = TRUE))

## Génération du tableau en LaTeX
data_age %>% kable(format = "latex", 
                     booktabs = TRUE,
                     caption = "Taux de consultations selon la population âgée") %>%
  kable_styling(latex_options = c("striped",
                                  "HOLD_position"))




# Standarisation des variables pour prendre en compte l'effet de la taille des variables et rendre les comparaisons plus équitables
data_csp <- data %>%
  mutate(across(c(taux_visites, 13:20), scale))


# Création les graphiques
plot1 <- ggplot(data_csp, aes(x = population_de_15_ans_ou_selon_la_csp_2021_agriculteurs_exploitants, y = taux_visites)) +
  geom_point() +
  labs(title = "", x = "Agriculteurs", y = "Taux de visites")

plot2 <- ggplot(data_csp, aes_string(x = names(data)[14], y = "taux_visites")) +
  geom_point() +
  labs(title = "", x = "Artisans", y = "Taux de visites")

plot3 <- ggplot(data_csp, aes_string(x = names(data)[15], y = "taux_visites")) +
  geom_point() +
  labs(title = "", x = "Cadres", y = "Taux de visites")

plot4 <- ggplot(data_csp, aes_string(x = names(data)[16], y = "taux_visites")) +
  geom_point() +
  labs(title = "", x = "Professions intermédiaires", y = "Taux de visites")

plot5 <- ggplot(data_csp, aes_string(x = names(data)[17], y = "taux_visites")) +
  geom_point() +
  labs(title = "", x = "Employés", y = "Taux de visites")

plot6 <- ggplot(data_csp, aes_string(x = names(data)[18], y = "taux_visites")) +
  geom_point() +
  labs(title = "", x = "Ouvriers", y = "Taux de visites")

plot7 <- ggplot(data_csp, aes_string(x = names(data)[19], y = "taux_visites")) +
  geom_point() +
  labs(title = "", x = "Retraités", y = "Taux de visites")

plot8 <- ggplot(data_csp, aes_string(x = names(data)[20], y = "taux_visites")) +
  geom_point() +
  labs(title = "", x = "Sans activites", y = "Taux de visites")

## Affichage des graphiques ensemble
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, ncol = 4)

# Définition des noms lisibles pour les variables
nom_variables <- c(
  "taux_de_mortalite_annuel_moyen_2015_2021" = "Mortalité",
  "taux_de_natalite_annuel_moyen_2015_2021" = "Natalité",
  "part_des_familles_sans_enf_de_de_25_ans_2021" = "Sans enfants",
  "part_des_familles_avec_1_enf_de_de_25_ans_2021" = "Un enfant",
  "part_des_familles_avec_3_enf_ou_plus_de_de_25_ans_2021" = "Trois enfants",
  "nb_visite" = "Nombre de visites"
)

#  Sélection des variables d'analyse
variables_analyse <- names(nom_variables)

df_analyse <- df[variables_analyse]

#  Conversion de toutes les colonnes en numérique
df_analyse <- df_analyse %>% mutate(across(everything(), as.numeric))

# Suppression les valeurs manquantes
df_analyse <- na.omit(df_analyse)  

#  Calcul des corrélations
cor_matrix <- cor(df_analyse, use="complete.obs")

# Tri,des corrélations par ordre décroissant
cor_target <- sort(cor_matrix["nb_visite", ], decreasing=TRUE)

#  Remplacement des noms de variables par des noms plus lisibles
cor_data <- data.frame(
  Variable = names(cor_target),
  Correlation = cor_target
)

# Application les nouveaux noms
cor_data$Variable <- nom_variables[cor_data$Variable]

# Exclure "Nombre de visites" du graphique
cor_data <- cor_data[cor_data$Variable != "Nombre de visites", ]

# Barplot des corrélations
ggplot(cor_data, aes(x = reorder(Variable, Correlation), y = Correlation, fill = Correlation)) +
  geom_bar(stat="identity") +
  coord_flip() +
  scale_fill_gradient2(low="blue", mid="white", high="red", midpoint=0) +
  labs(
       x="Variables",
       y="Coefficient de corrélation") +
  theme_minimal()

# Importation des données
data <- read.csv("../data/data.csv", sep = ",", dec=".")

coords <- data.frame(lon = data$longitude, lat = data$latitude)
distance_matrix <- distm(coords, fun = distHaversine) / 1000 # Pour avoir les distances en Km
# Identification des paires de communes où la distance dépasse 2000 km
indices <- which(distance_matrix > 2000, arr.ind = TRUE)

# Extraction de la colonne des indices des lignes
ligne_indices <- indices[, 1]

# Suppresssion des doublons
ligne_indices_unique <- unique(ligne_indices)

# Récupération des lignes concernées
indices_concernes <- ligne_indices_unique[1:7]
# Mise à jour des coordonnées géographiques
data[1973, c(93, 94)] <- c(2.775222, 50.41792)
data[72, c(93, 94)] <- c(6.231506, 44.09108)
data[399, c(93, 94)] <- c(1.560560, 45.16898)
data[1264, c(93, 94)] <- c(-1.486812, 47.29624)
data[1947, c(93, 94)] <- c(1.623721, 50.48337)
data[2124, c(93, 94)] <- c(2.916621, 42.74637)
data[2746, c(93, 94)] <- c(2.376579, 43.49010)
coords <- data.frame(lon = data$longitude, lat = data$latitude)
distance_matrix <- distm(coords, fun = distHaversine) / 1000 # Pour avoir les distances en Km
d_max <- 500

# Calcul de la matrice de poids
matrice_poids <- ifelse(distance_matrix <= d_max & distance_matrix > 0, 1 / distance_matrix, 0)
  
# Normalisation par ligne
#matrice_poids_norm <- matrice_poids / rowSums(matrice_poids)

### Matrice de voisinage
dist.w <- mat2listw(matrice_poids, style = "W", row.names=NULL)


# Calcul du taux moyen des voisins
data$taux_voisins <- lag.listw(dist.w, data$taux_visites)


# Moyennes pour séparer les quadrants
mean_x <- mean(data$taux_visites, na.rm = TRUE)
mean_y <- mean(data$taux_voisins, na.rm = TRUE)

# Attribution des quadrants
data <- data %>%
  mutate(quadrant = case_when(
    taux_visites >= mean_x & taux_voisins >= mean_y ~ "HH",
    taux_visites >= mean_x & taux_voisins < mean_y ~ "HL",
    taux_visites < mean_x & taux_voisins >= mean_y ~ "LH",
    taux_visites < mean_x & taux_voisins < mean_y ~ "LL"
  ))

# Sélection de 4 communes atypiques par quadrant
set.seed(123)  # Pour la reproductibilité
communes_atypiques <- data %>%
  group_by(quadrant) %>%
  slice_sample(n = 4)

# Création du Moran Plot avec labels améliorés
ggplot(data, aes(x = taux_visites, y = taux_voisins)) +
  geom_point(color = "blue", alpha = 0.6) + 
  geom_vline(xintercept = mean_x, linetype = "dashed") +
  geom_hline(yintercept = mean_y, linetype = "dashed") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  geom_text_repel(data = communes_atypiques, 
                  aes(label = libelle_x, color = quadrant), 
                  size = 3, max.overlaps = 100, fontface = "bold") +
  labs(
       x = "Taux de consultations par commune",
       y = "Taux de consultation moyen des communes voisines") +
  theme_minimal() +
  scale_color_manual(values = c("HH" = "red", "HL" = "purple", "LH" = "green", "LL" = "orange"))
# Calcul du LISA (Moran Local)
lisa_results <- localmoran(data$taux_visites, dist.w)

# Ajouter les résultats aux données
data$LISA_I <- lisa_results[,1]  # Indice de Moran local<
data$p_value <- lisa_results[,5] # Valeur p

# Moyenne des taux de consultation
mean_consult <- mean(data$taux_visites)

# Quadrants de LISA
data$quadrant <- with(data, ifelse(taux_visites > mean_consult & LISA_I > 0, "HH",
                                   ifelse(taux_visites < mean_consult & LISA_I > 0, "LH",
                                   ifelse(taux_visites > mean_consult & LISA_I < 0, "HL",
                                   "LL"))))

ggplot(data, aes(x = taux_visites, y = LISA_I, color = quadrant)) +
  geom_point(alpha = 0.6, size = 3) +
  scale_color_manual(values = c("HH" = "red", "LH" = "green", "HL" = "purple", "LL" = "orange")) +
  geom_text_repel(aes(label = ifelse(p_value < 0.1, as.character(libelle_x), "")), size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = mean_consult, linetype = "dashed") +
  ggtitle("Analyse LISA (Moran Local) des taux de consultation") +
  theme_minimal()


# Importation des données
data <- read.csv("../data/data.csv", sep = ",", dec=".")
# Renommer la colonne de population totale
colnames(data)[which(colnames(data) == "population_municipale_2021_x")] <- "population_totale"

# Regroupement des âges
data$pop_0_24 <- rowSums(data[, c("homme_0_4", "femmes_0_4", "hommes_5_9", "femmes_5_9", 
                                  "hommes_10_14", "femmes_10_14", "hommes_15_19", "femmes_15_19", 
                                  "hommes_20_24", "femmes_20_24")], na.rm = TRUE)

data$pop_25_64 <- rowSums(data[, c("hommes_25_29", "femmes_25_29", "hommes_30_34", "femmes_30_34", 
                                   "hommes_35_39", "femmes_35_39", "hommes_40_44", "femmes_40_44", 
                                   "hommes_45_49", "femmes_45_49", "hommes_50_54", "femmes_50_54", 
                                   "hommes_55_59", "femmes_55_59", "hommes_60_64", "femmes_60_64")], na.rm = TRUE)

data$pop_65_plus <- rowSums(data[, c("homme_65_69", "femmes_65_69", "hommes_70_47", "femmes_70_74", 
                                     "hommes_75_79", "femmes_75_79", "hommes_80_84", "femmes_80_84", 
                                     "hommes_85_89", "femmes_85_89", "hommes_90_94", "femmes_90_94", 
                                     "hommes_95_plus", "femmes_95_plus")], na.rm = TRUE)

# Calcul des pourcentages
data$pct_pop_0_24 <- (data$pop_0_24 / data$population_totale) * 100
data$pct_pop_25_64 <- (data$pop_25_64 / data$population_totale) * 100
data$pct_pop_65_plus <- (data$pop_65_plus / data$population_totale) * 100

data$pct_union_libre <- data$part_d_en_concubinage_ou_union_libre # Déjà en pourcentage

data$pct_ouvriers <- (data$population_de_15_ans_ou_selon_la_csp_2021_ouvriers / data$population_totale) * 100

data$pct_sans_emploi <- (data$population_de_15_ans_ou_selon_la_csp_2021_autres_personnes_sans_activite_professionnelle /
                        (data$pop_0_24 + data$pop_25_64)) * 100

data$pct_fam_3_enfants_plus <- data$part_des_familles_avec_3_enf_ou_plus_de_de_25_ans_2021 # Déjà en pourcentage

# Création de la base finale
data_modelisation <- data[, c("code", "libelle_x", "pct_pop_0_24", "pct_pop_25_64", "pct_pop_65_plus",
                              "pct_union_libre", "pct_ouvriers", "pct_sans_emploi", "pct_fam_3_enfants_plus",
                              "taux_de_natalite_annuel_moyen_2015_2021", "taux_de_mortalite_annuel_moyen_2015_2021")]

# Sauvegarde en CSV
write.csv(data_modelisation, "../data/data_modelisation.csv", row.names = FALSE)

###### 5. Modélisation #######

attach(data)

### Modèle estimé


modele <- taux_visites ~
  pct_pop_0_24 + 
  pct_pop_25_64+ 
  pct_pop_65_plus +
  pct_union_libre +
  pct_ouvriers + 
  pct_sans_emploi+
  pct_fam_3_enfants_plus +
  taux_de_natalite_annuel_moyen_2015_2021
  

### Modèle MCO
ze.lm <- lm(modele, data=data)
summary_lm <- summary(ze.lm)
summary_lm

### Test de Moran adapté sur les résidus
matrice <- dist.w
moran_lm <- lm.morantest(ze.lm, matrice)
moran_lm
moran_table <- data.frame(
  Statistique = c("Observed Moran I", "Expectation", "Variance", "statistic standard deviate", "p-value"),
  Valeur = c(moran_lm$estimate[1],
             moran_lm$estimate[2],
             moran_lm$estimate[3],
             moran_lm$statistic,
             moran_lm$p.value)
)%>%
  pivot_wider(names_from = Statistique, values_from = Valeur)



moran_table %>% kable(format = "latex",
                     booktabs = TRUE,
                     align = "c",
                     caption = "Résultats du Test de Moran sur les résidus du modèle MCO") %>%
  kable_styling(latex_options = c("striped",
                                  "HOLD_position"))
### Test LM-Error et LM-Lag
lm_tests <- lm.RStests(ze.lm, matrice, test = "all")
lm_tests
lm_tests_results <- data.frame(
  Valeur = c(lm_tests$RSerr$statistic, lm_tests$RSlag$statistic,
                  lm_tests$adjRSerr$statistic, lm_tests$adjRSlag$statistic,
                  lm_tests$SARMA$statistic),
  p_value = c(lm_tests$RSerr$p.value, lm_tests$RSlag$p.value,
               lm_tests$adjRSerr$p.value, lm_tests$adjRSlag$p.value,
               lm_tests$SARMA$p.value),
  df = c(lm_tests$RSerr$parameter, lm_tests$RSlag$parameter,
         lm_tests$adjRSerr$parameter, lm_tests$adjRSlag$parameter,
         lm_tests$SARMA$parameter)
)


lm_tests_results %>% kable(format = "latex",
                     booktabs = TRUE,
                     caption = "Résultats des diagnostics spatiaux (tests LM)") %>%
  kable_styling(latex_options = c("striped",
                                  "HOLD_position"))
### Modèle SEM
ze.sem<-errorsarlm(modele, data=data, matrice)
summary_sem <- summary(ze.sem)
summary_sem


# coefficients <- summary_sem$Coef
# colnames(coefficients) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
# 
# # Tableau des coefficients
# coefficients %>% kable(format = "latex", 
#                      booktabs = TRUE, 
#                      caption = "Résultats des coefficients principaux du modèle SAR") %>%
#   kable_styling(latex_options = c("striped",
#                                   "HOLD_position"))
# 

### Modèle SAR
ze.sar<-lagsarlm(modele, data=data, matrice)
summary_sar <- summary(ze.sar)
summary_sar

# coefficients <- summary_sar$Coef
# colnames(coefficients) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
# 
# 
# coefficients %>% kable(format = "latex", 
#                      booktabs = TRUE, 
#                      caption = "Résultats des coefficients principaux du modèle SAR") %>%
#   kable_styling(latex_options = c("striped",
#                                   "HOLD_position"))
ze.slx <- lmSLX(modele, data = data, matrice)
summary_slx <- summary(ze.slx)
summary_slx

# # Tableau des coeffiients
# 
# coefficients <- summary_slx$coefficients
# colnames(coefficients) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
# 
# coefficients %>% kable(format = "latex", 
#                      booktabs = TRUE, 
#                      caption = "Résultats des coefficients principaux du modèle SDM") %>%
#   kable_styling(latex_options = c("striped",
#                                   "HOLD_position"))


### Modèle SDM
ze.sardm<-lagsarlm(modele, data=data, matrice, type="mixed")
summary_sdm <-summary(ze.sardm)
summary_sdm



# Calcul des AIC pour chaque modèle
aic_values <- data.frame(
  Model = c("MCO", "SEM", "SAR", "SLX", "SDM"),
  AIC = c(AIC(ze.lm), AIC(ze.sem), AIC(ze.sar), AIC(ze.slx), AIC(ze.sardm)),
  LogLik = c(logLik(ze.lm), logLik(ze.sem), logLik(ze.sar), logLik(ze.slx), logLik(ze.sardm))
)


aic_values %>%
  kable(format = "latex", 
        booktabs = TRUE, 
        caption = "Comparaison des des différents modèles") %>%
  kable_styling(latex_options = c("striped", "hold_position"))

# Fonction pour ajuster le modèle spatial et calculer AIC/BIC
fit_spatial_model <- function(variables, data, matrice) {
  formula <- as.formula(paste("taux_visites ~", paste(variables, collapse = " + ")))

  model <- lagsarlm(formula, data = data, listw = matrice, type = "mixed")

  return(list(AIC = AIC(model), BIC = BIC(model), model = model))
}

# Liste des variables explicatives
variables <- c(
  "pct_pop_0_24",
  "pct_pop_25_64",
  "pct_pop_65_plus",
  "pct_union_libre",
  "pct_ouvriers",
  "pct_sans_emploi",
  "pct_fam_3_enfants_plus",
  "taux_de_natalite_annuel_moyen_2015_2021"
)

# Combinaisons possibles de variables
combinations <- unlist(lapply(1:length(variables), function(n) combn(variables, n, simplify = FALSE)), recursive = FALSE)

# Application de la fonction sur toutes les combinaisons
results <- lapply(combinations, function(vars) {
  fit_spatial_model(vars, data = data, matrice = dist.w)
})

# Identification avec l'AIC minimum
best_index <- which.min(sapply(results, function(res) res$AIC))
best_result <- results[[best_index]]
best_variables <- combinations[[best_index]]

# Data.frame avec les résultats
results_df <- data.frame(
  Variables = sapply(combinations, function(vars) paste(vars, collapse = ", ")),
  AIC = sapply(results, function(res) res$AIC),
  BIC = sapply(results, function(res) res$BIC)
)
sorted_results <- results_df[order(results_df$AIC), ]


modele_final <- taux_visites ~
  pct_pop_25_64+ 
  pct_pop_65_plus +
  pct_union_libre +
  pct_ouvriers + 
  pct_sans_emploi+
  pct_fam_3_enfants_plus +
  taux_de_natalite_annuel_moyen_2015_2021

### Modèle SDM
ze.sardm<-lagsarlm(modele_final, data=data, matrice, type="mixed")
summary_sdm <-summary(ze.sardm)


# Tableau des coeffiients

coefficients <- summary_sdm$Coef
colnames(coefficients) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")

coefficients %>% kable(format = "latex",
                     booktabs = TRUE,
                     caption = "Résultats des coefficients principaux du modèle SDM") %>%
  kable_styling(latex_options = c("striped",
                                  "HOLD_position"))
# Prédictions
predictions <- predict(ze.sardm, listw=matrice, newdata=data)  
data$taux_visites_predicted <- predictions
write.csv(data, "../data/data_prediction.csv", row.names = FALSE)



# Création du tableau des résultats pour le coefficient rho
rho_results <- data.frame(
  Statistique = c("Rho", "LR test value", "p-value", "Asymptotic standard error"),
  Valeur = c(summary_sdm$rho, summary_sdm$LR1$statistic, summary_sdm$LR1$p.value,
             summary_sdm$rho.se)
) %>% pivot_wider(names_from = Statistique, values_from = Valeur)

rho_results %>%
  kable(format = "latex", 
        booktabs = TRUE, 
        caption = "Résultats du coefficient Rho dans le modèle SDM") %>%
  kable_styling(latex_options = c("striped", "hold_position"))



coefficients <- summary_sdm$Coef[, 1]  
coeff_names <- rownames(summary_sdm$Coef) 

coeff_df <- data.frame(
  Variable = coeff_names,
  Coefficient = coefficients
)
coeff_df <- coeff_df[coeff_df$Variable != "(Intercept)", ]  # intercept exclue

# Tri par ordre décroissant des coefficients (valeurs absolues)
coeff_df <- coeff_df[order(abs(coeff_df$Coefficient), decreasing = TRUE), ]


# Barplot horizontal 
ggplot(coeff_df, aes(x = abs(Coefficient), y = reorder(Variable, abs(Coefficient)))) +
  geom_bar(stat = "identity", fill = "blue") +
  scale_x_continuous(limits = c(0, max(abs(coeff_df$Coefficient)) + 0.01)) +  # Échelle positive
  labs(x = "Valeur absolue du coefficient estimé",
       y = "Variables explicatives") +
  theme_minimal()
