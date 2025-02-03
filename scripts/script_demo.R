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
data <- read.csv("data/data.csv", sep = ",", dec=",")
View(demo)
str(demo)
names(demo)[names(demo) == "Libellé"] <- "libelle_maj"
data = demo
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


# Charger les bibliothèques nécessaires
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(dplyr)

# Charger les données (remplace "Test.csv" par ton vrai fichier)
df <- read.csv("data/data.csv", sep = ",", dec=",")

# Vérifier la structure des données
str(df)

# Conversion des colonnes en numérique si nécessaire
df_numeric <- df %>%
  select(where(is.numeric)) %>%  # Sélectionne les colonnes numériques
  select(-nb_visite)             # Exclut la variable cible

# Standardiser les données pour l'ACP
df_scaled <- scale(df_numeric)

# Effectuer l'ACP
pca_result <- PCA(df_scaled, scale.unit=TRUE, graph=FALSE)

# Afficher le pourcentage de variance expliquée
fviz_eig(pca_result, addlabels=TRUE, ylim=c(0,100))

# Afficher la contribution des variables aux deux premières composantes principales
fviz_pca_var(pca_result, col.var="contrib", gradient.cols=c("blue", "red"), repel=TRUE)

# -------------------------------------------------
# 📌 ANALYSE BIVARIÉE : Corrélation avec nb_visite
# -------------------------------------------------
# Ajouter la variable cible
df_numeric$nb_visite <- df$nb_visite

# Calculer les corrélations
cor_matrix <- cor(df_numeric, use="complete.obs")

# Trier les variables les plus corrélées avec nb_visite
cor_target <- sort(cor_matrix["nb_visite",], decreasing=TRUE)

# Afficher les 10 variables les plus corrélées avec nb_visite
print(cor_target[1:30])

# Visualisation des corrélations sous forme de heatmap
corrplot(cor_matrix, method="color", type="upper", tl.col="black", tl.srt=45)


# Charger les bibliothèques nécessaires
library(ggplot2)
library(dplyr)
library(tidyr)

colnames(df) <- gsub("homme_", "hommes_", colnames(df))
colnames(df) <- gsub("hommes_70_47$", "hommes_70_74", colnames(df))


# Charger les données (remplace "Test.csv" par ton fichier)
df <- read.csv("Test.csv", sep="\t", header=TRUE)

# Sélectionner les colonnes de la pyramide des âges
age_groups <- c("0_4", "5_9", "10_14", "15_19", "20_24", "25_29", "30_34", "35_39",
                "40_44", "45_49", "50_54", "55_59", "60_64", "65_69", "70_74",
                "75_79", "80_84", "85_89", "90_94", "95_plus")

# Restructurer les données pour la visualisation
hommes_vars <- intersect(colnames(df), paste0("hommes_", age_groups))
femmes_vars <- intersect(colnames(df), paste0("femmes_", age_groups))

# Créer la pyramide des âges avec les hommes d'abord, puis les femmes
pyramide <- data.frame(
  Age = rep(age_groups, 2),  # Liste tous les âges d'abord pour les hommes, puis pour les femmes
  Sexe = c(rep("Homme", length(age_groups)), rep("Femme", length(age_groups))),
  Population = c(colSums(df[paste0("hommes_", age_groups)], na.rm=TRUE),
                 -colSums(df[paste0("femmes_", age_groups)], na.rm=TRUE))  # Femmes en négatif
)



ggplot(pyramide, aes(x=Age, y=Population, fill=Sexe)) +
  geom_bar(stat="identity", width=0.8) +
  coord_flip() +  # Pour afficher en pyramide
  scale_y_continuous(labels = abs) +  # Afficher les valeurs absolues
  labs(title="Pyramide des âges",
       x="Tranche d'âge",
       y="Population",
       fill="Sexe") +
  theme_minimal() +
  scale_fill_manual(values=c("blue", "pink"))  # Couleurs pour Homme/Femme


## D'autres analyses

# Charger les bibliothèques nécessaires
library(ggplot2)
library(dplyr)
library(corrplot)

# Sélectionner uniquement les variables d'intérêt
variables_analyse <- c("taux_de_mortalite_annuel_moyen_2015_2021", 
                       "taux_de_natalite_annuel_moyen_2015_2021", 
                       "part_des_familles_sans_enf_de_de_25_ans_2021", 
                       "part_des_familles_avec_1_enf_de_de_25_ans_2021", 
                       "part_des_familles_avec_3_enf_ou_plus_de_de_25_ans_2021", 
                       "nb_visite")

df_analyse <- df[variables_analyse]
# Convertir toutes les colonnes en numérique
df_analyse <- df_analyse %>% mutate(across(everything(), as.numeric))

# Vérifier les valeurs manquantes et les gérer si besoin
df_analyse <- na.omit(df_analyse)  

# Calculer les corrélations entre nb_visite et les autres variables
cor_matrix <- cor(df_analyse, use="complete.obs")

# Trier les corrélations par ordre décroissant
cor_target <- sort(cor_matrix["nb_visite",], decreasing=TRUE)

# Afficher le top des corrélations
print(cor_target)

# Visualiser les corrélations sous forme de barplot
ggplot(data = data.frame(Variable = names(cor_target), Correlation = cor_target), 
       aes(x = reorder(Variable, Correlation), y = Correlation, fill = Correlation)) +
  geom_bar(stat="identity") +
  coord_flip() +
  scale_fill_gradient2(low="blue", mid="white", high="red", midpoint=0) +
  labs(title="Corrélations entre le nombre de visite et les autres variables",
       x="Variables",
       y="Coefficient de corrélation") +
  theme_minimal()


################################################
### MORTALITE ET NATALITE########################"

# Charger les bibliothèques nécessaires
library(ggplot2)
library(gridExtra)  # Pour afficher plusieurs graphiques ensemble

# 📌 1️⃣ Histogramme du taux de mortalité
p1 <- ggplot(df, aes(x = taux_de_mortalite_annuel_moyen_2015_2021)) +
  geom_histogram(bins = 30, fill = "red", alpha = 0.7, color = "black") +
  labs(title = "Distribution du taux de mortalité (2015-2021)", 
       x = "Taux de mortalité moyen", 
       y = "Nombre de communes") +
  theme_minimal()

# 📌 2️⃣ Histogramme du taux de natalité
p2 <- ggplot(df, aes(x = taux_de_natalite_annuel_moyen_2015_2021)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7, color = "black") +
  labs(title = "Distribution du taux de natalité (2015-2021)", 
       x = "Taux de natalité moyen", 
       y = "Nombre de communes") +
  theme_minimal()

# 📌 3️⃣ Nuage de points pour voir la relation entre mortalité et natalité
p3 <- ggplot(df, aes(x = taux_de_mortalite_annuel_moyen_2015_2021, 
                     y = taux_de_natalite_annuel_moyen_2015_2021)) +
  geom_point(alpha = 0.7, color = "purple") +
  geom_smooth(method = "lm", color = "black", linetype = "dashed") +  # Ajout d'une tendance linéaire
  labs(title = "Relation entre taux de mortalité et taux de natalité",
       x = "Taux de mortalité moyen (2015-2021)",
       y = "Taux de natalité moyen (2015-2021)") +
  theme_minimal()

# 📌 4️⃣ Courbes de densité pour mieux voir la distribution
p4 <- ggplot(df) +
  geom_density(aes(x = taux_de_mortalite_annuel_moyen_2015_2021, fill = "Mortalité"), alpha = 0.5, color = "red") +
  geom_density(aes(x = taux_de_natalite_annuel_moyen_2015_2021, fill = "Natalité"), alpha = 0.5, color = "blue") +
  labs(title = "Distribution des taux de mortalité et natalité", 
       x = "Taux",
       y = "Densité") +
  scale_fill_manual(values = c("Mortalité" = "red", "Natalité" = "blue")) +
  theme_minimal()

# Afficher tous les graphiques ensemble
grid.arrange(p1, p2, p3, p4, ncol = 2)

