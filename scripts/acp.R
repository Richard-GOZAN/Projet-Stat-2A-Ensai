df = read.csv("data/data.csv")

data1 = df[, c(2, 4, 6, 7, 50, 95, 8, 9, 10, 38, 39, 40, 52 : 91)]

write.csv(data1, "data/data1.csv", row.names = FALSE)



# Charger les données
df<- read.csv("data/data1.csv")


# Définition des regroupements
age_groups <- list(
  "0_24"  = c("0_4", "5_9", "10_14", "15_19", "20_24"),
  "25_44" = c("25_29", "30_34", "35_39", "40_44"),
  "45_59" = c("45_49", "50_54", "55_59"),
  "60_plus" = c("60_64", "65_69", "70_74", "75_79", "80_84", "85_89", "90_94", "95_plus")
)

# Création des nouvelles colonnes pour chaque grande tranche d'âge
for (group in names(age_groups)) {
  hommes_cols <- intersect(paste0("hommes_", age_groups[[group]]), colnames(df))
  femmes_cols <- intersect(paste0("femmes_", age_groups[[group]]), colnames(df))
  
  if (length(hommes_cols) > 0 & length(femmes_cols) > 0) {
    df[[paste0("total_", group)]] <- rowSums(df[, hommes_cols, drop = FALSE], na.rm = TRUE) + 
      rowSums(df[, femmes_cols, drop = FALSE], na.rm = TRUE)
  } else {
    warning(paste("Aucune colonne trouvée pour le groupe", group))
  }
}

# Vérification de la colonne de population totale
if (!"population_municipale_2021_x" %in% colnames(df)) {
  stop("Erreur : la colonne population_municipale_2021_x est introuvable !")
}

# Calcul des proportions par tranche d'âge
for (group in names(age_groups)) {
  total_col <- paste0("total_", group)
  if (total_col %in% colnames(df)) {
    df[[paste0("prop_", group)]] <- df[[total_col]] / df$population_municipale_2021_x
  }
}

# Aperçu des résultats
head(df[, c("population_municipale_2021_x", grep("^total_|^prop_", names(df), value = TRUE))])



data2 <- df[, c("libelle_x", "population_municipale_2021_x",
                "taux_de_mortalite_annuel_moyen_2015_2021",
                "taux_de_natalite_annuel_moyen_2015_2021",
                "departement", "taux_visites",
                "taille_menage_moyenne_",
                #"part_des_familles_sans_enf_de_de_25_ans_2021",
                #"part_des_familles_avec_1_enf_de_de_25_ans_2021",
                #"part_des_familles_avec_2_enf_de_de_25_ans_2021",
                #"part_des_familles_avec_3_enf_ou_plus_de_de_25_ans_2021",
                "part_monoparentale",
                grep("^prop_", names(df), value = TRUE))]
write.csv(data2, "data/data2.csv", row.names = FALSE)


data_acp <- read.csv("data/data2.csv")

library(Factoshiny)
# PCAshiny(data_acp)

##############################################################

# Statistiques descriptives

summary(data_acp)

# Mise en oeuvre de l'ACP

res.pca <- PCA(data_acp, quali.sup=c(1, 5))

# Choix du nombre d'axes

barplot(res.pca$eig[,2],names=paste("Dim",1:nrow(res.pca$eig)))

# Analyse des résultats

summary(res.pca, ncp=2, nbelements=3)

# Graphe des individus

plot(res.pca, choix="ind", habillage=13, cex = 1.1,
     select="cos2 0.6", title="Graphe des individus")

plot(res.pca, choix="ind", habillage=13, axes=3:4, cex=0.7)

# Graphe des variables

plot(res.pca, choix="var", habillage=13, axes=1:2, new.plot=TRUE)

# Ellipses pour la variable qualitative mise en supplémentaire

plotellipses(res.pca)
