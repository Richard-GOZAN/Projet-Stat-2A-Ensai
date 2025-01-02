# Importation des données de démonstration

demo <- read.csv("data/demof2.csv", sep = ";")
View(demo)

# Statistiques descriptives

install.packages("summarytools")
library(summarytools)
descr(demo)
