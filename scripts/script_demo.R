# Importation des données de démonstration

demo <- read.csv("data/demof2.csv", sep = ";")
View(demo)

# Statistiques descriptives

install.packages("summarytools")
library(summarytools)
descr(demo)

for(i in 1:12){
  cat("2x", i, "=", 2*i, "\n")
}