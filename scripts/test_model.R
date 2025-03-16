

# Charger le package
library(AER)


data(Arabidopsis)
summary(Arabidopsis[,"total.fruits"])

mp1 <- glmer(total.fruits ~ nutrient * amd + rack + status +
               (1 | popu) + (1 | gen), data = Arabidopsis, family = "poisson")
