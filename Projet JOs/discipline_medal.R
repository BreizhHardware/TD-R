# Installation et chargement des packages nécessaires
if (!require("readxl")) install.packages("readxl")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("ggrepel")) install.packages("ggrepel")

library(readxl)
library(dplyr)
library(ggplot2)
library(ggrepel)

# Importation des données pour les médailles
donnees_medailles <- read_excel("Projet JOs/Porjet_JO.xlsx", sheet = "Travail_medailles")

# Calcul du nombre de médailles par pays (utilisez Country_code au lieu de Country)
nombre_medailles <- table(donnees_medailles$Country_code, donnees_medailles$Medal_type)
nombre_medailles <- as.data.frame.matrix(nombre_medailles)
nombre_medailles$Total <- rowSums(nombre_medailles)
nombre_medailles$Pays <- rownames(nombre_medailles)
nombre_medailles <- nombre_medailles[order(-nombre_medailles$Total),]

# Utilisation directe des données de disciplines fournies
disciplines_par_pays <- data.frame(
  Country = c("FRA", "USA", "AUS", "CHN", "JPN", "CAN", "GER", "ESP", "BRA", "ITA"),
  Nombre_Disciplines = c(45, 44, 42, 41, 41, 40, 40, 38, 37, 36)
)

# Combiner les données (pas besoin de changer car les codes pays correspondent déjà)
donnees_combinees <- inner_join(
  nombre_medailles %>% select(Pays, Total),
  disciplines_par_pays,
  by = c("Pays" = "Country")
)

# Le reste du code reste inchangé
# Créer des catégories pour l'analyse chi-carré
donnees_combinees <- donnees_combinees %>%
  mutate(
    Cat_Disciplines = cut(Nombre_Disciplines,
                          breaks = c(0, 35, 40, 45, Inf),
                          labels = c("30-35", "36-40", "41-45", "45+"),
                          include.lowest = TRUE),
    Cat_Medailles = cut(Total,
                        breaks = c(0, 20, 40, 60, Inf),
                        labels = c("1-20", "21-40", "41-60", "60+"),
                        include.lowest = TRUE)
  )

# Table de contingence pour le test chi-carré
contingence <- table(donnees_combinees$Cat_Disciplines, donnees_combinees$Cat_Medailles)
print("Table de contingence:")
print(contingence)

# Test d'indépendance du Chi-carré
test_chi2 <- chisq.test(contingence, simulate.p.value = TRUE)
print("Résultat du test Chi-carré:")
print(test_chi2)

# Afficher les résidus standardisés
print("Résidus standardisés:")
print(round(test_chi2$residuals, 2))

# Graphique de la relation
p1 <- ggplot(donnees_combinees, aes(x = Nombre_Disciplines, y = Total)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  geom_text_repel(aes(label = Pays), size = 3) +
  labs(title = "Relation entre disciplines et médailles par pays",
       subtitle = paste("Test Chi² : p-value =", format.pval(test_chi2$p.value, digits = 3)),
       x = "Nombre de disciplines",
       y = "Nombre total de médailles") +
  theme_minimal()

print(p1)
ggsave("Projet JOs/disciplines_medailles_chi2.png", p1, width = 10, height = 8, dpi = 300)

# Ratio médailles/disciplines
donnees_combinees$Ratio <- donnees_combinees$Total / donnees_combinees$Nombre_Disciplines

# Top pays par efficacité
top_efficacite <- donnees_combinees %>%
  arrange(desc(Ratio)) %>%
  head(10)

p3 <- ggplot(top_efficacite, aes(x = reorder(Pays, Ratio), y = Ratio)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(title = "Top pays avec le meilleur ratio médailles/disciplines",
       x = "",
       y = "Ratio médailles/disciplines") +
  theme_minimal()

print(p3)
ggsave("Projet JOs/top10_efficacite_pays.png", p3, width = 8, height = 6, dpi = 300)