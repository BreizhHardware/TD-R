# Charger les bibliothèques nécessaires
library(ggplot2)
library(readxl)
library(dplyr)

# Charger le fichier Excel
file_path <- "C:/Users/enoso/Downloads/Porjet_JO.xlsx"

# Lire la feuille "Travail_athletes"
df_athletes <- read_excel(file_path, sheet = "Travail_athletes")

# Calculer la moyenne d'athlètes par discipline
athletes_par_discipline <- df_athletes %>%
  group_by(Discipline) %>%
  summarise(Moyenne_Athletes = mean(n()))

# Trier et sélectionner le top 10
top_10_disciplines <- head(arrange(athletes_par_discipline, desc(Moyenne_Athletes)), 10)

# Afficher l'histogramme
ggplot(top_10_disciplines, aes(x = reorder(Discipline, -Moyenne_Athletes), y = Moyenne_Athletes)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Top 10 des disciplines avec le plus d'athlètes en moyenne", 
       x = "Discipline", y = "Nombre moyen d'athlètes") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
