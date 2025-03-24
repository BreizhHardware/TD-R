# Installation et chargement des packages nécessaires
if (!require("readxl")) {
  install.packages("readxl")
}
if (!require("ggplot2")) {
  install.packages("ggplot2")
}
if (!require("dplyr")) {
  install.packages("dplyr")
}
library(readxl)
library(ggplot2)
library(dplyr)

# Importation des données depuis le fichier Excel
donnees_medailles <- read_excel("Projet JOs/Porjet_JO.xlsx",
                                sheet = "Travail_medailles")

# Vérifier si les données contiennent la discipline
if("Discipline" %in% colnames(donnees_medailles)) {
  # Compter le nombre de médailles par discipline
  medailles_par_discipline <- donnees_medailles %>%
    group_by(Discipline) %>%
    summarise(Total = n()) %>%
    arrange(desc(Total))
} else {
  # Si la discipline n'est pas dans ce jeu de données, charger les données des athlètes
  df_athletes <- read_excel("Projet JOs/Porjet_JO.xlsx",
                            sheet = "Travail_athletes")

  # Joindre les données des athlètes avec les médailles (en supposant un champ commun comme Name ou Athlete_ID)
  # Adapter la clé de jointure selon la structure réelle des données
  donnees_jointes <- inner_join(donnees_medailles, df_athletes,
                                by = c("Athlete_ID" = "Athlete_ID"))  # adapter ces colonnes

  # Compter le nombre de médailles par discipline
  medailles_par_discipline <- donnees_jointes %>%
    group_by(Discipline) %>%
    summarise(Total = n()) %>%
    arrange(desc(Total))
}

# Sélectionner le top 10 des disciplines
top_10_disciplines <- head(medailles_par_discipline, 10)

# Créer un graphique à barres
graphique_disciplines <- ggplot(top_10_disciplines, aes(x = reorder(Discipline, Total), y = Total)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 des disciplines avec le plus de medailles",
       x = "Discipline",
       y = "Nombre de medailles") +
  theme_minimal()

# Afficher le graphique
print(graphique_disciplines)

# Sauvegarder le graphique
ggsave("Projet JOs/top10_disciplines.png", plot = graphique_disciplines,
       width = 8, height = 6, units = "in", dpi = 300)

# Afficher également un tableau des résultats
print(top_10_disciplines)