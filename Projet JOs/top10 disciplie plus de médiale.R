library(readxl)

# Lire la feuille "Travail_medailles"
df_medailles <- read_excel(file_path, sheet = "Travail_medailles")

# Compter le nombre de médailles par discipline
medailles_par_discipline <- df_medailles %>%
  group_by(Discipline) %>%
  summarise(Nombre_Medailles = n()) %>%
  arrange(desc(Nombre_Medailles))

# Sélectionner les 10 disciplines les plus médaillées
top_10_medailles <- head(medailles_par_discipline, 10)

# Afficher l'histogramme
ggplot(top_10_medailles, aes(x = reorder(Discipline, -Nombre_Medailles), y = Nombre_Medailles)) +
  geom_bar(stat = "identity", fill = "gold") +
  labs(title = "Top 10 des disciplines ayant attribué le plus de médailles", 
       x = "Discipline", y = "Nombre de médailles") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
