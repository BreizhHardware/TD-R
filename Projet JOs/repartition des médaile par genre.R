# Compter le nombre de médailles par genre
medailles_par_genre <- df_medailles %>%
  group_by(Gender) %>%
  summarise(Nombre_Medailles = n())

# Afficher l'histogramme
ggplot(medailles_par_genre, aes(x = Gender, y = Nombre_Medailles, fill = Gender)) +
  geom_bar(stat = "identity") +
  labs(title = "Répartition des médailles par genre", 
       x = "Genre", y = "Nombre de médailles") +
  theme_minimal()
