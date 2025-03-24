# Charger les packages nécessaires
library(readxl)
library(ggplot2)

# Charger le fichier Excel
file_path <- "C:\\Users\\enoso\\Downloads\\Porjet_JO.xlsx"
# Lire la feuille "Travail_athletes"
df_athletes <- read_excel(file_path, sheet = "Travail_athletes")

# Vérifier si la colonne "Gender" existe (sensible à la casse)
if ("Gender" %in% names(df_athletes)) {
  # Compter les occurrences de chaque genre
  genre_counts <- table(df_athletes$Gender)
  
  # Créer un data frame pour ggplot2
  genre_df <- data.frame(genre = names(genre_counts), count = as.numeric(genre_counts))
  
  # Créer l'histogramme
  ggplot(genre_df, aes(x = genre, y = count)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    labs(title = "Distribution des genres des athlètes", x = "Genre", y = "Nombre d'athlètes") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotation des étiquettes de l'axe x
} else {
  print("La colonne 'Gender' n'existe pas dans le fichier Excel.")
}