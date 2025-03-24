# Charger les bibliothèques nécessaires
library(ggplot2)
library(readxl)
library(dplyr)

# Définir le chemin du fichier Excel
file_path <- "C:/Users/enoso/Downloads/Porjet_JO.xlsx"

# Lire la feuille "Travail_medailles"
df_medailles <- read_excel(file_path, sheet = "Travail_medailles")

# Vérifier les noms des colonnes pour voir comment les médailles sont nommées
print(colnames(df_medailles))

# Filtrer les données pour exclure les genres 'O' et 'X' et ne garder que 'W' et 'M'
df_medailles_filtrées <- df_medailles %>%
  filter(Gender %in% c("W", "M"))

# Supposons que la colonne des médailles s'appelle "Medal_code"
# Convertir les codes des médailles en valeurs textuelles
df_medailles_filtrées <- df_medailles_filtrées %>%
  mutate(Medal_code = case_when(
    Medal_code == 1 ~ "Gold",
    Medal_code == 2 ~ "Silver",
    Medal_code == 3 ~ "Bronze",
    TRUE ~ as.character(Medal_code)
  ))

# 1. Filtrer les données pour obtenir seulement les médailles d'or
df_gold <- df_medailles_filtrées %>%
  filter(Medal_code == "Gold")

# 2. Vérification : Supprimer les lignes avec des valeurs manquantes (NA) dans la colonne Gender ou Medal_code
df_gold_clean <- df_gold %>%
  filter(!is.na(Gender))  # Enlever les lignes où Gender est NA

# 3. Vérification de la répartition des médailles par genre
print(table(df_gold_clean$Gender))  # Nombre d'observations par genre

# Vérification de la distribution des valeurs dans 'Nombre_Medailles' (devrait être un nombre constant pour chaque genre)
df_gold_clean <- df_gold_clean %>%
  group_by(Gender) %>%
  mutate(Nombre_Medailles = n()) %>%
  ungroup()

# 4. Compter le nombre de médailles par genre et par type
medailles_par_genre <- df_medailles_filtrées %>%
  group_by(Gender, Medal_code) %>%
  summarise(Nombre_Medailles = n(), .groups = "drop")

# Créer un tableau de contingence pour le test du Khi²
tableau_contingence <- medailles_par_genre %>%
  spread(key = Medal_code, value = Nombre_Medailles, fill = 0) %>%
  select(-Gender)

# Effectuer le test Khi²
khi2_test <- chisq.test(tableau_contingence)
print(khi2_test)

# Afficher l'histogramme
ggplot(medailles_par_genre, aes(x = Gender, y = Nombre_Medailles, fill = Medal_code)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Gold" = "gold", "Silver" = "gray", "Bronze" = "chocolate")) +
  labs(title = "Répartition des médailles par genre et type", 
       x = "Genre", y = "Nombre de médailles", fill = "Type de médaille") +
  theme_minimal()
