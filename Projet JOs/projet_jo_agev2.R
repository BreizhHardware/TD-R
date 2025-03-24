# Installer et charger les packages nécessaires
#install.packages("readxl")
#install.packages("ggplot2")
#install.packages("dplyr")
# Charger les bibliothèques nécessaires
library(readxl)
library(ggplot2)
library(dplyr)

# Charger les données
data_athletes <- read_excel("Porjet_JO.xlsx", sheet = "Travail_athletes")
data_medailles <- read_excel("Porjet_JO.xlsx", sheet = "Travail_medailles")

# Créer des catégories d'âge
data_medailles <- data_medailles %>%
  mutate(Age_Category = factor(case_when(
    Age < 18 ~ "Moins de 18 ans",
    Age >= 18 & Age < 25 ~ "18-24 ans",
    Age >= 25 & Age < 30 ~ "25-29 ans",
    Age >= 30 & Age < 35 ~ "30-34 ans",
    Age >= 35 & Age < 40 ~ "35-39 ans",
    Age >= 40 ~ "40 ans et plus"
  ), levels = c("Moins de 18 ans", "18-24 ans", "25-29 ans", "30-34 ans", "35-39 ans", "40 ans et plus")))

# Filtrer les pays avec plus de 20 athlètes
country_counts <- data_medailles %>%
  group_by(Country) %>%
  summarise(n = n()) %>%
  filter(n > 20)

data_medailles_filtered <- data_medailles %>%
  filter(Country %in% country_counts$Country)

# Filtrer les disciplines avec plus de 20 athlètes
discipline_counts <- data_medailles %>%
  group_by(Discipline) %>%
  summarise(n = n()) %>%
  filter(n > 20)

data_medailles_filtered <- data_medailles_filtered %>%
  filter(Discipline %in% discipline_counts$Discipline)

# Calculer le nombre d'athlètes par catégorie et par pays
data_country <- data_medailles_filtered %>%
  count(Country, Age_Category)

# Calculer le nombre d'athlètes par catégorie et par discipline
data_discipline <- data_medailles_filtered %>%
  count(Discipline, Age_Category)

# Trouver les 3 disciplines avec le plus d'athlètes
top_disciplines <- data_discipline %>%
  group_by(Discipline) %>%
  summarise(Total_Athletes = sum(n)) %>%
  arrange(desc(Total_Athletes)) %>%
  slice_head(n = 3)  # Garder les 3 premiers

# Trouver les 3 pays avec le plus d'athlètes
top_pays <- data_medailles_filtered %>%
  group_by(Country) %>%
  summarise(Total_Athletes = n()) %>%
  arrange(desc(Total_Athletes)) %>%
  slice_head(n = 3)  # Garder les 3 premiers

print("Top 3 pays avec le plus d'athlètes :")
print(top_pays)

# Afficher dans le terminal
print("Top 3 disciplines avec le plus d'athlètes :")
print(top_disciplines)

# Création du graphique
ggplot(data_discipline, aes(x = Age_Category, y = n, group = Discipline, color = Discipline)) +
  geom_bar(stat = "identity", position = "dodge", fill = NA, color = "black") +  # Barres pour toutes les disciplines
  geom_line(data = data_discipline %>% filter(Discipline %in% top_disciplines$Discipline),  # Lignes seulement pour le top 3
            aes(group = Discipline), size = 1.5) +  
  geom_point(data = data_discipline %>% filter(Discipline %in% top_disciplines$Discipline),  # Points pour le top 3
             size = 3) +  
  theme_minimal() +
  labs(title = "Nombre d'athlètes par catégorie d'âge et discipline (Top 3)",
       x = "Catégorie d'âge", y = "Nombre d'athlètes")

# Graphique : Nombre d'athlètes par catégorie d'âge et pays (Top 3)
ggplot(data_country, aes(x = Age_Category, y = n, group = Country)) +
  geom_bar(stat = "identity", position = "dodge", fill = NA, color = "black") +  # Barres pour tous les pays
  geom_line(data = data_country %>% filter(Country %in% top_pays$Country),  # Lignes pour le top 3
            aes(y = n, color = Country), size = 1.5) +  
  geom_point(data = data_country %>% filter(Country %in% top_pays$Country),  # Points pour le top 3
             aes(y = n, color = Country), size = 3) +  
  theme_minimal() +
  labs(title = "Nombre d'athlètes par catégorie d'âge et pays (Top 3)",
       x = "Catégorie d'âge", y = "Nombre d'athlètes")
