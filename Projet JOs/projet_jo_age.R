# Installer et charger les packages nécessaires
#install.packages("readxl")
#install.packages("ggplot2")
#install.packages("dplyr")
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

# Filtrer les pays avec plus d'un athlète
country_counts <- data_medailles %>% group_by(Country) %>% summarise(n = n()) %>% filter(n > 20)
data_medailles_filtered <- data_medailles %>% filter(Country %in% country_counts$Country)

# Filtrer les disciplines avec plus d'un athlète
discipline_counts <- data_medailles %>% group_by(Discipline) %>% summarise(n = n()) %>% filter(n > 20)
data_medailles_filtered <- data_medailles_filtered %>% filter(Discipline %in% discipline_counts$Discipline)

# Histogramme des âges (catégories)
ggplot(data_medailles, aes(x = Age_Category)) +
  geom_bar(fill = "blue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Répartition des âges des athlètes", x = "Catégorie d'âge", y = "Nombre d'athlètes")

# Comparaison par pays
ggplot(data_medailles_filtered, aes(x = Age_Category, fill = Country)) +
  geom_bar(position = "dodge", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Nombre d'athlètes par catégorie d'âge et pays", x = "Catégorie d'âge", y = "Nombre d'athlètes")

# Comparaison par discipline
ggplot(data_medailles_filtered, aes(x = Age_Category, fill = Discipline)) +
  geom_bar(position = "dodge", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Nombre d'athlètes par catégorie d'âge et discipline", x = "Catégorie d'âge", y = "Nombre d'athlètes")
