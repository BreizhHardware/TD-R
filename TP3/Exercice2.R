# Spécifier un miroir CRAN
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# Installer le package readxl
install.packages("readxl")

# Charger le package readxl
library(readxl)

# Spécifier le chemin vers votre fichier .xlsx
file_path <- "./TP3/tp3datas.xlsx"

# Lire la feuille nommée "Exercice 1"
data <- read_excel(file_path, sheet = "Exercice 2")

# 1. Quelle est la population statistique observée ? La taille de l’échantillon ?

# Extraire les effectifs du dataframe
effectifs <- as.numeric(data[1, -1])  # Prendre tous les colonnes sauf la première

# Calculer la taille de l'échantillon
taille_echantillon <- sum(effectifs)

# Afficher les résultats
cat("Population statistique : Feuilles contaminées selon leur nombre de bourgeonnements\n")
cat("Taille de l'échantillon :", taille_echantillon, "feuilles\n")

# 2. Quel est le caractère statistique observé et sa nature ?
cat("Caractère statistique : Nombre de bourgeonnements par feuille\n")
cat("Nature : Quantitative discrète (nombres entiers de 0 à 10)\n")

# 3. Représenter graphiquement les données ?

# Créer un vecteur des valeurs (0 à 10 bourgeonnements)
bourgeonnements <- 0:10

# Créer le diagramme en bâtons
barplot(effectifs,
        names.arg = bourgeonnements,
        main = "Distribution du nombre de bourgeonnements par feuille",
        xlab = "Nombre de bourgeonnements",
        ylab = "Nombre de feuilles",
        col = "lightblue",
        border = "black")

# Ajouter une grille pour meilleure lisibilité
grid()

# 4. Calculer la moyenne et l’écart-type corrigé empirique du nombre de
# bourgeonnements par feuille.

# Créer un vecteur avec toutes les valeurs répétées selon leurs effectifs
valeurs <- rep(0:10, effectifs)

# Calculer la moyenne
moyenne <- mean(valeurs)

# Calculer l'écart-type corrigé empirique
ecart_type <- sd(valeurs)

# Afficher les résultats avec 3 décimales
cat("Moyenne :", round(moyenne, 3), "bourgeonnements par feuille\n")
cat("Écart-type corrigé :", round(ecart_type, 3), "bourgeonnements\n")