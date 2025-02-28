# Spécifier un miroir CRAN
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# Installer le package readxl
install.packages("readxl")

# Charger le package readxl
library(readxl)

# Spécifier le chemin vers votre fichier .xlsx
file_path <- "./TP3/tp3datas.xlsx"

# Lire la feuille nommée "Exercice 1"
data <- read_excel(file_path, sheet = "Exercice 5", col_names = FALSE)

data_matrix <- as.matrix(data)

# 1. Quelles sont les populations observées ?
cat("Populations observées : Composant bleu, Composant jaune, Composant vert\n")

# 2. Quel est le caractère statistique observé, sa nature ?

cat("\nCaractère statistique : Durée de vie des composants, mesurée en temps\n")
cat("Nature : Quantitative continue (mesures de temps)\n")

# 3. Calculer les moyennes et les écart-types corrigés du temps écoulé pour chaque
# composant

# Extraire les données de chaque composant en supprimant les NA et le "("
composant_bleu <- as.numeric(na.omit(data_matrix[1, -1][data_matrix[1, -1] != "("]))
composant_jaune <- as.numeric(na.omit(data_matrix[2, -1][data_matrix[2, -1] != "("]))
composant_vert <- as.numeric(na.omit(data_matrix[3, -1][data_matrix[3, -1] != "("]))

# Calculer les moyennes
moy_bleu <- mean(composant_bleu)
moy_jaune <- mean(composant_jaune)
moy_vert <- mean(composant_vert)

# Calculer les écart-types corrigés
sd_bleu <- sd(composant_bleu)
sd_jaune <- sd(composant_jaune)
sd_vert <- sd(composant_vert)

# Afficher les résultats
cat("\nMoyennes :\n")
cat("Composant bleu :", round(moy_bleu, 2), "\n")
cat("Composant jaune :", round(moy_jaune, 2), "\n")
cat("Composant vert :", round(moy_vert, 2), "\n")

cat("\nÉcart-types corrigés :\n")
cat("Composant bleu :", round(sd_bleu, 2), "\n")
cat("Composant jaune :", round(sd_jaune, 2), "\n")
cat("Composant vert :", round(sd_vert, 2), "\n")

# 4. Tracer le diagramme des moyennes avec des barres d’erreurs d’un écart-type.

# Créer les données pour le graphique
composants <- c("Bleu", "Jaune", "Vert")
moyennes <- c(moy_bleu, moy_jaune, moy_vert)
ecart_types <- c(sd_bleu, sd_jaune, sd_vert)

# Créer le graphique de barres
barplot_result <- barplot(moyennes,
                          names.arg = composants,
                          ylim = c(0, max(moyennes + ecart_types) + 1),
                          main = "Moyennes des temps ecoules par composant",
                          xlab = "Composants",
                          ylab = "Temps ecoule",
                          col = c("lightblue", "yellow", "lightgreen"))

# Ajouter les barres d'erreur
arrows(x0 = barplot_result,
       y0 = moyennes - ecart_types,
       x1 = barplot_result,
       y1 = moyennes + ecart_types,
       angle = 90,
       code = 3,
       length = 0.1)

# Ajouter une grille
grid()

# 5. Réaliser un box-plot comparatif des 3 composants.

# Créer un boxplot comparatif
boxplot(list("Bleu" = composant_bleu,
             "Jaune" = composant_jaune,
             "Vert" = composant_vert),
        main = "Distribution des temps ecoules par composant",
        xlab = "Composants",
        ylab = "Temps ecoule",
        col = c("lightblue", "yellow", "lightgreen"))

# Ajouter une grille
grid()