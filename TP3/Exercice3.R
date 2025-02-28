# https://github.com/BreizhHardware/TD-R/tree/main/TP3

# Spécifier un miroir CRAN
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# Installer le package readxl
install.packages("readxl")

# Charger le package readxl
library(readxl)

# Spécifier le chemin vers votre fichier .xlsx
file_path <- "./TP3/tp3datas.xlsx"

# Lire la feuille nommée "Exercice 1"
data <- read_excel(file_path, sheet = "Exercice 3")

# 1. Quelle est la population statistique observée ? La taille de l’échantillon ?

# Convertir la colonne des effectifs en numérique
effectifs <- as.numeric(unlist(data[1:4, 2]))  # Utiliser unlist pour extraire les valeurs

# Calculer la taille de l'échantillon
taille_echantillon <- sum(effectifs)

# Afficher les résultats
cat("Population statistique : Professionnels de santé par catégorie\n")
cat("Taille de l'échantillon :", taille_echantillon, "personnes\n")

# 2. Quel est le caractère statistique observé et sa nature ?

# Afficher le caractère statistique et sa nature
cat("\nCaractère statistique : Profession exercée dans le domaine de la santé\n")
cat("Nature : Qualitative nominale (catégories non ordonnées de professions médicales)\n")

# 3. Représenter graphiquement les données avec un diagramme circulaire.

# Calculer les pourcentages pour chaque profession
pourcentages <- round(effectifs / taille_echantillon * 100, 1)

# Créer les labels avec les pourcentages
labels <- paste(data$Profession[1:4], "\n(", pourcentages, "%)", sep="")

# Créer le diagramme circulaire
pie(effectifs,
    labels = labels,
    main = "Repartition des professionnels de sante",
    col = c("lightblue", "lightgreen", "lightpink", "lightyellow"),
    border = "white")

# Ajouter une légende
legend("topright",
       data$Profession[1:4],
       fill = c("lightblue", "lightgreen", "lightpink", "lightyellow"),
       cex = 0.8)