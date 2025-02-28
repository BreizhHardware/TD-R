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
data <- read_excel(file_path, sheet = "Exercice 4")

data_matrix <- as.matrix(data)

# 1. Quelles sont les populations observées ?

cat("Populations observées : Exploitations agricoles par classe de superficie\n")

# 2. Quel est le caractère statistique observé, sa nature ?

cat("\nCaractère statistique : Nombre d'exploitations agricoles par classe de superficie\n")
cat("Nature : Quantitative discrète (nombre d'exploitations) et Qualitative ordinale (classes de superficie)\n")

# 3. Tracer les histogrammes des deux distributions statistiques

# Extraire les données en excluant la première ligne
superficie <- data_matrix[2:nrow(data_matrix),1]
nb_1979 <- as.numeric(data_matrix[2:nrow(data_matrix),2])
nb_2010 <- as.numeric(data_matrix[2:nrow(data_matrix),3])

# Créer une mise en page pour deux graphiques côte à côte
par(mfrow = c(1, 2))

# Histogramme pour 1979
barplot(nb_1979,
        names.arg = superficie,
        main = "Distribution des exploitations en 1979",
        xlab = "Superficie (ha)",
        ylab = "Nombre d'exploitations (milliers)",
        col = "lightblue",
        border = "black")

# Histogramme pour 2010
barplot(nb_2010,
        names.arg = superficie,
        main = "Distribution des exploitations en 2010",
        xlab = "Superficie (ha)",
        ylab = "Nombre d'exploitations (milliers)",
        col = "lightgreen",
        border = "black")

# 4. Réaliser un box-plot comparatif en approchant les données de chaque classe par
# leur milieu et commenter l’évolution.

# Afficher les données pour vérification
print(superficie)

# Extraire les bornes des classes manuellement pour ces données spécifiques
bornes <- matrix(c(
  0, 20,      # moins de 20
  20, 50,     # 20 à 50
  50, 100,    # 50 à 100
  100, 200,   # 100 à 200
  200, 300    # 200 et plus
), ncol=2, byrow=TRUE)

# Calculer les centres des classes
centres_classes <- rowMeans(bornes)

# Créer des vecteurs répétés pour chaque année
valeurs_1979 <- rep(centres_classes, nb_1979)
valeurs_2010 <- rep(centres_classes, nb_2010)

# Créer le box-plot
annees <- factor(c(rep("1979", length(valeurs_1979)),
                   rep("2010", length(valeurs_2010))))
valeurs <- c(valeurs_1979, valeurs_2010)

boxplot(valeurs ~ annees,
        main = "Evolution de la distribution des superficies",
        xlab = "Annee",
        ylab = "Superficie (ha)",
        col = c("lightblue", "lightgreen"))
grid()



# Réinitialiser la mise en page
par(mfrow = c(1, 1))