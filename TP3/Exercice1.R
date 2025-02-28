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
data <- read_excel(file_path, sheet = "Exercice 1")

# 1. Quelle est la population statistique observée ? La taille de l’échantillon ?
tailles <- as.vector(as.matrix(data))

echantillon_taille <- length(tailles)

print("Population observée: Tailles des conducteurs (en cm)")
cat("Taille de l'échantillon:", echantillon_taille, "observations\n")

# 2. Quel est le caractère statistique observé et sa nature ?
print("Caractère statistique: Taille des conducteurs")
print("Nature: Quantitative continue (mesurée en cm)")

# 3. Dépouiller les données suivant une distribution des fréquences dont la limite
# inférieure est 150 cm (inclus) et dont la largeur de chaque classe est de 5. On
# indiquera les fréquences de chaque classe et on représentera graphiquement
# l’histogramme associé à cette distribution

# Créer les classes avec une largeur de 5cm
breaks <- seq(150, 190, by = 5)  # De 150 à 190 pour couvrir toutes les données

# Calculer la distribution des fréquences
freq_dist <- table(cut(tailles, breaks = breaks, include.lowest = TRUE, right = FALSE))

# Afficher le tableau des fréquences absolues
cat("Distribution des fréquences absolues:\n")
print(freq_dist)

# Calculer et afficher les fréquences relatives
freq_rel <- prop.table(freq_dist)
cat("\nDistribution des fréquences relatives:\n")
print(freq_rel)

# Créer l'histogramme
hist(tailles,
     breaks = breaks,
     main = "Distribution des tailles des conducteurs",
     xlab = "Taille (cm)",
     ylab = "Frequence",
     col = "lightblue",
     border = "black")

# 4. Tracer la courbe de la fonction de répartition empirique (diagramme des
# fréquences cumulées) de la distribution de fréquences de la question précédente.

# Calculer les fréquences cumulées
freq_cum <- cumsum(freq_dist)
freq_cum_rel <- cumsum(freq_rel)

# Créer les points pour le graphique (milieu des classes)
points_x <- breaks[-length(breaks)] + 2.5

# Tracer la courbe des fréquences cumulées
plot(points_x, freq_cum_rel,
     type = "s",  # type "s" pour une courbe en escalier
     main = "Fonction de repartition empirique",
     xlab = "Taille (cm)",
     ylab = "Frequences cumulees",
     ylim = c(0, 1),
     col = "blue")

# Ajouter une grille
grid()

# Ajouter les points
points(points_x, freq_cum_rel, pch = 16, col = "red")

# 5. Calculer la médiane, l’intervalle interquartile et interdécile.

# Calculer la médiane
mediane <- median(tailles)

# Calculer les quartiles
Q1 <- quantile(tailles, 0.25)
Q3 <- quantile(tailles, 0.75)
IQR <- Q3 - Q1

# Calculer les déciles
D1 <- quantile(tailles, 0.1)
D9 <- quantile(tailles, 0.9)
IDR <- D9 - D1

# Afficher les résultats
cat("Médiane:", mediane, "cm\n")
cat("\nIntervalle interquartile (IQR):\n")
cat("Q1 (25%):", Q1, "cm\n")
cat("Q3 (75%):", Q3, "cm\n")
cat("IQR:", IQR, "cm\n")
cat("\nIntervalle interdécile (IDR):\n")
cat("D1 (10%):", D1, "cm\n")
cat("D9 (90%):", D9, "cm\n")
cat("IDR:", IDR, "cm\n")

# 6. Si on élimine les 5% plus grands et les 5% les plus petits des conducteurs, quel
# intervalle de taille standard retiendra-t-on ?

# Calculer les percentiles 5 et 95
P5 <- quantile(tailles, 0.05)
P95 <- quantile(tailles, 0.95)
intervalle <- P95 - P5

# Afficher les résultats
cat("Intervalle standard (après élimination des 5% extrêmes):\n")
cat("Borne inférieure (5%):", P5, "cm\n")
cat("Borne supérieure (95%):", P95, "cm\n")
cat("Amplitude de l'intervalle:", intervalle, "cm\n")