# Création du tableau de contingence
donnees <- matrix(
  c(33, 93, 55, 20, 16, 9,
    35, 77, 34, 13, 10, 4,
    81, 34, 25, 6, 2, 1),
  nrow = 3, byrow = TRUE
)

# Ajout des noms de lignes et colonnes
rownames(donnees) <- c("Moins de 30 ans", "Entre 30 et 60 ans", "Plus de 60 ans")
colnames(donnees) <- c("0", "1", "2", "3", "4", "5")

# 1. Pour chaque classe d'âge, donnez une esƟmaƟon sans biais du nombre moyen de sorƟe au
# cinéma ainsi que la variance correspondante.

# Fonction pour calculer la moyenne pondérée
moyenne_ponderee <- function(x, effectifs) {
  sum(x * effectifs) / sum(effectifs)
}

# Fonction pour calculer la variance non biaisée
variance_non_biaisee <- function(x, effectifs, moyenne) {
  n <- sum(effectifs)
  sum(effectifs * (x - moyenne)^2) / (n - 1)
}

# Valeurs possibles de sorties
sorties <- as.numeric(colnames(donnees))

# Calcul pour chaque classe d'âge
resultats <- data.frame(
  classe_age = rownames(donnees),
  effectif_total = rowSums(donnees),
  moyenne = NA,
  variance = NA
)

for (i in 1:nrow(donnees)) {
  moy <- moyenne_ponderee(sorties, donnees[i,])
  var <- variance_non_biaisee(sorties, donnees[i,], moy)
  resultats$moyenne[i] <- moy
  resultats$variance[i] <- var
}

# Affichage des résultats arrondis
resultats$moyenne <- round(resultats$moyenne, 2)
resultats$variance <- round(resultats$variance, 2)
print(resultats)

# 2. Représenter graphiquement les intervalles moyennes écart-type et commentés.

# Calcul des écarts-types
resultats$ecart_type <- sqrt(resultats$variance)

# Création du graphique
plot(resultats$moyenne,
     1:3,
     ylim = c(0.5, 3.5),
     xlim = c(0, max(resultats$moyenne + resultats$ecart_type) + 0.5),
     pch = 19,
     main = "Moyenne et écart-type des sorties au cinéma par classe d'âge",
     xlab = "Nombre de sorties",
     ylab = "",
     yaxt = "n")

# Ajout des noms des classes d'âge
axis(2, at = 1:3, labels = resultats$classe_age, las = 1)

# Ajout des intervalles moyenne ± écart-type
arrows(resultats$moyenne - resultats$ecart_type,
       1:3,
       resultats$moyenne + resultats$ecart_type,
       1:3,
       code = 3,
       angle = 90,
       length = 0.1)

# Ajout d'une grille verticale pour faciliter la lecture
grid(nx = NULL, ny = NA)