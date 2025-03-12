data(iris)

# 1. Quelle est la nature de iris ? (matrix, vector, data.frame....)
cat("Iris est une matrice ?", is.matrix(iris), "\n")
cat("Iris est un vecteur ?", is.vector(iris), "\n")
cat("Iris est une data frame ?", is.data.frame(iris), "\n")

# 2. Les données. Combien d'individus ? de variables ? Pour chacune des variables donner
# leur nature (quantitative, qualitative, ...).

# Nombre d'individus et de variables
cat("Nombre d'individus:", nrow(iris), "\n")
cat("Nombre de variables:", ncol(iris), "\n\n")

# Nature de chaque variable
cat("Nature de chaque variable:\n")
for (i in 1:ncol(iris)) {
  var_name <- names(iris)[i]
  var_type <- if (is.factor(iris[, i])) {
    "qualitative nominale"
  } else if (is.numeric(iris[, i])) {
    "quantitative continue"
  } else {
    "autre"
  }

  cat("- ", var_name, ": ", var_type, "\n", sep="")
}

# 3. Calculer pour chaque variable lorsque cela a un sens, la moyenne et la variance
# empirique. Même question en divisant la population selon le niveau de la variable
# Species.

# Calculer la moyenne et la variance pour chaque variable numérique
cat("Statistiques pour toutes les données:\n")
for (i in 1:4) {  # Les 4 premières variables sont numériques
  var_name <- names(iris)[i]
  var_mean <- mean(iris[, i])
  var_var <- var(iris[, i])

  cat("- ", var_name, ":\n", sep="")
  cat("  Moyenne: ", round(var_mean, 2), "\n", sep="")
  cat("  Variance: ", round(var_var, 2), "\n", sep="")
}

# Calculer la moyenne et la variance par niveau de Species
cat("\nStatistiques par espèce:\n")
species_levels <- levels(iris$Species)

for (species in species_levels) {
  cat("\nEspèce: ", species, "\n", sep="")

  # Filtrer les données pour l'espèce actuelle
  species_data <- iris[iris$Species == species, ]

  for (i in 1:4) {  # Les 4 premières variables sont numériques
    var_name <- names(iris)[i]
    var_mean <- mean(species_data[, i])
    var_var <- var(species_data[, i])

    cat("- ", var_name, ":\n", sep="")
    cat("  Moyenne: ", round(var_mean, 2), "\n", sep="")
    cat("  Variance: ", round(var_var, 2), "\n", sep="")
  }
}

# 4. On s'intéresse au 4 premières variables. Standardiser les données (retirer la moyenne
# et diviser par l'écart-type).

# Création d'un nouveau dataframe pour les données standardisées
iris_standardized <- iris

# Standardisation des 4 premières variables
for (i in 1:4) {
  var_name <- names(iris)[i]
  iris_standardized[, i] <- scale(iris[, i])
}

# Afficher un résumé des données standardisées
cat("Résumé des données standardisées:\n")
print(summary(iris_standardized[, 1:4]))

# Vérifier que la moyenne est proche de 0 et l'écart-type est proche de 1
cat("\nVérification des moyennes (doivent être proches de 0):\n")
for (i in 1:4) {
  var_name <- names(iris_standardized)[i]
  var_mean <- mean(iris_standardized[, i])
  cat("- ", var_name, ": ", round(var_mean, 4), "\n", sep="")
}

cat("\nVérification des écarts-types (doivent être proches de 1):\n")
for (i in 1:4) {
  var_name <- names(iris_standardized)[i]
  var_sd <- sd(iris_standardized[, i])
  cat("- ", var_name, ": ", round(var_sd, 4), "\n", sep="")
}

# 5. Tracer le graphique variable Petal.Length versus Petal.Width en utilisant des couleurs
# différentes selon le niveau de la variable Species. Essayer tous les "croisements"
# possibles et commentez ces graphiques.

# Définir une palette de couleurs distincte
iris_colors <- c("red", "blue", "green")

# 1. Graphique Petal.Length vs Petal.Width
plot(iris$Petal.Length, iris$Petal.Width,
     main="Petal.Length vs Petal.Width",
     xlab="Petal.Length",
     ylab="Petal.Width",
     pch=19,
     col=iris_colors[as.numeric(iris$Species)])

# Ajouter une légende
legend("topleft",
       legend=levels(iris$Species),
       col=iris_colors,
       pch=19)

# 2. Matrice de nuages de points pour toutes les combinaisons
pairs(iris[, 1:4],
      main="Matrices de nuages de points",
      pch=19,
      col=iris_colors[as.numeric(iris$Species)])

# Ajouter une légende
legend("bottomright",
       legend=levels(iris$Species),
       col=iris_colors,
       pch=19,
       cex=0.8)

# 3. Graphiques individuels pour chaque combinaison
par(mfrow=c(2, 3))

# Toutes les combinaisons de variables
plot(iris$Sepal.Length, iris$Sepal.Width, main="Sepal.Length vs Sepal.Width",
     xlab="Sepal.Length", ylab="Sepal.Width", pch=19,
     col=iris_colors[as.numeric(iris$Species)])
legend("topright", legend=levels(iris$Species), col=iris_colors, pch=19, cex=0.6)

plot(iris$Sepal.Length, iris$Petal.Length, main="Sepal.Length vs Petal.Length",
     xlab="Sepal.Length", ylab="Petal.Length", pch=19,
     col=iris_colors[as.numeric(iris$Species)])
legend("topleft", legend=levels(iris$Species), col=iris_colors, pch=19, cex=0.6)

plot(iris$Sepal.Length, iris$Petal.Width, main="Sepal.Length vs Petal.Width",
     xlab="Sepal.Length", ylab="Petal.Width", pch=19,
     col=iris_colors[as.numeric(iris$Species)])
legend("topleft", legend=levels(iris$Species), col=iris_colors, pch=19, cex=0.6)

plot(iris$Sepal.Width, iris$Petal.Length, main="Sepal.Width vs Petal.Length",
     xlab="Sepal.Width", ylab="Petal.Length", pch=19,
     col=iris_colors[as.numeric(iris$Species)])
legend("topright", legend=levels(iris$Species), col=iris_colors, pch=19, cex=0.6)

plot(iris$Sepal.Width, iris$Petal.Width, main="Sepal.Width vs Petal.Width",
     xlab="Sepal.Width", ylab="Petal.Width", pch=19,
     col=iris_colors[as.numeric(iris$Species)])
legend("topright", legend=levels(iris$Species), col=iris_colors, pch=19, cex=0.6)

plot(iris$Petal.Length, iris$Petal.Width, main="Petal.Length vs Petal.Width",
     xlab="Petal.Length", ylab="Petal.Width", pch=19,
     col=iris_colors[as.numeric(iris$Species)])
legend("topleft", legend=levels(iris$Species), col=iris_colors, pch=19, cex=0.6)

# Réinitialiser la mise en page
par(mfrow=c(1, 1))

# Commentaires sur les graphiques
cat("\nCommentaires sur les graphiques:\n")

cat("\nPetal.Length vs Petal.Width:\n")
cat("- Forte corrélation positive entre ces variables\n")
cat("- Setosa est clairement isolée avec des pétales plus petits\n")
cat("- Bonne séparation entre les trois espèces\n")

cat("\nSepal.Length vs Sepal.Width:\n")
cat("- Corrélation moins évidente\n")
cat("- Setosa a des sépales plus courts mais plus larges\n")
cat("- Chevauchement important entre Versicolor et Virginica\n")

cat("\nSepal.Length vs Petal.Length/Width:\n")
cat("- Bonne séparation des espèces\n")
cat("- Setosa bien isolée\n")
cat("- Corrélation positive au sein de chaque espèce\n")

cat("\nSepal.Width vs Petal.Length/Width:\n")
cat("- Tendance à une corrélation négative\n")
cat("- Setosa a des sépales larges mais des pétales courts\n")

cat("\nConclusion:\n")
cat("- Les variables liées aux pétales sont plus discriminantes\n")
cat("- Setosa est la plus facilement identifiable\n")
cat("- La combinaison Petal.Length/Petal.Width offre la meilleure séparation\n")