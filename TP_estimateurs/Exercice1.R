# Création du vecteur de données
poids <- c(0.64, 0.65, 0.73, 0.60, 0.65, 0.77, 0.82, 0.64, 0.66, 0.72,
           0.87, 0.84, 0.66, 0.76, 0.63, 0.52, 0.66, 0.45, 0.74, 0.79)

# Configuration de la mise en page 1x2
par(mfrow=c(1,2))


# 1. Représenter graphiquement les données pour un le modèle gaussien semble-t-il pertinent ?

# Création de l'histogramme avec courbe de densité
hist(poids,
     probability = TRUE,
     main = "Histogramme des poids des cocons",
     xlab = "Poids (g)",
     ylab = "Densité",
     col = "lightblue",
     breaks = "FD")

# Ajout de la courbe de densité normale
curve(dnorm(x, mean=mean(poids), sd=sd(poids)),
      add=TRUE,
      col="red",
      lwd=2)

# Création du diagramme Q-Q
qqnorm(poids,
       main = "Diagramme Q-Q Normal",
       xlab = "Quantiles théoriques",
       ylab = "Quantiles observés")
qqline(poids, col="red")

# Réinitialisation de la mise en page
par(mfrow=c(1,1))

# Affichage des statistiques descriptives
summary(poids)

# 2. Calculer une esƟmaƟon sans biais de la moyenne  et de la variance 2
#  du modèle gaussien en foncƟon de ces données. Superposer la courbe obtenue et conclure.

# Calcul des estimateurs
moyenne <- mean(poids)
variance <- var(poids)
ecart_type <- sqrt(variance)

# Affichage des résultats
cat("Estimation de la moyenne (μ̂):", round(moyenne, 4), "g\n")
cat("Estimation de la variance (σ̂²):", round(variance, 6), "g²\n")
cat("Estimation de l'écart-type (σ̂):", round(ecart_type, 4), "g\n")

# Création du graphique avec la courbe de densité estimée
hist(poids,
     probability = TRUE,
     main = "Distribution des poids avec courbe normale estimée",
     xlab = "Poids (g)",
     ylab = "Densité",
     col = "lightblue",
     breaks = "FD")

# Superposition de la courbe de densité normale avec les paramètres estimés
curve(dnorm(x, mean = moyenne, sd = ecart_type),
      col = "red",
      lwd = 2,
      add = TRUE)

# Ajout d'une légende
legend("topright",
       legend = c("Densité normale estimée"),
       col = "red",
       lwd = 2)