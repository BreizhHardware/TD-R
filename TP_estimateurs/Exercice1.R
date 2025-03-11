# https://github.com/BreizhHardware/TD-R/tree/main/TP_estimateurs

# Création du vecteur de données
poids <- c(0.64, 0.65, 0.73, 0.60, 0.65, 0.77, 0.82, 0.64, 0.66, 0.72,
           0.87, 0.84, 0.66, 0.76, 0.63, 0.52, 0.66, 0.45, 0.74, 0.79)

# Configuration de la mise en page 1x2
par(mfrow=c(1,2))


# 1. Représenter graphiquement les données pour un le modèle gaussien semble-t-il pertinent ?

# Calcul du nombre de classes pour l'histogramme
n <- length(poids)
k <- sqrt(n)

# Création de l'histogramme avec courbe de densité
histoooo <- hist(poids,
     probability = TRUE,
     main = "Histogramme des poids des cocons",
     xlab = "Poids (g)",
     ylab = "Densité",
     col = "lightblue",
     breaks = k)

lines(density(poids), col="green", lwd=2)

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
moyenne <- 1/n * sum(poids)
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
       legend = "Densité normale estimée",
       col = "red",
       lwd = 2)

# Données les intervalles de confiance à 95% et 90% pour la moyenne et la variance

# Calcul des intervalles de confiance à 95%
alpha <- 0.05
z <- qnorm(1 - alpha / 2)

# Intervalle de confiance pour la moyenne
borne_inf_moyenne <- moyenne - z * ecart_type / sqrt(n)
borne_sup_moyenne <- moyenne + z * ecart_type / sqrt(n)

# Intervalle de confiance pour la variance
borne_inf_variance <- (n - 1) * variance / qchisq(1 - alpha / 2, df = n - 1)
borne_sup_variance <- (n - 1) * variance / qchisq(alpha / 2, df = n - 1)

# Affichage des intervalles de confiance
cat("\nIntervalle de confiance à 95% pour la moyenne (μ): [",
    round(borne_inf_moyenne, 4), ",",
    round(borne_sup_moyenne, 4), "] g\n")

cat("Intervalle de confiance à 95% pour la variance (σ²): [",
    round(borne_inf_variance, 6), ",",
    round(borne_sup_variance, 6), "] g²\n")

# Affichage de l'histogramme des poids avec la courbe de densité estimée et les intervalles de confiance

scale_factor = max(histoooo$counts) / max(dnorm(seq(min(poids), max(poids), length.out=1), mean=mean(poids), sd=sd(poids)))

hist(poids,
     main = "Histogramme des poids des cocons",
     xlab = "Poids (g)",
     ylab = "Densité",
     col = "lightblue",
     ylim = c(0, max(histoooo$counts) * scale_factor),
     breaks = k)

# Colorer l'intervalle de confiance
x_vals <- seq(borne_inf_moyenne, borne_sup_moyenne, length.out = 100)
y_vals <- dnorm(x_vals, mean = moyenne, sd = ecart_type)
polygon(c(borne_inf_moyenne, x_vals, borne_sup_moyenne),
        c(0, y_vals, 0),
        col = rgb(0, 0, 1, 0.2),
        border = NA)

lines(density(poids), col="green", lwd=2)

# Ajout de la courbe de densité normale
curve(scale_factor * dnorm(x, mean=mean(poids), sd=sd(poids)),
      add=TRUE,
      col="red",
      lwd=2)

# Ajout des intervalles de confiance pour la moyenne
abline(v = c(borne_inf_moyenne, borne_sup_moyenne),
       col = "blue",
       lty = 2)


# Calcul des intervalles de confiance à 90%
alpha <- 0.10
z <- qnorm(1 - alpha / 2)

# Intervalle de confiance pour la moyenne
borne_inf_moyenne <- moyenne - z * ecart_type / sqrt(n)
borne_sup_moyenne <- moyenne + z * ecart_type / sqrt(n)

# Intervalle de confiance pour la variance
borne_inf_variance <- (n - 1) * variance / qchisq(1 - alpha / 2, df = n - 1)
borne_sup_variance <- (n - 1) * variance / qchisq(alpha / 2, df = n - 1)

# Affichage des intervalles de confiance
cat("\nIntervalle de confiance à 90% pour la moyenne (μ): [",
    round(borne_inf_moyenne, 4), ",",
    round(borne_sup_moyenne, 4), "] g\n")

cat("Intervalle de confiance à 90% pour la variance (σ²): [",
    round(borne_inf_variance, 6), ",",
    round(borne_sup_variance, 6), "] g²\n")

# Ajout des intervalles de confiance pour la moyenne
abline(v = c(borne_inf_moyenne, borne_sup_moyenne),
       col = "purple",
       lty = 2)

# Colorer l'intervalle de confiance
x_vals <- seq(borne_inf_moyenne, borne_sup_moyenne, length.out = 100)
y_vals <- dnorm(x_vals, mean = moyenne, sd = ecart_type)
polygon(c(borne_inf_moyenne, x_vals, borne_sup_moyenne),
        c(0, y_vals, 0),
        col = rgb(1, 0, 1, 0.2),
        border = NA)

# Ajout d'une légende
legend("topright",
       legend = c("Densité estimée", "Densité normale estimée", "Intervalle de confiance à 95%", "Intervalle de confiance à 90%"),
       col = c("green", "red", "blue", "purple"),
       lty = c(1, 1, 2, 2),
       lwd = c(2, 2, 1, 1))

# Crée un histogramme des poids et récupère les classes
hist_obj <- hist(poids, plot = FALSE, breaks = k)
observed_counts <- hist_obj$counts
breaks <- hist_obj$breaks
mids <- hist_obj$mids

# Calcul des probabilités attendues
total_count <- sum(observed_counts)
expected_probs <- diff(pnorm(breaks, mean = moyenne, sd = ecart_type))
expected_counts <- total_count * expected_probs

# Perform chi-squared test
chitest <- chisq.test(observed_counts, p = expected_probs, rescale.p = TRUE)
print(chitest)