# Définition des données d'épaisseur en millimètres
epaisseurs <- c(6.47, 7.02, 7.15, 7.22, 7.44, 6.99, 7.47, 7.61, 7.32, 7.22,
                7.52, 6.92, 7.28, 6.69, 7.24, 7.19, 6.97, 7.52, 6.22, 7.13,
                7.32, 7.67, 7.24, 6.21)

# Paramètres du problème
n <- length(epaisseurs)           # Taille de l'échantillon
mu0 <- 7.3                        # Moyenne annoncée par le fournisseur
sigma <- 0.38                     # Écart-type connu
alpha <- 0.05                     # Niveau de signification 5%

# Calcul des statistiques de base
moyenne <- mean(epaisseurs)
ecart_type_echantillon <- sd(epaisseurs)

# Calcul de la statistique du test z
z_stat <- (moyenne - mu0) / (sigma / sqrt(n))
p_value <- 2 * pnorm(-abs(z_stat))  # Test bilatéral

# Valeur critique pour un test bilatéral avec α = 0.05
z_critique <- qnorm(1 - alpha/2)

# Affichage des résultats
cat("Analyse des épaisseurs des composants isolants\n")
cat("---------------------------------------------\n")
cat(sprintf("Nombre d'observations: %d\n", n))
cat(sprintf("Moyenne des épaisseurs: %.4f mm\n", moyenne))
cat(sprintf("Écart-type échantillon: %.4f mm\n", ecart_type_echantillon))
cat(sprintf("Écart-type connu: %.4f mm\n", sigma))
cat("\nTest d'hypothèse\n")
cat(sprintf("H0: μ = %.1f mm (le fournisseur respecte ses engagements)\n", mu0))
cat(sprintf("H1: μ ≠ %.1f mm (le fournisseur ne respecte pas ses engagements)\n", mu0))
cat(sprintf("Statistique z calculée: %.4f\n", z_stat))
cat(sprintf("Valeur critique (α = %.2f): ±%.4f\n", alpha, z_critique))
cat(sprintf("p-value: %.4f\n", p_value))

# Conclusion
cat("\nConclusion:\n")
if (abs(z_stat) > z_critique) {
  cat(sprintf("Comme |z| = %.4f > %.4f et p-value = %.4f < %.2f, nous rejetons H0.\n",
              abs(z_stat), z_critique, p_value, alpha))
  cat("L'entreprise peut affirmer, avec un risque faible de se tromper, que le fournisseur ne respecte pas ses engagements.\n")
} else {
  cat(sprintf("Comme |z| = %.4f < %.4f et p-value = %.4f > %.2f, nous ne pouvons pas rejeter H0.\n",
              abs(z_stat), z_critique, p_value, alpha))
  cat("L'entreprise ne peut pas affirmer, avec un risque faible de se tromper, que le fournisseur ne respecte pas ses engagements.\n")
}

# Représentation graphique
k <- sqrt(n)

hist(epaisseurs,
     breaks = k,
     col = "lightblue",
     main = "Distribution des épaisseurs des composants",
     xlab = "Épaisseur (mm)",
     ylab = "Fréquence")

# Ajout de lignes verticales pour la moyenne échantillon et la moyenne annoncée
abline(v = moyenne, col = "red", lwd = 2)
abline(v = mu0, col = "blue", lwd = 2, lty = 2)

# Ajout de la courbe de densité normale estimée
curve(dnorm(x, mean = moyenne, sd = sigma),
      add = TRUE,
      col = "darkgreen",
      lwd = 2)

# Légende
legend("topright",
       legend = c(paste("Moyenne échantillon:", round(moyenne, 4), "mm"),
                  paste("Moyenne annoncée:", mu0, "mm"),
                  "Densité normale estimée"),
       col = c("red", "blue", "darkgreen"),
       lwd = c(2, 2, 2),
       lty = c(1, 2, 1))

# Intervalle de confiance pour la moyenne
marge_erreur <- z_critique * sigma / sqrt(n)
IC_inf <- moyenne - marge_erreur
IC_sup <- moyenne + marge_erreur

cat(sprintf("\nIntervalle de confiance à %.0f%% pour la moyenne: [%.4f, %.4f]\n",
            (1-alpha)*100, IC_inf, IC_sup))