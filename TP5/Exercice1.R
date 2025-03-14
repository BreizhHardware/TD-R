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

# Nombre de classes pour l'histogramme
k <- ceiling(sqrt(n))

# Affichage des statistiques descriptives
cat("Analyse des épaisseurs des composants isolants\n")
cat("---------------------------------------------\n")
cat(sprintf("Nombre d'observations: %d\n", n))
cat(sprintf("Moyenne des épaisseurs: %.4f mm\n", moyenne))
cat(sprintf("Écart-type échantillon: %.4f mm\n", ecart_type_echantillon))
cat(sprintf("Écart-type connu: %.4f mm\n", sigma))

# Création des classes pour l'histogramme et calcul des fréquences observées
hist_obj <- hist(epaisseurs, plot = FALSE, breaks = k)
observed_counts <- hist_obj$counts
breaks <- hist_obj$breaks
mids <- hist_obj$mids

# Calcul des fréquences théoriques selon N(7.3, 0.38²)
expected_probs <- diff(pnorm(breaks, mean = mu0, sd = sigma))
expected_counts <- expected_probs * n

# Test du chi-deux
chi_stat <- sum((observed_counts - expected_counts)^2 / expected_counts)
df <- length(observed_counts) - 1  # Degrés de liberté
p_value_chi <- pchisq(chi_stat, df = df, lower.tail = FALSE)

# Valeur critique pour le chi-deux
chi_critique <- qchisq(1 - alpha, df = df)

# Affichage des résultats du test chi-deux
cat("\nTest d'hypothèse (chi-deux)\n")
cat(sprintf("H0: La distribution suit une loi N(%.1f, %.2f²) (le fournisseur respecte ses engagements)\n", mu0, sigma))
cat(sprintf("H1: La distribution ne suit pas une loi N(%.1f, %.2f²) (le fournisseur ne respecte pas ses engagements)\n", mu0, sigma))
cat(sprintf("Statistique chi-deux calculée: %.4f\n", chi_stat))
cat(sprintf("Valeur critique (α = %.2f, dl = %d): %.4f\n", alpha, df, chi_critique))
cat(sprintf("p-value: %.4f\n", p_value_chi))

# Conclusion du test chi-deux
cat("\nConclusion:\n")
if (chi_stat > chi_critique) {
  cat(sprintf("Comme χ² = %.4f > %.4f et p-value = %.4f < %.2f, nous rejetons H0.\n",
              chi_stat, chi_critique, p_value_chi, alpha))
  cat("L'entreprise peut affirmer, avec un risque faible de se tromper, que le fournisseur ne respecte pas ses engagements.\n")
} else {
  cat(sprintf("Comme χ² = %.4f < %.4f et p-value = %.4f > %.2f, nous ne pouvons pas rejeter H0.\n",
              chi_stat, chi_critique, p_value_chi, alpha))
  cat("L'entreprise ne peut pas affirmer, avec un risque faible de se tromper, que le fournisseur ne respecte pas ses engagements.\n")
}

# Représentation graphique
hist(epaisseurs,
     breaks = breaks,
     col = "lightblue",
     main = "Distribution des épaisseurs des composants",
     xlab = "Épaisseur (mm)",
     ylab = "Fréquence",
     freq = TRUE)

# Tracer la distribution théorique
curve(dnorm(x, mean = mu0, sd = sigma) * n * (breaks[2] - breaks[1]),
      col = "darkblue",
      lwd = 2,
      add = TRUE)

# Ajout de lignes verticales pour la moyenne échantillon et la moyenne annoncée
abline(v = moyenne, col = "red", lwd = 2)
abline(v = mu0, col = "blue", lwd = 2, lty = 2)

# Légende
legend("topright",
       legend = c(paste("Moyenne échantillon:", round(moyenne, 4), "mm"),
                  paste("Moyenne annoncée:", mu0, "mm"),
                  "Distribution théorique N(7.3, 0.38²)"),
       col = c("red", "blue", "darkblue"),
       lwd = c(2, 2, 2),
       lty = c(1, 2, 1))

# Alternative avec la fonction intégrée
cat("\nRésultat avec la fonction intégrée chisq.test():\n")
chitest <- chisq.test(observed_counts, p = expected_probs, rescale.p = TRUE)
print(chitest)