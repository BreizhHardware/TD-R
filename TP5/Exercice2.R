# Données des contenus en litres
contenus <- c(10.1, 9.8, 10.2, 10.3, 10.4, 9.8, 9.9, 10.4, 10.2, 9.5, 10.4, 9.6)

# Paramètres
n <- length(contenus)         # Taille de l'échantillon
mu0 <- 10                     # Valeur de référence à tester
alpha <- 0.05                 # Niveau de signification

# Statistiques descriptives
moyenne <- mean(contenus)
ecart_type <- sd(contenus)
erreur_standard <- ecart_type / sqrt(n)

# Affichage des statistiques descriptives
cat("Analyse du contenu des récipients en plastique\n")
cat("--------------------------------------------\n")
cat(sprintf("Nombre d'échantillons: %d\n", n))
cat(sprintf("Contenu moyen: %.4f litres\n", moyenne))
cat(sprintf("Écart-type échantillon: %.4f litres\n", ecart_type))

# Nombre de classes pour l'histogramme (adapté pour petit échantillon)
k <- ceiling(sqrt(n))  # Formule classique: racine carrée de n

# Création des classes pour l'histogramme et calcul des fréquences observées
hist_obj <- hist(contenus, plot = FALSE, breaks = k)
observed_counts <- hist_obj$counts
breaks <- hist_obj$breaks
mids <- hist_obj$mids

# Calcul des fréquences théoriques selon N(10, σ²)
expected_probs <- diff(pnorm(breaks, mean = mu0, sd = ecart_type))
expected_counts <- expected_probs * n

# Test du chi-deux
chi_stat <- sum((observed_counts - expected_counts)^2 / expected_counts)
df <- length(observed_counts) - 1  # Degrés de liberté
p_value_chi <- pchisq(chi_stat, df = df, lower.tail = FALSE)

# Valeur critique pour le chi-deux
chi_critique <- qchisq(1 - alpha, df = df)

# Affichage des résultats du test chi-deux
cat("\nTest d'hypothèse (chi-deux)\n")
cat(sprintf("H0: La distribution suit une loi N(%.1f, σ²)\n", mu0))
cat(sprintf("H1: La distribution ne suit pas une loi N(%.1f, σ²)\n", mu0))
cat(sprintf("Statistique chi-deux calculée: %.4f\n", chi_stat))
cat(sprintf("Valeur critique (α = %.2f, dl = %d): %.4f\n", alpha, df, chi_critique))
cat(sprintf("p-value: %.4f\n", p_value_chi))

# Conclusion du test chi-deux
cat("\nConclusion:\n")
if (chi_stat > chi_critique) {
  cat(sprintf("Comme χ² = %.4f > %.4f et p-value = %.4f < %.2f, nous rejetons H0.\n",
              chi_stat, chi_critique, p_value_chi, alpha))

  if (moyenne > mu0) {
    cat("De plus, la moyenne échantillon (%.4f) est supérieure à 10.\n", moyenne)
    cat("On peut affirmer, avec un faible risque de se tromper, que le contenu moyen est strictement supérieur à 10 litres.\n")
  } else {
    cat("Cependant, la moyenne échantillon (%.4f) n'est pas supérieure à 10.\n", moyenne)
    cat("On ne peut pas conclure que le contenu moyen est strictement supérieur à 10 litres.\n")
  }
} else {
  cat(sprintf("Comme χ² = %.4f < %.4f et p-value = %.4f > %.2f, nous ne pouvons pas rejeter H0.\n",
              chi_stat, chi_critique, p_value_chi, alpha))
  cat("On ne peut pas affirmer, avec un faible risque de se tromper, que le contenu moyen est strictement supérieur à 10 litres.\n")
}

# Représentation graphique
hist(contenus,
     breaks = breaks,
     col = "lightblue",
     main = "Distribution du contenu des récipients",
     xlab = "Contenu (litres)",
     ylab = "Fréquence",
     freq = TRUE)

# Tracer la distribution théorique
curve(dnorm(x, mean = mu0, sd = ecart_type) * n * (breaks[2] - breaks[1]),
      col = "darkblue",
      lwd = 2,
      add = TRUE)

# Ajout de lignes verticales pour la moyenne échantillon et la valeur de référence
abline(v = moyenne, col = "red", lwd = 2)
abline(v = mu0, col = "blue", lwd = 2, lty = 2)

# Légende
legend("topright",
       legend = c(paste("Moyenne échantillon:", round(moyenne, 4), "L"),
                  paste("Valeur de référence:", mu0, "L"),
                  "Distribution théorique N(10, σ²)"),
       col = c("red", "blue", "darkblue"),
       lwd = c(2, 2, 2),
       lty = c(1, 2, 1))

# Alternative avec la fonction intégrée
cat("\nRésultat avec la fonction intégrée chisq.test():\n")
chitest <- chisq.test(observed_counts, p = expected_probs, rescale.p = TRUE)
print(chitest)

# Note sur la fiabilité du test avec petit échantillon
cat("\nNote sur la fiabilité: Avec seulement 12 observations, le test du chi-deux\n")
cat("peut manquer de puissance. Vérifiez que chaque classe contient au moins\n")
cat("5 observations attendues pour une meilleure fiabilité.\n")

# Afficher les effectifs par classe
cat("\nEffectifs par classe:\n")
for(i in 1:length(observed_counts)) {
  cat(sprintf("Classe %d [%.2f-%.2f]: Observés = %.0f, Attendus = %.2f\n",
              i, breaks[i], breaks[i+1], observed_counts[i], expected_counts[i]))
}