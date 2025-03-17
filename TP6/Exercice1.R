# https://github.com/BreizhHardware/TD-R/tree/main/TP6

# Données observées
observed <- c(B = 84, J = 79, R = 75, O = 49, V = 36, D = 47)
total <- sum(observed)

# Proportions attendues selon le responsable de communication
expected_props <- c(B = 0.3, J = 0.2, R = 0.2, O = 0.1, V = 0.1, D = 0.1)

# Calcul des effectifs attendus
expected <- total * expected_props

# Test du chi-deux
chi_squared <- sum((observed - expected)^2 / expected)
degrees_freedom <- length(observed) - 1
p_value <- pchisq(chi_squared, df = degrees_freedom, lower.tail = FALSE)

# Valeur critique pour alpha = 0.05
alpha <- 0.05
critical_value <- qchisq(1 - alpha, df = degrees_freedom)

# Affichage des résultats
cat("Analyse de la distribution des couleurs des bonbons Yopy\n")
cat("--------------------------------------------------\n")
cat(sprintf("Nombre total de bonbons: %d\n\n", total))

# Tableau des effectifs observés et attendus
colors <- c("Brun", "Jaune", "Rouge", "Orange", "Vert", "Doré")
cat("Couleur  Observé  Attendu  Ecart (%) \n")
cat("--------------------------------\n")
for (i in seq_along(observed)) {
  ecart <- sqrt(((observed[i] - expected[i])^2 / expected[i]) * 100)
  cat(sprintf("%-8s  %4d     %5.1f    %6.3f\n",
              colors[i], observed[i], expected[i], ecart))
}

# Résultats du test
cat("\nTest du chi-deux\n")
cat("--------------\n")
cat("H0: La distribution suit les proportions annoncées.\n")
cat("H1: La distribution ne suit pas les proportions annoncées.\n\n")
cat(sprintf("Chi-deux calculé: %.3f\n", chi_squared))
cat(sprintf("Degrés de liberté: %d\n", degrees_freedom))
cat(sprintf("Valeur critique (α = %.2f): %.3f\n", alpha, critical_value))
cat(sprintf("p-value: %.6f\n\n", p_value))

# Conclusion
cat("Conclusion:\n")
if (chi_squared > critical_value) {
  cat(sprintf("Comme χ² = %.3f > %.3f et p-value = %.6f < %.2f, nous rejetons H0.\n",
              chi_squared, critical_value, p_value, alpha))
  cat("On peut affirmer, avec un risque de 5%, que le responsable de la communication a tort.\n")
} else {
  cat(sprintf("Comme χ² = %.3f < %.3f et p-value = %.6f > %.2f, nous ne pouvons pas rejeter H0.\n",
              chi_squared, critical_value, p_value, alpha))
  cat("On ne peut pas affirmer que le responsable de la communication a tort.\n")
}

# Vérification avec la fonction intégrée
cat("\nRésultat avec la fonction intégrée chisq.test():\n")
chi_test <- chisq.test(observed, p = expected_props)

# Graphique comparatif
barplot_data <- rbind(observed, expected)
colnames(barplot_data) <- colors
barplot(barplot_data, beside = TRUE, col = c("lightblue", "lightgreen"),
        main = "Distribution des couleurs des bonbons Yopy",
        ylab = "Nombre de bonbons", ylim = c(0, max(barplot_data) * 1.2))
legend("topright", legend = c("Observé", "Attendu"), fill = c("lightblue", "lightgreen"))

# Vérification avec un test de Kolmogorov-Smirnov
cat("\nTest de Kolmogorov-Smirnov\n")
cat("-------------------------\n")
cat("H0: Les distributions observée et attendue sont identiques.\n")
cat("H1: Les distributions observée et attendue sont différentes.\n\n")

# Créer des échantillons pour le test KS en répétant chaque catégorie selon sa fréquence
observed_sample <- rep(seq_along(observed), observed)
expected_sample <- rep(seq_along(expected), round(expected))

# Exécuter le test KS
ks_test <- ks.test(observed_sample, expected_sample)
cat("Résultat avec la fonction intégrée ks.test():\n")
print(ks_test)

# Comparaison avec le test du chi-deux dans un tableau
cat("\nComparaison des p values\n")
cat("-------------------\n")
cat("KS test  Chi-deux test\n")
  cat(sprintf("%.3f    %.3f\n",
              ks_test$p.value, chi_test$p.value))
