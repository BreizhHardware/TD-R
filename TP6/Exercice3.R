# https://github.com/BreizhHardware/TD-R/tree/main/TP6

# Données du problème
attaques <- 0:7
nb_fruits <- c(60, 105, 65, 47, 15, 4, 3, 1)
total <- sum(nb_fruits)

# Estimation du paramètre λ
lambda_est <- sum(attaques * nb_fruits) / total

# Fréquences attendues selon une loi de Poisson P(λ)
frequences_attendues <- dpois(attaques, lambda = lambda_est) * total

# Affichage des résultats
cat("Analyse du comportement des insectes attaquant les fruits\n")
cat("------------------------------------------------------\n")
cat(sprintf("Nombre total de fruits : %d\n", total))
cat(sprintf("Paramètre λ estimé : %.4f\n\n", lambda_est))

cat("Nombre d'attaques | Observé | Attendu (Poisson) | Écart\n")
cat("--------------------------------------------------------\n")
for (i in seq_along(attaques)) {
  ecart <- (nb_fruits[i] - frequences_attendues[i])^2 / frequences_attendues[i]
  cat(sprintf("%16d | %7d | %16.2f | %5.2f\n",
              attaques[i], nb_fruits[i], frequences_attendues[i], ecart))
}

# Test d'ajustement du chi-deux
chi_squared <- sum((nb_fruits - frequences_attendues)^2 / frequences_attendues)
df <- length(attaques) - 1 - 1  # -1 pour le paramètre estimé
p_value <- pchisq(chi_squared, df, lower.tail = FALSE)
alpha <- 0.05

# Résultats du test
cat("\nTest du chi-deux d'ajustement à une loi de Poisson\n")
cat("------------------------------------------------\n")
cat("H0: Les attaques suivent une loi de Poisson (attaques indépendantes)\n")
cat("H1: Les attaques ne suivent pas une loi de Poisson (comportement grégaire)\n\n")
cat(sprintf("Chi-deux calculé: %.4f\n", chi_squared))
cat(sprintf("Degrés de liberté: %d\n", df))
cat(sprintf("p-value: %.4f\n", p_value))

# Conclusion
cat("\nConclusion:\n")
if (p_value < alpha) {
  cat(sprintf("Comme p-value = %.4f < %.2f, nous rejetons H0.\n", p_value, alpha))
  cat("Le comportement des insectes est significativement grégaire.\n")
} else {
  cat(sprintf("Comme p-value = %.4f > %.2f, nous ne pouvons pas rejeter H0.\n", p_value, alpha))
  cat("On ne peut pas affirmer que le comportement des insectes est significativement grégaire.\n")
}

# Vérification avec la fonction intégrée
chi_test <- chisq.test(nb_fruits, p = dpois(attaques, lambda = lambda_est), rescale.p = TRUE)
cat("\nRésultat avec la fonction intégrée chisq.test():\n")
print(chi_test)

# Graphique comparatif
barplot_data <- rbind(nb_fruits, frequences_attendues)
barplot(barplot_data, beside = TRUE, col = c("lightblue", "lightgreen"),
        names.arg = attaques,
        main = "Distribution des attaques d'insectes sur les fruits",
        xlab = "Nombre d'attaques",
        ylab = "Nombre de fruits")
legend("topright", legend = c("Observé", "Attendu (Poisson)"),
       fill = c("lightblue", "lightgreen"))