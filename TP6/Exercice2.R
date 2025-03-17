# https://github.com/BreizhHardware/TD-R/tree/main/TP6

# Données du problème
mois <- 1:12
admissions <- c(18, 16, 8, 10, 6, 4, 4, 9, 11, 10, 12, 12)
n_total <- sum(admissions)

# Test d'ajustement du chi-deux
# H0: Distribution uniforme (entrées réparties au hasard)
# H1: Distribution non uniforme (certains mois plus propices)

# Valeurs attendues sous H0 (distribution uniforme)
attendues <- rep(n_total / 12, 12)  # 10 admissions par mois

# Niveau de risque
alpha <- 0.01

# Calcul du chi-deux
chi_2 <- sum((admissions - attendues)^2 / attendues)

# Degrés de liberté
df <- 12 - 1

# Valeur critique
val_critique <- qchisq(1 - alpha, df)

# p-value
p_value <- pchisq(chi_2, df, lower.tail = FALSE)

# Affichage des résultats
cat("Analyse de la répartition des entrées à l'hôpital par mois\n")
cat("--------------------------------------------------------\n")
cat(sprintf("Nombre total d'admissions: %d\n", n_total))
cat(sprintf("Statistique du chi-deux calculée: %.4f\n", chi_2))
cat(sprintf("Degrés de liberté: %d\n", df))
cat(sprintf("Valeur critique (α = %.2f): %.4f\n", alpha, val_critique))
cat(sprintf("p-value: %.4f\n", p_value))

# Conclusion
cat("\nConclusion:\n")
if (chi_2 > val_critique) {
  cat(sprintf("Comme χ² = %.4f > %.4f et p-value = %.4f < %.2f, nous rejetons H0.\n",
              chi_2, val_critique, p_value, alpha))
  cat("On peut affirmer, avec un risque de 1%, que les entrées ne se font pas au hasard dans l'année.\n")
} else {
  cat(sprintf("Comme χ² = %.4f < %.4f et p-value = %.4f > %.2f, nous ne pouvons pas rejeter H0.\n",
              chi_2, val_critique, p_value, alpha))
  cat("On ne peut pas affirmer, avec un risque de 1%, que certains mois sont plus propices à la maladie.\n")
}

# Vérification avec la fonction intégrée
chi_test <- chisq.test(admissions, p = rep(1/12, 12))
cat("\nRésultat avec la fonction intégrée chisq.test():\n")
print(chi_test)

# Représentation graphique
barplot(admissions, names.arg = mois,
        col = "lightblue",
        main = "Répartition des entrées à l'hôpital par mois",
        xlab = "Mois",
        ylab = "Nombre d'entrées")

# Ajouter une ligne horizontale pour la valeur attendue
abline(h = n_total / 12, col = "red", lwd = 2)
legend("topright", legend = "Valeur attendue (distribution uniforme)",
       col = "red", lwd = 2)

# Afficher les contributions au chi-deux par mois
cat("\nComparaison des effectifs observés et attendus par mois:\n")
cat("Mois | Observés | Attendus | Ecart (%)\n")
cat("---------------------------------\n")
for(i in 1:12) {
  ecart <- sqrt(((admissions[i] - attendues[i])^2 / attendues[i]) * 100)
  cat(sprintf("%4d | %8d | %8.1f | %8.4f\n", i, admissions[i], attendues[i], ecart))
}

# Vérification avec un test de Kolmogorov-Smirnov
cat("\nTest de Kolmogorov-Smirnov\n")
cat("-------------------------\n")
cat("H0: Les distributions observée et attendue sont identiques.\n")

# Convertir en distribution cumulative pour KS test
prob_theorique <- rep(1/12, 12)
ks_test <- ks.test(admissions/sum(admissions), prob_theorique)
print(ks_test)

# Comparaison avec le test du chi-deux dans un tableau
cat("\nComparaison des p values\n")
cat("-------------------\n")
cat("KS test  Chi-deux test\n")
cat(sprintf("%.3f    %.3f\n",
            ks_test$p.value, chi_test$p.value))
