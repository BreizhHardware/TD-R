# https://github.com/BreizhHardware/TD-R/tree/main/Big%20Data/TP2
# 1. Compléter les tableaux suivants :
# Régression de la résistance à la rupture Y en fonction de l’épaisseur X1

# Création du jeu de données
resistance <- c(37.8, 22.5, 17.1, 10.8, 7.2, 42.3, 30.2, 19.4, 14.8, 9.5, 32.4, 21.6)
epaisseur <- c(4, 4, 3, 2, 1, 6, 4, 4, 1, 1, 3, 4)
densite <- c(4, 3.6, 3.1, 3.2, 3.0, 3.8, 3.8, 2.9, 3.8, 2.8, 3.4, 2.8)

donnees <- data.frame(Y = resistance, X1 = epaisseur, X2 = densite)

modele1 <- lm(Y ~ X1, data = donnees)
resume1 <- summary(modele1)

# Affichage du tableau des coefficients et erreurs-types
cat("1. Régression de la résistance à la rupture Y en fonction de l'épaisseur X1\n")
coef_table1 <- coef(resume1)
cat("Coefficients et erreurs-types:\n")
print(coef_table1)

# Tableau d'analyse de la variance
anova_table1 <- anova(modele1)
cat("\nSource de variation:\n")
print(anova_table1)

# 2. Compléter les tableaux suivants :
# Régression de la résistance à la rupture Y en fonction de la densité X2

modele2 <- lm(Y ~ X2, data = donnees)
resume2 <- summary(modele2)

cat("\n2. Régression de la résistance à la rupture Y en fonction de la densité X2\n")
coef_table2 <- coef(resume2)
cat("Coefficients et erreurs-types:\n")
print(coef_table2)

# Tableau d'analyse de la variance
anova_table2 <- anova(modele2)
cat("\nSource de variation:\n")
print(anova_table2)

# 3. Compléter les tableaux suivants :
# Régression de la résistance à la rupture Y en fonction de l’épaisseur X1 et de la densité X2

modele12 <- lm(Y ~ X1 + X2, data = donnees)
resume12 <- summary(modele12)

cat("\n3. Régression de Y en fonction de X1 et X2\n")
coef_table12 <- coef(resume12)
cat("Coefficients et erreurs-types:\n")
print(coef_table12)

# Tableau d'analyse de la variance
anova_table12 <- anova(modele12)
cat("\nSource de variation:\n")
print(anova_table12)

# 4. Quel pourcentage de variation dans la résistance à la rupture est expliquée par chacune des régressions ?
cat("\n4. Pourcentage de variation expliquée:\n")
cat("Régression avec X1: ", round(resume1$r.squared * 100, 2), "%\n")
cat("Régression avec X2: ", round(resume2$r.squared * 100, 2), "%\n")
cat("Régression avec X1 et X2: ", round(resume12$r.squared * 100, 2), "%\n")

# 5. Compléter le tableau suivant
cat("\n5. Carré moyen résiduel et écart-type des résidus:\n")
cmr1 <- sum(modele1$residuals^2) / modele1$df.residual
cmr2 <- sum(modele2$residuals^2) / modele2$df.residual
cmr12 <- sum(modele12$residuals^2) / modele12$df.residual

cat("Régression due à X1: CMR =", round(cmr1, 4), ", σ =", round(sqrt(cmr1), 4), "\n")
cat("Régression due à X2: CMR =", round(cmr2, 4), ", σ =", round(sqrt(cmr2), 4), "\n")
cat("Régression due à (X1,X2): CMR =", round(cmr12, 4), ", σ =", round(sqrt(cmr12), 4), "\n")

# 6. Compléter le tableau d’analyse de la variance suivant pour la régression comportant les deux variables explicatives

anova_complete <- anova(modele12)
SCR_X1X2 <- sum(anova_complete$"Sum Sq")
SCT <- SCR_X1X2 + sum(modele12$residuals^2)

cat("\n6. Tableau ANOVA complet:\n")
cat("Source de variation | Somme des carrés | Ddl | Carrés moyens | Fobs\n")
cat("Régression due à (X1,X2) | ", round(SCR_X1X2, 4), " | ",
    anova_complete$Df[1] + anova_complete$Df[2], " | ",
    round(SCR_X1X2/(anova_complete$Df[1] + anova_complete$Df[2]), 4), " | ",
    round(resume12$fstatistic[1], 4), "\n")
cat("Résiduelle | ", round(sum(modele12$residuals^2), 4), " | ", modele12$df.residual,
    " | ", round(cmr12, 4), " | \n")
cat("Totale | ", round(SCT, 4), " | ", nrow(donnees)-1, " | | \n")

# 7. Tester au seuil de signification 5%, l’hypothèse nulle H0 : β1 = β2 = 0 contre  l’hypothèse alternative H1 : au moins un des coefficients est différent de 0.  Quelle est votre conclusion ?

cat("\n7. Test de H0: β1 = β2 = 0\n")
f_obs <- resume12$fstatistic[1]
f_crit <- qf(0.95, resume12$fstatistic[2], resume12$fstatistic[3])
cat("F observé =", round(f_obs, 4), ", F critique (α=5%) =", round(f_crit, 4), "\n")
p_value <- pf(f_obs, resume12$fstatistic[2], resume12$fstatistic[3], lower.tail = FALSE)
cat("p-value =", format(p_value, scientific = FALSE), "\n")
cat("Conclusion: On rejette H0, au moins un des coefficients est différent de 0.\n")

# 8. Dans le cas du modèle de régression linéaire ne comportant que l’épaisseur du  matériau comme variable explicative, déterminer un intervalle de confiance à  95% pour ��1. Pouvons-nous affirmer, au seuil de signification 5%, que la  régression linéaire est significative entre la résistance à la rupture et l’épaisseur  du matériau ? Justifier votre conclusion.

cat("\n8. Intervalle de confiance pour β1:\n")
ic_beta1 <- confint(modele1, "X1", level = 0.95)
cat("IC à 95% pour β1 :", round(ic_beta1[1], 4), "à", round(ic_beta1[2], 4), "\n")

t_obs <- coef(summary(modele1))["X1", "t value"]
t_crit <- qt(0.975, modele1$df.residual)
p_val <- coef(summary(modele1))["X1", "Pr(>|t|)"]
cat("t observé =", round(t_obs, 4), ", t critique (α=5%) =", round(t_crit, 4), "\n")
cat("Conclusion: La régression linéaire entre Y et X1 est significative (p-value =",
    format(p_val, scientific = FALSE), ")\n")

# 9. Quel est l’apport marginal de la variable X2 lorsqu’elle est introduite à la suite de la variable X1 ?
cat("\n9. Apport marginal de X2 après X1:\n")
modele_seq <- anova(modele1, modele12)
print(modele_seq)
cat("L'apport marginal de X2 est significatif (p-value =",
    format(modele_seq[2, "Pr(>F)"], scientific = FALSE), ")\n")


# 10. Nous voulons obtenir diverses estimations et prévisions de la résistance à la  rupture. Quelle est, en moyenne, la résistance à la rupture de jouets dont  l’épaisseur du matériau utilisé et la densité du matériau sont celles indiquées  dans le tableau suivant ?

cat("\n10. Estimations de la résistance à la rupture:\n")
nouvelles_donnees <- data.frame(
  X1 = c(4, 3, 4),
  X2 = c(3.8, 3.4, 2.9)
)

predictions <- predict(modele12, nouvelles_donnees, interval = "confidence")
rownames(predictions) <- c("X1=4, X2=3.8", "X1=3, X2=3.4", "X1=4, X2=2.9")
print(round(predictions, 4))