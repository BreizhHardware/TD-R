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
