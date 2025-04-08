# https://github.com/BreizhHardware/TD-R/tree/main/Big%20Data/TP2
# 1.Modéliser le problème à l’aide d’une régression logistique.

donnees <- read.table("Big Data/TP3/anesthesie.txt", header = TRUE)

cat("Aperçu des données :\n")
print(head(donnees))

cat("\nRésumé statistique :\n")
print(summary(donnees))

# Visualisation des données
plot(donnees$X1, donnees$Y,
     xlab = "Dosage d'agent anesthésique",
     ylab = "Mouvement (1=oui, 0=non)",
     main = "Relation entre dosage et mouvement",
     pch = 19)

modele_logit <- glm(Y ~ X1, data = donnees, family = binomial(link = "logit"))

# Résultats du modèle
cat("\n1. Régression logistique : Y en fonction de X1\n")
resume <- summary(modele_logit)
print(resume)

# Interprétation des coefficients
cat("\nInterprétation :\n")
cat("- Coefficient X1 :", round(coef(modele_logit)[2], 4),
    "\n- Odds ratio :", round(exp(coef(modele_logit)[2]), 4),
    " (réduction des chances de bouger quand le dosage augmente d'une unité)\n")

# Courbe de régression logistique
curve(predict(modele_logit, data.frame(X1=x), type="response"),
      add=TRUE, col="red", lwd=2,
      from=min(donnees$X1), to=max(donnees$X1))

# Qualité d'ajustement
cat("\nQualité du modèle :\n")
cat("- AIC :", round(modele_logit$aic, 2), "\n")
cat("- Déviance résiduelle :", round(modele_logit$deviance, 2),
    "avec", modele_logit$df.residual, "degrés de liberté\n")

# Prédictions et classification
seuil <- 0.5
proba <- predict(modele_logit, type = "response")
pred <- ifelse(proba > seuil, 1, 0)
matrice_confusion <- table(Prédit = pred, Observé = donnees$Y)

cat("\nMatrice de confusion (seuil = 0.5) :\n")
print(matrice_confusion)

# Taux de bon classement
taux_bon_classement <- sum(diag(matrice_confusion)) / sum(matrice_confusion)
cat("\nTaux de bon classement :", round(taux_bon_classement * 100, 2), "%\n")

# 2.Est-ce qu’un patient ayant eu pour dosage X1 = 1.25 a plus de chance de bouger que de ne pas bouger ?

nouveau_patient <- data.frame(X1 = 1.25)
proba_bouger <- predict(modele_logit, nouveau_patient, type = "response")

cat("\nPour un dosage X1 = 1.25 :\n")
cat("Probabilité de bouger :", round(proba_bouger, 4), "\n")
cat("Probabilité de ne pas bouger :", round(1 - proba_bouger, 4), "\n")

if (proba_bouger > 0.5) {
  cat("Le patient a plus de chance de bouger que de ne pas bouger.\n")
} else if (proba_bouger < 0.5) {
  cat("Le patient a plus de chance de ne pas bouger que de bouger.\n")
} else {
  cat("Le patient a autant de chances de bouger que de ne pas bouger.\n")
}

# 3.Faire le graphique des données et de la courbe de régression logistique.
# Graphique amélioré de la régression logistique
plot(donnees$X1, donnees$Y,
     xlab = "Dosage d'agent anesthésique (X1)",
     ylab = "Mouvement (1=oui, 0=non)",
     main = "Relation entre dosage et mouvement avec régression logistique",
     pch = 19,
     col = ifelse(donnees$Y == 1, "red", "blue"),
     xlim = c(0.7, 2.6),
     ylim = c(-0.05, 1.05))

# Légende pour les observations
legend("topright",
       legend = c("Mouvement", "Pas de mouvement"),
       col = c("red", "blue"),
       pch = 19,
       bty = "n")

# Séquence de valeurs X1 pour une courbe lisse
x_seq <- seq(min(donnees$X1) - 0.1, max(donnees$X1) + 0.1, length.out = 100)

# Calcul des probabilités prédites
y_pred <- predict(modele_logit, newdata = data.frame(X1 = x_seq), type = "response")

# Tracé de la courbe de régression logistique
lines(x_seq, y_pred, col = "darkgreen", lwd = 2)

# Ligne horizontale pour le seuil de décision
abline(h = 0.5, lty = 2, col = "gray50")

# Annotation du seuil de décision
text(2.4, 0.52, "Seuil de décision (p=0.5)", col = "gray50", cex = 0.8)

# Mise en évidence du point X1 = 1.25
points(1.25, predict(modele_logit, newdata = data.frame(X1 = 1.25), type = "response"),
       col = "purple", pch = 19, cex = 1.5)
text(1.25, predict(modele_logit, newdata = data.frame(X1 = 1.25), type = "response") + 0.07,
     "X1 = 1.25", col = "purple", cex = 0.8)

# Légende pour la courbe
legend("bottomleft",
       legend = "Courbe de régression logistique",
       col = "darkgreen",
       lwd = 2,
       bty = "n")