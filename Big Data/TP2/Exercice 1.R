# https://github.com/BreizhHardware/TD-R/tree/main/Big%20Data/TP2
# 1. Représenter le nuage de points {(��ଵ, ��ଵ), … , (��௡, ��௡)}. À partir de celui-ci, expliquer pourquoi on peut envisager l’existence d’une liaison linéaire entre Y et X.

# Définition des données
prix <- c(420, 380, 350, 400, 440, 380, 450, 420)
ventes <- c(5.5, 6, 6.5, 6, 5, 6.5, 4.5, 5)

# Création d'un dataframe
donnees <- data.frame(prix = prix, ventes = ventes)

plot(donnees$prix, donnees$ventes,
     main = "Relation entre prix et ventes",
     xlab = "Prix du produit",
     ylab = "Nombre de ventes",
     pch = 19, col = "blue")

cat("On observe une tendance linéaire négative: quand le prix augmente, les ventes diminuent\n")

# 2. On adopte alors le modèle de régression linéaire simple : ∀��, ��௜ = β଴ + βଵ��௜ +∈௜ . Les paramètres ��଴ et��ଵ sont des réels inconnus. On considère la forme matricielle usuelle : Y = X�� + �� , avec β = (��଴ ��ଵ ) . Créer dans R la matrice X associée.
X <- cbind(1, prix)
print(X)

# 3. En posant y = ( ௧ ��ଵ, . . ., ��௡) , calculer �� = ൫ �� ௧ ��൯ ିଵ �� ௧ ��. Que représente b par rapport  à β ?
Y <- matrix(ventes, ncol = 1)
beta <- solve(t(X) %*% X) %*% t(X) %*% Y
print(beta)

# 4. Vérifier que l’on a b = ( ௧ b0 ,b1 ) , avec  b1= ଵ ∑ (௫೔ି௫̅) ೙ మ ೔సభ ∑ (��௜ ௡ ௜ୀଵ − ��̅)(��௜ − ��ത)  b0 = ��ത – b1��̅.  Retrouver ces résultats numériques avec les commandes lm et coef
x_mean <- mean(prix)
y_mean <- mean(ventes)

# Calcul manuel de b1
numerateur <- sum((prix - x_mean) * (ventes - y_mean))
denominateur <- sum((prix - x_mean)^2)
b1_manuel <- numerateur / denominateur

# Calcul manuel de b0
b0_manuel <- y_mean - b1_manuel * x_mean

# Affichage des coefficients manuels
cat("b0 (manuel) =", b0_manuel, "\n")
cat("b1 (manuel) =", b1_manuel, "\n")

# Vérification avec lm()
modele <- lm(ventes ~ prix, data = donnees)
coef_lm <- coef(modele)
cat("b0 (lm) =", coef_lm[1], "\n")
cat("b1 (lm) =", coef_lm[2], "\n")

# 5.  Tracer la droite de régression sur le nuage de points.
plot(donnees$prix, donnees$ventes,
     main = "Régression linéaire: Prix vs Ventes",
     xlab = "Prix du produit",
     ylab = "Nombre de ventes",
     pch = 19, col = "blue")
abline(modele, col = "red", lwd = 2)

# Calcul des valeurs prédites
y_pred <- predict(modele, data.frame(prix = prix))

# Ajout des traits reliant les points à la droite
for (i in 1:length(prix)) {
  segments(prix[i], ventes[i], prix[i], y_pred[i], col = "purple", lty = 2)
}

# Ajout d'une légende
legend("topright",
       legend = c("Observations", "Droite de régression", "Résidus"),
       col = c("blue", "red", "purple"),
       pch = c(19, NA, NA),
       lty = c(NA, 1, 2),
       lwd = c(NA, 2, 1))

# 6. Calculer "à la main" le coefficient de détermination et le coefficient de détermination  ajusté. Est-ce que le modèle de régression linéaire simple est pertinent avec les données ?
y_pred <- X %*% beta  # Valeurs prédites
TSS <- sum((ventes - y_mean)^2)  # Somme totale des carrés
RSS <- sum((ventes - y_pred)^2)  # Somme des carrés des résidus

R_carre <- 1 - RSS/TSS
cat("R² =", R_carre, "\n")

# Calcul du R² ajusté
n <- length(ventes)
p <- 1  # Nombre de variables explicatives (hors intercept)
R_carre_ajuste <- 1 - (RSS/(n-p-1))/(TSS/(n-1))
cat("R² ajusté =", R_carre_ajuste, "\n")

# 7. Retrouver les estimations précédentes avec la commande summary.
resume <- summary(modele)
print(resume)