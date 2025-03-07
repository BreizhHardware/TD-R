# https://github.com/BreizhHardware/TD-R/tree/main/TP_estimateurs

# 1. Quelle est la loi de F et montrer qu'elle peut être approchée par une loi normale dont on donnera les paramètres

# Paramètres
n <- 50  # taille de l'échantillon
p <- 0.13  # proportion de gauchers

# Loi exacte : Binomiale(n,p)
# F ~ B(50, 0.13)

cat("Loi de F : F ~ B(50, 0.13)\n")

# Paramètres de l'approximation normale
mu <- n * p  # moyenne
sigma <- sqrt(n * p * (1-p))  # écart-type

cat("Paramètres de l'approximation normale :\n")
cat("μ =", round(mu, 2), "\n")
cat("σ =", round(sigma, 2), "\n")

# Visualisation de la comparaison
x <- 0:15
plot(x, dbinom(x, n, p),
     type = "h",
     lwd = 2,
     col = "blue",
     main = "Comparaison Binomiale vs Normal",
     xlab = "Nombre de gauchers",
     ylab = "Densité de probabilité")

# Superposition de la courbe normale
curve(dnorm(x, mean = mu, sd = sigma),
      add = TRUE,
      col = "red",
      lwd = 2)

legend("topright",
       legend = c("Binomiale", "Normale"),
       col = c("blue", "red"),
       lwd = 2)

# 2. Calculer la probabilité d'observer plus de 5 gauchers dans l'échanƟllon de façon exacte puis
# approchée.

# Paramètres
n <- 50
p <- 0.13
mu <- n * p

# Calcul exact avec la loi binomiale
# P(F > 5) = 1 - P(F ≤ 5)
proba_exacte <- 1 - pbinom(5, n, p)

# Calcul approché avec la loi normale
# Avec correction de continuité : P(F > 5) = P(F ≥ 6) = P(F > 5.5)
proba_approx <- 1 - pnorm(5.5, mu, sigma)

# Affichage des résultats
cat("Probabilité exacte P(F > 5):", round(proba_exacte, 4), "\n")
cat("Probabilité approchée P(F > 5):", round(proba_approx, 4), "\n")