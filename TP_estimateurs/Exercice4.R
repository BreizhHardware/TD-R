# https://github.com/BreizhHardware/TD-R/tree/main/TP_estimateurs

# 1. Si l'automate est réglée sur une moyenne de 10,5 kg, quelle est la probabilité qu'un sac
# conƟent moins de 10 kg de farine ?

# Paramètres
mu <- 10.5      # moyenne
sigma <- 0.6    # écart-type
x <- 10         # valeur seuil

# Calcul de la probabilité P(X < 10)
proba <- pnorm(x, mean = mu, sd = sigma)

# Affichage du résultat
cat("P(X < 10) =", round(proba, 4), "\n")

# Visualisation graphique
curve(dnorm(x, mean = mu, sd = sigma),
      from = 8,
      to = 13,
      main = "Distribution du poids des sacs de farine",
      xlab = "Poids (kg)",
      ylab = "Densité")

# Zone de probabilité avec transparence
x_values <- seq(8, 10, length.out = 100)
polygon(c(x_values, 10),
        c(dnorm(x_values, mu, sigma), 0),
        col = rgb(1, 0, 0, 0.5))

# Légende
abline(v = 10, col = "red", lty = 2)
text(10, 0.2, "10 kg", pos = 4)

# 2. Quelle devrait être la valeur de la moyenne afin que le risque qu'un sac conƟenne moins de
# 10 kilos soit limité à 5% l'écart type restant toujours à 0,6 kg.

# Paramètres connus
sigma <- 0.6    # écart-type
x <- 10         # seuil
alpha <- 0.05   # risque souhaité

# Calcul de la moyenne requise
# Si P(X < 10) = 0.05, alors 10 = µ + z_{0.05}*σ
# où z_{0.05} est le quantile d'ordre 0.05 de la loi normale centrée réduite
mu_requise <- x - qnorm(alpha) * sigma

# Affichage du résultat
cat("La moyenne doit être réglée sur", round(mu_requise, 3), "kg\n")

# 3. L'automate est réglée sur 10,8 kg et les sacs sont livrés par paleƩes de 20 sacs quelle est la
# probabilité que le poids total de farine sur une paleƩe dépasse 222 kg ?

# Paramètres pour un sac
mu_sac <- 10.8      # moyenne par sac
sigma_sac <- 0.6    # écart-type par sac
n_sacs <- 20        # nombre de sacs par palette

# Paramètres pour la palette (somme des sacs)
mu_palette <- n_sacs * mu_sac
sigma_palette <- sqrt(n_sacs) * sigma_sac

# Seuil
seuil <- 222

# Calcul de P(S > 222)
proba <- 1 - pnorm(seuil, mean = mu_palette, sd = sigma_palette)

# Affichage du résultat
cat("P(S > 222) =", round(proba, 4), "\n")