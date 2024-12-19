# Exemple 1
# Définir les paramètres de la loi binomiale
n <- 70  # nombre d'essais
p <- 0.4  # probabilité de succès

# Calculer la probabilité d'obtenir au plus 30 succès
prob_at_most_30 <- sum(dbinom(x=0:30, size=n, prob=p))

# Afficher le résultat
print(paste("La probabilité d'obtenir au plus 30 succès est:", prob_at_most_30))

# Exemple 2
# Définir les paramètres de la loi binomiale
n <- 70  # nombre d'essais
p <- 0.4  # probabilité de succès

# Initialiser les variables
i <- 0
j <- 0

# Calculer la probabilité cumulative jusqu'à atteindre 0.9
while (i < 0.9) {
  i <- sum(dbinom(x=0:j, size=n, prob=p))
  j <- j + 1
}

# Afficher le résultat
print(paste("La probabilité cumulative atteint 0.9 à:", j - 1))

# Exemple 3
# Définir les paramètres de la loi binomiale
n <- 70  # nombre d'essais
p <- 0.4  # probabilité de succès

# Calculer la distribution binomiale
distribution <- dbinom(x=0:n, size=n, prob=p)

# Afficher la distribution
print(distribution)