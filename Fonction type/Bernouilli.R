# Exemple 1
# Définir la probabilité de succès
p <- 0.4

# Calculer la probabilité de succès
prob_succes <- dbinom(x=1, size=1, prob=p)

# Afficher le résultat
print(paste("La probabilité de succès est:", prob_succes))

# Exemple 2
# Définir la probabilité de succès
p <- 0.4

# Calculer la probabilité d'échec
prob_echec <- dbinom(x=0, size=1, prob=p)

# Afficher le résultat
print(paste("La probabilité d'échec est:", prob_echec))

# Exemple 3
# Définir la probabilité de succès
p <- 0.4

# Définir le nombre d'expériences
n <- 1000

# Simuler les expériences de Bernoulli
experiences <- rbinom(n=n, size=1, prob=p)

# Calculer la proportion de succès
proportion_succes <- mean(experiences)

# Afficher le résultat
print(paste("La proportion de succès sur", n, "expériences est:", proportion_succes))