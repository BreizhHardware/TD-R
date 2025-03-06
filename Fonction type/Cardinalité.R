# Exemple 1
# Définir l'ensemble
ensemble <- c(1, 2, 3, 4, 5)

# Calculer la cardinalité de l'ensemble
cardinalite <- length(ensemble)

# Afficher le résultat
print(paste("La cardinalité de l'ensemble est:", cardinalite))

# Exemple 2
# Définir l'ensemble avec des éléments répétés
ensemble <- c(1, 2, 2, 3, 4, 4, 5)

# Calculer la cardinalité de l'ensemble avec des éléments uniques
cardinalite_unique <- length(unique(ensemble))

# Afficher le résultat
print(paste("La cardinalité de l'ensemble avec des éléments uniques est:", cardinalite_unique))

# Exemple 3
# Définir un ensemble vide
ensemble_vide <- c()

# Calculer la cardinalité de l'ensemble vide
cardinalite_vide <- length(ensemble_vide)

# Afficher le résultat
print(paste("La cardinalité de l'ensemble vide est:", cardinalite_vide))