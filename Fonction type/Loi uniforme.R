# Exemple 1 : Calculer la probabilité d'obtenir une valeur dans un intervalle donné
# Définir les paramètres de la loi uniforme
a <- 0  # borne inférieure
b <- 100  # borne supérieure

# Calculer la probabilité d'obtenir une valeur entre 20 et 40
prob_in_interval <- punif(40, min=a, max=b) - punif(20, min=a, max=b) 
#Cette fonction punif calcule la probabilité cumulative
#(ou fonction de répartition) jusqu'à la valeur 40 pour une loi uniforme définie entre
#a (borne inférieure) et b (borne supérieure).
#En gros c'est P(X<=40) où X est la variable aléatoire

# Afficher le résultat
print(paste("La probabilité d'obtenir une valeur entre 20 et 40 est:", prob_in_interval))

# Exemple 2 : Trouver la valeur à partir d'une probabilité cumulative
# Définir les paramètres de la loi uniforme
a <- 0  # borne inférieure
b <- 100  # borne supérieure

# Probabilité cumulative cible
target_cumulative <- 0.9

# Calculer la valeur correspondante
value_at_target <- qunif(target_cumulative, min=a, max=b) #Elle sert à déterminer la valeur
#de la variable aléatoire pour laquelle la probabilité cumulative atteint la valeur
#spécifiée (target_cumulative).

# Afficher le résultat
print(paste("La valeur correspondant à une probabilité cumulative de 0.9 est:", value_at_target))

# Exemple 3 : Calculer et afficher les probabilités pour une distribution uniforme discrétisée
# Définir les paramètres de la loi uniforme discrétisée
a <- 0  # borne inférieure
b <- 100  # borne supérieure
n <- 10  # nombre de divisions de l'intervalle

# Calculer les points de la distribution discrète
discrete_points <- seq(a, b, length.out=n + 1) # Cette ligne génère une série de points équidistants
#dans l'intervalle [a,b] où a est la borne inférieure et b la supérieure 


# Calculer la densité de probabilité pour chaque point
densities <- dunif(discrete_points, min=a, max=b) #Cette ligne calcule la densité de
#probabilité de la loi uniforme pour chacun des points générés dans discrete_points.

# Afficher les densités
print("Densités de probabilité pour la distribution uniforme discrétisée:")
print(data.frame(Point=discrete_points, Densité=densities))
