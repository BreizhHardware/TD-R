# Créer un vecteur x = (x1, . . . , x11) contenant les réels compris entre 0 et 1 par pas de 0.1
x <- seq(0, 1, by=0.1)

# Afficher la longueur de x
print(length(x))

# En utilisant les opérations vectorielles, créer un vecteur y = 4x(1 − x)
y <- 4*x*(1-x)

# Tracer la courbe rejoignant les points (x1, y1), . . . , (x11, y11) avec la commande plot
plot(x, y)

# Calculer le maximum des y1, . . . , y11
print(max(y))

# En quel point le maximum est-il atteint ?
print(x[which.max(y)])

# Tracer la courbe de la fonction f (x) = 4x2(1 − x), x
# ∈ [−2, 1], en rouge
x <- seq(-2, 1, by=0.1)
y <- 4*x^2*(1-x)
plot(x, y, col="red")