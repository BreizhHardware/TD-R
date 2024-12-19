# Exemple 1
# Définir la fonction à intégrer
f <- function(x) {
  return(x^2)
}

# Définir les bornes de l'intégration
a <- 0
b <- 1

# Définir le nombre de rectangles
n <- 1000

# Calculer la largeur de chaque rectangle
dx <- (b - a) / n

# Calculer les abscisses des points intermédiaires
x_vals <- seq(a, b, length.out = n+1)

# Calculer les ordonnées des points intermédiaires
y_vals <- f(x_vals)

# Calculer l'intégrale en utilisant la méthode des rectangles
integrale_rect <- sum(y_vals[-(n+1)] * dx)

# Afficher le résultat
print(paste("L'intégrale de x^2 sur [0, 1] est environ:", integrale_rect))

# Exemple 2
# Définir la fonction à intégrer
f <- function(x) {
  return(exp(x))
}

# Définir les bornes de l'intégration
a <- 0
b <- 1

# Définir le nombre de subdivisions (doit être pair)
n <- 1000

# Calculer la largeur de chaque subdivision
h <- (b - a) / n

# Calculer les abscisses des points intermédiaires
x_vals <- seq(a, b, length.out = n+1)

# Calculer les ordonnées des points intermédiaires
y_vals <- f(x_vals)

# Calculer l'intégrale en utilisant la méthode de Simpson
integrale_simpson <- (h/3) * (y_vals[1] + y_vals[n+1] + 4 * sum(y_vals[seq(2, n, by=2)]) + 2 * sum(y_vals[seq(3, n-1, by=2)]))

# Afficher le résultat
print(paste("L'intégrale de e^x sur [0, 1] est environ:", integrale_simpson))

# Exemple 3
# Définir la fonction à intégrer
f <- function(x) {
  return(sin(x))
}

# Définir les bornes de l'intégration
a <- 0
b <- pi

# Calculer l'intégrale en utilisant la fonction integrate
result <- integrate(f, a, b)

# Afficher le résultat
print(paste("L'intégrale de sin(x) sur [0, pi] est environ:", result$value))