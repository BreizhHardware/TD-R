# Crée des vecteurs
# y0 constitué de la suite des entiers de 0 à 10 par pas de 2
y0 <- seq(0, 10, by=2)
# y1 constitué de tous les entiers pairs entre 1 et 18
y1 <- seq(2, 18, by=2)
# y2 constitué de 20 fois de suite la valeur 4
y2 <- rep(4, 20)
# y3 constitué de 20 nombres entre 0 et 10
y3 <- runif(20, 0, 10)

# Extraire de y3
# le troisième élément
y33 <- y3[3]
# tous les éléments sauf le troisième
y34 <- y3[-3]

# Comparer les commandes suivantes
matrix(y3, nrow = 2)
matrix(y3, byrow = TRUE)

# Construire une matrice A comportant quatre lignes et trois colonnes remplies par lignes
# successives avec les éléments du vecteur 1:12
A <- matrix(1:12, nrow = 4, byrow = TRUE)

# Construire une matrice B comportant quatre lignes et trois colonnes remplies par
# colonnes successives avec les éléments du vecteur 1:12
B <- matrix(1:12, nrow = 4, byrow = FALSE)

# Extraire l’élément situé en deuxième ligne et troisième colonne de A
A[2, 3]

# Extraire la première colonne de A, puis la deuxième ligne de A.
A[, 1]
A[2, ]

# Construire une matrice C constituée des lignes 1 et 4 de A.
C <- A[c(1, 4), ]
print(A)
print(C)