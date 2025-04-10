print("DS Félix MARQUET début (DS_Felix_MARQUET.R)")
# Exercice 1
print("Début exercice 1")
# 5/
print("Question 5:")
sum(dbinom(x=0,size=3600,prob=0.166)) # Ici je fais la loi binomial pour X=0, 3600 lancé et une probabilité de 1/6
# 6/
print("Question 6:")
i <- 0 # On défini i à 0 pour avoir une probabilité null au début
x <- 480 # On défini j à 480 car on veux x superieur a 480 sinon ça n'a pas de sense
while (i < 0.96) { # On boucle tant que la probabilité est inferieur a 96%
  i <- sum(dbinom(x=480:x, size=3600, prob=0.166)) # On calcule la probabilité
  x <- x + 1 # On augmente x de 1
}
print(x - 1)
print("Fin exercice 1")
# Exercice 2
print("Début exercice 2")
# Définition des fonction f(t), g(t) et h(t) dans [0 ; 2] (impossible de mettre dans if dans une function(t)
f <- function(t) {
  (3 / 4 * t) * (2 - t) # On défini la fonction f
}

g <- function(t) {
  1 / (sqrt(2 * 3.14 * (1 / 2)^2)) * exp(-(t - 1)^2 / 2 * (1 / 2)^2) # On défini la fonction g
}

h <- function(t) {
  (1/2) + (t*0) # Ici t*0 car sinon le compilateur faire un erreur car une fonction en fonction t ne possède pas de t
}

# 3/
print("Question 3:")
maxXf <- 0
maxXf_t <- 0
maxXg <- 0
maxXg_t <- 0
maxXh <- 0
maxXh_t <- 0
t <- 0
while (t < 2) { # Tant que t < 2 on continue la boucle
  Xf_t <- (3 / 4 * t) * (2 - t) # On calcul Xf(t)
  Xg_t <- 1 / (sqrt(2 * 3.14 * (1 / 2)^2)) * exp(-(t - 1)^2 / 2 * (1 / 2)^2) # On calcul Xg(t)
  Xh_t <- 1/2 # On calcul Xh(t)
  if (Xf_t > maxXf) { # Si Xf(t) est superieur au Xf maximum trouvé jusqu'a présent on le remplace par Xf(t)
    maxXf <- Xf_t
    maxXf_t <- t
  }
  if (Xg_t > maxXg) { # Si Xg(t) est superieur au Xg maximum trouvé jusqu'a présent on le remplace par Xg(t)
    maxXg <- Xg_t
    maxXg_t <- t
  }
  if (Xh_t > maxXh) { # Si Xh(t) est superieur au Xh maximum trouvé jusqu'a présent on le remplace par Xh(t)
    maxXh <- Xh_t
    maxXh_t <- t
  }
  t <- t + 0.01 # On augmente t de 0.01 pour avoir des mesures précises
}
# On affiche les résultats obtenu
print("maxXf :")
print(maxXf)
print("maxXf_t :")
print(maxXf_t)
print("maxXg :")
print(maxXg)
print("maxXg_t :")
print(maxXg_t)
print("maxXh :")
print(maxXh)
print("maxXh_t :")
print(maxXh_t)

# 4/
print("Question 4:")
result_q4 <- integrate(f, lower = 0, upper = 2) # Calcul de l'intégrale de f entre 0 et 2
print(result_q4)

# 5/
print("Question 5:")
he <- function(t) {
    t * (1/2) # On défini t*h(t) pour pouvoir calculer l'espérence de h(t)
}
result_q5 <- integrate(he, lower = 0, upper = 2) # On se place entre 0 et 2 car le reste vaut 0
print(result_q5)

# 6/
print("Question 6:")
f_entre_0.5_0.75 <- integrate(f, lower = 0.5, upper = 0.75) # Calcul de l'intégrale de f entre 0.5 (00h30) et 0.75 (00h45)
g_entre_0.5_0.75 <- integrate(g, lower = 0.5, upper = 0.75)
h_entre_0.5_0.75 <- integrate(h, lower = 0.5, upper = 0.75)
print("f entre 0.5 et 0.75 :")
print(f_entre_0.5_0.75)
print("g entre 0.5 et 0.75 :")
print(g_entre_0.5_0.75)
print("h entre 0.5 et 0.75 :")
print(h_entre_0.5_0.75)
print("Fin exercice 2")
print("DS Félix MARQUET fin (DS_Felix_MARQUET.R)")
