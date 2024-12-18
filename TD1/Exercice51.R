# Dans une station-service, la demande hebdomadaire en essence, en milliers de litres, est
# une variable aléatoire X de densité f (x) = c(1 − x)^4 1[0,1].
# a) Déterminer c
f <- function(x) {(1-x)^4}
result <- integrate(f, lower = 0, upper = 1)
# Je détermine C car intérale de c*f(x) = 1
c <- 1 / result$value
print(c)
# b) La station est réapprovisionnée chaque lundi à 20h. Quelle doit être la capacité du réservoir
# d’essence pour que la probabilité d’épuiser ce réservoir soit inférieure à 10^−5 ?
F <- function(x) {
  integrate(function(t) c * (1 - t)^4, lower = 0, upper = x)$value
}
cible <- 1 - 10^(-5)
capacite <- uniroot(function(x) F(x) - cible, lower = 0, upper = 1)$root
print(capacite * 1000)
# c) (Bonus) Calculer l'espérence
fonction_esperance <- function (x) {
  x * c * f(x)
}

esperance <- integrate(fonction_esperance, lower = 0, upper = 1)$value
print(esperance)

# d) (Bonus) Faire avec la méthode des réctangle et tracer le graphique de la construction
n <- 15
dx <- 1 / n
x_vals <- seq(0, 1, length.out = n)
y_vals <- f(x_vals)
integrale_rect <- sum(y_vals * dx)

print(integrale_rect)
plot(x_vals, y_vals, type = "list", col = "blue", lwd = 2,
     main = "Methode des rectangles pour l'integrale",
     xlab = "x", ylab = "f(x)")
rect(x_vals[-n], 0, x_vals[-1], y_vals[-n], col = rgb(0, 0, 1, 0.2), border = NA)