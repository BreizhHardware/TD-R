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
  x * c * (1 - x)^4
}

esperance <- integrate(fonction_esperance, lower = 0, upper = 1)$value
print(esperance)