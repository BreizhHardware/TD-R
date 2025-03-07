# https://github.com/BreizhHardware/TD-R/tree/main/TP_estimateurs

# 1. Rappeler pourquoi l'hypothèse de normalité s'applique très souvent sur de l'incerƟtude.

cat("L'hypothèse de normalité pour les erreurs de mesure est justifiée par :\n\n",
    "1. Le Théorème Central Limite :\n",
    "   Les erreurs sont la somme de nombreuses petites perturbations indépendantes.\n",
    "   La somme de variables aléatoires indépendantes tend vers une loi normale.\n\n",
    "2. Les propriétés naturelles des erreurs de mesure :\n",
    "   - Symétrie : erreurs positives et négatives équiprobables\n",
    "   - Concentration : petites erreurs plus fréquentes\n",
    "   - Rareté des extrêmes : grandes erreurs peu probables\n")

# 2. Donnez un encadrement de l'intervalle [a,b] pour que la probabilité que X soit compris entre
# a et b égal 95%. Cet intervalle est-il unique ?

# Paramètres de la loi normale
mu <- 2
sigma <- 0.25
conf_level <- 0.95

# Valeur critique pour 95% (1.96 pour loi normale standard)
z <- qnorm((1 + conf_level)/2)

# Calcul des bornes de l'intervalle
a <- mu - z * sigma
b <- mu + z * sigma

cat("Intervalle de confiance à 95% : [", round(a, 3), ",", round(b, 3), "]\n")
cat("La probabilité P(", round(a, 3), "< X <", round(b, 3), ") = 0.95\n\n")
cat("Note : Cet intervalle n'est pas unique.\n",
    "On peut avoir une infinité d'intervalles [a,b] différents\n",
    "tant que P(a < X < b) = 0.95\n")

# 3. À parƟr de quelle valeur de c a-t-on P c ( ) 0, 01    ?

# Paramètres
alpha <- 0.01  # probabilité recherchée

# Calcul de c en utilisant la symétrie de la loi normale
# P(|X| > c) = 0.01 équivaut à P(X < -c) + P(X > c) = 0.01
c <- mu + qnorm(1 - alpha/2) * sigma

# Affichage
cat("La valeur de c est :", round(c, 3), "\n")
cat("Vérification : P(|X| >", round(c, 3), ") =",
    round(pnorm(-c, mu, sigma) + (1 - pnorm(c, mu, sigma)), 4))