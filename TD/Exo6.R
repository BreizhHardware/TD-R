# Utiliser R pour donner les valeurs numériques attendues :
# Combien y-a-t-il de carrés (4 cartes de même valeur) dans un jeu de 32 cartes ?
nb_carres <- 8
nb_carres

# Combien d’anagrammes peut-on faire avec le mot "dinosaure" ?
anagrammes <- factorial(9) / factorial(2)
anagrammes

# Combien de chances a-t-on de gagner le super jackpot à l’euromillion ? (donc d’avoir 5
# bons numéros parmi 49, et 2 bons numéros étoilés parmi 10)
chances <- choose(49, 5) * choose(10, 2)
chances

# Chaque pièce d’un nouveau jeu de domino est de la forme : a b avec (a, b) ∈ {0, . . . , 9}2
# en sachant qu’un domino reste le même si on le tourne à 180 degrés (par exemple, 8 = 8
# est un, et un seul domino). Déterminer le nombre de pièces différentes que contient un
# jeu complet de dominos
nb_dominos <- choose(10, 2) + 10
nb_dominos