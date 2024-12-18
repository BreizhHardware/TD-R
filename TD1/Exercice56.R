# A partir de 7heures du matin, les bus passent toutes les quinze minutes à un arrêt
# précis. Un usager se présente à cet arrêt entre 7h et 7h30. On fait l’hypothèse que l’heure exacte de
# son arrivée, représentée par le nombre de minutes après 7h, est une variable aléatoire uniformément
# répartie sur l’intervalle [0,30]. Quelle est la probabilité que l’usager attende moins de cinq minutes le
# prochain bus ? Qu’il l’attende plus de dix minutes ?

# Omega = [0;30]
# X(Omega) = [0;15]
temps_total <- 30
# Proba n'importe quel minute
p_min <- 1 / temps_total
# Calcul de P(10 <= X <= 15)
p10_X_15 <- (15-10) * p_min
# Calcul de P(25 <= X <= 30)
p25_X_30 <- (30-25) * p_min
# Calcul p_moins_5_min
p_moins_5_min <- p10_X_15 + p25_X_30
print(p_moins_5_min)

# Calcul de P(0 < X < 5)
p0_X_5 <- (5-(0+1)) * p_min
# Calcul de P(15 < X < 20)
p15_X_20 <- (20-(15+1)) * p_min
# Calcul p_plus_de_10_min
p_plus_de_10_min <- p0_X_5 + p15_X_10
print(p_plus_de_10_min)
