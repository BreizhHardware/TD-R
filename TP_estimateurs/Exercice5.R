# https://github.com/BreizhHardware/TD-R/tree/main/TP_estimateurs

# 1. En moyenne, quel sera la note de l'étudiant ?

# Paramètres
n_questions <- 100
n_choices <- 5
points_correct <- 3
points_wrong <- -1
points_blank <- 0

# Probabilités (l'étudiant répond toujours)
p_correct <- 1/n_choices     # 1/5 pour chaque question
p_wrong <- 1 - p_correct    # 4/5 pour chaque question

# Calcul de l'espérance pour une question
E_question <- points_correct * p_correct + points_wrong * p_wrong

# Calcul de l'espérance totale
E_total <- n_questions * E_question

# Affichage
cat("En moyenne, l'étudiant obtiendra", E_total, "points\n")

# 2. Par quelle loi peut-on approcher la note finale de l’étudiant ?

sigma2 <- n_questions * p_correct * (1 - p_correct) * (points_correct - points_wrong)^2

# Affichage des paramètres de la distribution normale
cat("La note finale peut être approchée par une distribution normale de moyenne", E_total, "et de variance", sigma2, "\n")

# 3. En uƟlisant la quesƟon précédente, donnez une approximaƟon de la probabilité que
# l'étudiant aƩeigne la moyenne.

# Paramètres de la loi normale
sigma <- sqrt(sigma2)  # écart-type
seuil <- 10       # moyenne à atteindre

# Calcul de P(X ≥ 10) où X suit N(mu, sigma²)
proba <- 1 - pnorm(seuil, mean = E_total, sd = sigma)

# Affichage
cat("Probabilité d'atteindre la moyenne :", round(proba, 4), "\n")

# 4. De même, donnez une esƟmaƟon de la probabilité que l'étudiant dépasse 0. Comparez les
# approximaƟons précédentes avec les résultats exacts.

# Paramètres
sigma <- sqrt(sigma2)  # écart-type calculé dans le code
seuil_zero <- 0  # nouveau seuil

# Calcul de P(X > 0)
proba_pos <- 1 - pnorm(seuil_zero, mean = E_total, sd = sigma)

# Affichage
cat("Probabilité d'avoir une note positive :", round(proba_pos, 4))