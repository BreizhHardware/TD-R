# Une course oppose 20 concurrents, dont Émile
# a) Combien y-a-t-il de podiums possibles ?
podiums_total <- factorial(20) / factorial(20 - 3)
podiums_total

# b) Combien y-a-t-il de podiums possibles où Émile est premier ?
emile_premier <- 1 * (factorial(19) / factorial(17))
emile_premier

# c) Combien y-a-t-il de podiums possibles dont Émile fait partie ?
emile_podium <- 3 * (factorial(19) / factorial(17))
emile_podium

# d) On souhaite récompenser les 3 premiers en leur offrant un prix identique à chacun. Combien
# y-a-t-il de distributions de récompenses possibles ?
distributions <- choose(20, 3)
distributions
