# Création d'un vecteur pour stocker les 10 000 moyennes normalisées
ms=numeric(10000);
# Définition des paramètres: p=probabilité de succès, x=séquence pour le tracé de la courbe normale
p=0.75; x=seq(-4,4,0.025);
# Boucle principale qui effectue 50 itérations avec taille d'échantillon croissante
for (j in(1:50)){
  # k = taille d'échantillon (j²), donc de 1 à 2500
  k=j*j; for (i in (1:10000)){
    # Calcul des paramètres théoriques d'une proportion: moyenne et écart-type
    sig=sqrt(p*(1-p)/k); mu=p;
    # Génération de k essais de Bernoulli, calcul de leur moyenne, 
    # puis normalisation: (moyenne-mu)/sigma
    ms[i]=(mean(rbinom(k,1,p))-mu)/sig }
  # Tracé de l'histogramme des moyennes normalisées
  hist(ms, breaks=41, xlab="x-variable", xlim=c(-4,4), prob=TRUE, main=sprintf("normal curve over histogram, n = %d",k))
  # Superposition de la courbe de densité normale standard N(0,1)
  curve(dnorm(x), col="darkblue", lwd=2, add=TRUE, yaxt="n")
}


# Définition de la séquence pour le tracé des courbes
x = seq(-5, 5, 0.025)

# Boucle principale qui effectue 50 itérations avec taille d'échantillon croissante
for (j in (1:50)) {
  # k = taille d'échantillon (j²), donc de 1 à 2500
  k = j*j
  for (i in (1:10000)) {
    # Génération de k variables Cauchy et calcul de leur moyenne
    ms[i] = mean(rcauchy(k))
  }
  # Pour éviter que les valeurs extrêmes ne déforment l'histogramme
  valid_range = ms > -5 & ms < 5
  # Tracé de l'histogramme des moyennes
  hist(ms[valid_range], breaks=41, xlab="x-variable", xlim=c(-5, 5),
       prob=TRUE, main=sprintf("Distribution des moyennes de Cauchy, n = %d", k))
  # Superposition de la courbe de densité normale standard N(0,1)
  curve(dnorm(x), col="darkblue", lwd=2, add=TRUE, yaxt="n")
  # Superposition de la courbe de densité Cauchy standard
  curve(dcauchy(x), col="darkred", lwd=2, add=TRUE, yaxt="n")
  # Légende
  legend("topright", legend=c("Normale N(0,1)", "Cauchy"),
         col=c("darkblue", "darkred"), lwd=2)
}