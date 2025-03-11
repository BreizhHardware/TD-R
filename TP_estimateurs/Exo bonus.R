# https://github.com/BreizhHardware/TD-R/tree/main/TP_estimateurs

for1 <- sqrt((((88/600) * (600 - 88)) / 600) / 600)
i1 <- 88 / 600 + 1.96 * for1
s1 <- 88 / 600 - 1.96 * for1

for2 <- sqrt(((109)/600 * (600 - 109) / 600) / 600)
i2 <- 109 / 600 + 1.96 * for2
s2 <- 109 / 600 - 1.96 * for2

for3 <- sqrt(((107)/600 * (600 - 107) / 600) / 600)
i3 <- 107 / 600 + 1.96 * for3
s3 <- 107 / 600 - 1.96 * for3

for4 <- sqrt(((94)/600 * (600 - 94) / 600) / 600)
i4 <- 94 / 600 + 1.96 * for4
s4 <- 94 / 600 - 1.96 * for4

for5 <- sqrt(((105)/600 * (600 - 105) / 600) / 600)
i5 <- 105 / 600 + 1.96 * for5
s5 <- 105 / 600 - 1.96 * for5

for6 <- sqrt(((97)/600 * (600 - 97) / 600) / 600)
i6 <- 97 / 600 + 1.96 * for6
s6 <- 97 / 600 - 1.96 * for6

cat("Intervalle de confiance pour 1 :", round(s2, 3), round(i1, 3), "\n")
cat("Intervalle de confiance pour 2 :", round(s2, 3), round(i2, 3), "\n")
cat("Intervalle de confiance pour 3 :", round(s3, 3), round(i3, 3), "\n")
cat("Intervalle de confiance pour 4 :", round(s4, 3), round(i4, 3), "\n")
cat("Intervalle de confiance pour 5 :", round(s5, 3), round(i5, 3), "\n")
cat("Intervalle de confiance pour 6 :", round(s6, 3), round(i6, 3), "\n")

proba1 <- 88 / 600
proba2 <- 109 / 600
proba3 <- 107 / 600
proba4 <- 94 / 600
proba5 <- 105 / 600
proba6 <- 97 / 600

cat("Probabilité pour 1 :", round(proba1, 3), "\n")
cat("Probabilité pour 2 :", round(proba2, 3), "\n")
cat("Probabilité pour 3 :", round(proba3, 3), "\n")
cat("Probabilité pour 4 :", round(proba4, 3), "\n")
cat("Probabilité pour 5 :", round(proba5, 3), "\n")
cat("Probabilité pour 6 :", round(proba6, 3), "\n")

if (proba1 < s1 || proba1 > i1) {
  cat("Le dé est truqué pour 1\n")
} else {
  cat("Le dé n'est pas truqué pour 1\n")
}

if (proba2 < s2 || proba2 > i2) {
  cat("Le dé est truqué pour 2\n")
} else {
  cat("Le dé n'est pas truqué pour 2\n")
}

if (proba3 < s3 || proba3 > i3) {
  cat("Le dé est truqué pour 3\n")
} else {
  cat("Le dé n'est pas truqué pour 3\n")
}

if (proba4 < s4 || proba4 > i4) {
  cat("Le dé est truqué pour 4\n")
} else {
  cat("Le dé n'est pas truqué pour 4\n")
}

if (proba5 < s5 || proba5 > i5) {
  cat("Le dé est truqué pour 5\n")
} else {
  cat("Le dé n'est pas truqué pour 5\n")
}

if (proba6 < s6 || proba6 > i6) {
  cat("Le dé est truqué pour 6\n")
} else {
  cat("Le dé n'est pas truqué pour 6\n")
}

if ((proba1 < s1 || proba1 > i1) ||
    (proba2 < s2 || proba2 > i2) ||
    (proba3 < s3 || proba3 > i3) ||
    (proba4 < s4 || proba4 > i4) ||
    (proba5 < s5 || proba5 > i5) ||
    (proba6 < s6 || proba6 > i6)) {
  cat("Le dé est truqué\n")
} else {
  cat("Le dé n'est pas truqué\n")
}

# Correction

x <- (88-100) ^ 2 + (109-100) ^ 2 + (107-100) ^ 2 + (94-100) ^ 2 + (105-100) ^ 2 + (97-100) ^ 2
xprim <- x / 100

xsq <- chisq.test(c(88, 109, 107, 94, 105, 97), p = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6))
print(xsq)