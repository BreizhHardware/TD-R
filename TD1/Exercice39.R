# Question a
# Ici on cherche P(X <= 30) via une loi binomial avec 70 et 0.4
sum(dbinom(x=0:30,size=70,prob=0.4))

# Question b
# Ici on cherche P(X) >= 0.9 via la même loi binomial qu'avant
i <- 0
j <- 30
while (i < 0.9) {
  i <- sum(dbinom(x=0:j, size=70, prob=0.4))
  j <- j + 1
}
print(i)
print(j - 1)

# Bonus
print(dbinom(x=0:70, size=70, prob=0.4))
