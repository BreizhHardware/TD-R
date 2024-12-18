x = c(1, 3, 5, 7, 9)
y = c(2, 3, 5, 7, 11, 13)
z = c(9, 3, 2, 5, 9, 2, 3, 9, 1)

x + 2
y * 3
length(x)
x + y
sum(x > 5)
sum(x[x > 5])
sum(x > 5 | x < 3)
y[3]
y[-3]
y[x]
(y > 7)
y[y > 7]
sort(z)
sort(z, dec = TRUE)
rev(z)
order(z)
unique(z)
duplicated(z)
table(z)
rep(z, 3)