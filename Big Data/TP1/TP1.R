# https://github.com/BreizhHardware/TD-R/tree/main/Big%20Data/TP1

v1 <- c(175, 182, 165, 187, 158)
v2 <- c(19, 18, 21, 22, 20)
tableau <- data.frame(taille=v1,age=v2)
names(tableau)
print(tableau$taille)
summary(tableau)
write.table(tableau, "sortie.csv", sep=";")

options(repos = c(CRAN = "https://cran.irsn.fr/"))
# install.packages("maps")
library(maps)
map("france")

datas <- rnorm(20)
barplot(datas)
hist(datas, nclass=4)
plot(seq(0,2*pi,by=0.01), sin(seq(0,2*pi,by=0.01)))
