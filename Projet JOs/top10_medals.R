# Installation et chargement des packages nécessaires
if (!require("readxl")) {
  install.packages("readxl")
}
if (!require("ggplot2")) {
  install.packages("ggplot2")
}
if (!require("tidyr")) {
  install.packages("tidyr")
}
library(ggplot2)
library(readxl)
library(tidyr)

# Importation des données depuis le fichier Excel
donnees_medailles <- read_excel("Projet JOs/Porjet_JO.xlsx",
                                sheet = "Travail_medailles")

# Visualisation de la structure des données
str(donnees_medailles)

# Nettoyage des données
# Conversion des dates au format approprié (supposant le format MM/JJ/AAAA)
donnees_medailles$Medal_date <- as.Date(donnees_medailles$Medal_date, format = "%m/%d/%Y")

# Correction de la colonne Age - remplacer les erreurs et convertir en numérique
donnees_medailles$Age <- ifelse(donnees_medailles$Age == "#NOM?" | donnees_medailles$Age == "01/02/1900", NA, donnees_medailles$Age)
donnees_medailles$Age <- as.numeric(donnees_medailles$Age)

# Affichage des premières lignes pour vérifier les données
head(donnees_medailles)

# Obtention d'un résumé du nombre de médailles par pays
nombre_medailles <- table(donnees_medailles$Country, donnees_medailles$Medal_type)
nombre_medailles <- as.data.frame.matrix(nombre_medailles)

# Ajout d'une colonne pour le total des médailles
if(ncol(nombre_medailles) > 0) {
  nombre_medailles$Total <- rowSums(nombre_medailles)
  # Tri par nombre total de médailles (décroissant)
  nombre_medailles <- nombre_medailles[order(-nombre_medailles$Total),]
}

# Affichage du tableau des médailles
print(nombre_medailles)

# Conversion de nombre_medailles en un format adapté pour ggplot2
donnees_graphique <- data.frame(
  Pays = rownames(nombre_medailles),
  Total = nombre_medailles$Total
)

# Graphique des 10 premiers pays
graphique_top10 <- ggplot(head(donnees_graphique, 10), aes(x = reorder(Pays, Total), y = Total)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Nombre total de medailles par pays - Top 10",
       x = "",
       y = "Nombre total de medailles") +
  theme_minimal()

# Affichage du graphique top 10
print(graphique_top10)

# Sauvegarde du graphique comme fichier image
ggsave("Projet JOs/top10_medals.png", plot = graphique_top10, width = 8, height = 6, units = "in", dpi = 300)

# Obtention des 10 premiers pays par total de médailles
pays_top10 <- head(rownames(nombre_medailles), 10)
donnees_top10 <- nombre_medailles[pays_top10, ]

# Obtention des noms des colonnes de médailles
cols_medailles <- setdiff(colnames(donnees_top10), "Total")

# Vérifiez d'abord l'ordre réel des colonnes de médailles
print("Colonnes de médailles trouvées:")
print(cols_medailles)

# Assurer la correspondance correcte entre les types de médailles et les couleurs
# Création d'un dataframe vide
donnees_graphique <- data.frame()

# Trouvez les colonnes de médailles indépendamment de leur format exact
col_or <- grep("or|gold", cols_medailles, ignore.case = TRUE, value = TRUE)[1]
col_argent <- grep("argent|silver", cols_medailles, ignore.case = TRUE, value = TRUE)[1]
col_bronze <- grep("bronze", cols_medailles, ignore.case = TRUE, value = TRUE)[1]

types_medailles <- c(col_or, col_argent, col_bronze)
noms_affichage <- c("Or", "Argent", "Bronze")

cat("Utilisation des colonnes:", paste(types_medailles, collapse=", "), "\n")

# Traitement de chaque pays
for (i in seq_along(pays_top10)) {
  pays <- pays_top10[i]
  donnees_pays <- data.frame(
    Pays = rep(pays, length(types_medailles)),
    Type_Medaille = noms_affichage,  # Utiliser les noms standardisés
    Nombre = as.numeric(donnees_top10[i, types_medailles]),
    Total = rep(donnees_top10[i, "Total"], length(types_medailles))
  )
  donnees_graphique <- rbind(donnees_graphique, donnees_pays)
}

# Classement des pays par total de médailles (décroissant)
donnees_graphique$Pays <- factor(donnees_graphique$Pays,
                                 levels = rev(pays_top10))

# Création d'un graphique à barres empilées horizontal avec les bonnes couleurs
graphique_type_medailles <- ggplot(donnees_graphique, aes(x = Pays, y = Nombre, fill = Type_Medaille)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +
  labs(title = "Top 10 des pays par type de medailles",
       x = "",
       y = "Nombre de medailles") +
  scale_fill_manual(values = c("Or" = "#FFD700", "Argent" = "#C0C0C0", "Bronze" = "#CD7F32")) +
  theme_minimal() +
  guides(fill = guide_legend(title = "Type de medaille"))

# Affichage du graphique
print(graphique_type_medailles)

# Sauvegarde du graphique comme fichier image
ggsave("Projet JOs/top10_medals_by_type.png", plot = graphique_type_medailles, width = 8, height = 6, units = "in", dpi = 300)

# Calcul de la moyenne, médiane, variance et écart type pour les 10 premiers pays
moyenne_medailles <- mean(donnees_top10$Total)
mediane_medailles <- median(donnees_top10$Total)
variance_medailles <- var(donnees_top10$Total)
ecart_type_medailles <- sd(donnees_top10$Total)

# Affichage des résultats
cat("Statistiques des 10 premiers pays par nombre total de médailles:\n")
cat("Moyenne: ", moyenne_medailles, "\n")
cat("Médiane: ", mediane_medailles, "\n")
cat("Variance: ", variance_medailles, "\n")
cat("Écart type: ", ecart_type_medailles, "\n")

cat("10 premiers pays par nombre total de médailles:", donnees_top10$Total, "\n")