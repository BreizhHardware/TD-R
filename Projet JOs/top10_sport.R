# Charger le fichier Excel
file_path <- "Projet JOs/Porjet_JO.xlsx"

# Lire la feuille "Travail_athletes"
df_athletes <- read_excel(file_path, sheet = "Travail_athletes")

# Compter le nombre de disciplines uniques par pays
disciplines_par_pays <- df_athletes %>%
  group_by(National Olympic Committee) %>%
  summarise(Nombre_Disciplines = n_distinct(Discipline)) %>%
  arrange(desc(Nombre_Disciplines))

# Renommer les colonnes
colnames(disciplines_par_pays) <- c("Pays", "Nombre_Disciplines")

# Sélectionner le top 100 pays
top_10 <- head(disciplines_par_pays, 10)

# Trouver les pays ignorés
pays_ignores <- setdiff(disciplines_par_pays$Pays, top_100$Pays)

# Afficher les pays ignorés
print("Pays ignorés :")
print(pays_ignores)

# Tracer l'histogramme
ggplot(top_10, aes(x = reorder(Pays, -Nombre_Disciplines), y = Nombre_Disciplines)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  labs(title = "Top 10 des pays avec le plus de disciplines", x = "Pays", y = "Nombre de disciplines") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))