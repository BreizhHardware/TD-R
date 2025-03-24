# Installation et chargement des packages nécessaires
if (!require("ggplot2")) {
  install.packages("ggplot2")
}
if (!require("rnaturalearth")) {
  install.packages("rnaturalearth")
}

if (!require("rnaturalearthdata")) {
  install.packages("rnaturalearthdata")
}
if (!require("dplyr")) {
  install.packages("dplyr")
}

library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
#-----------------------RECUPERATION PAYS MEDAILLES ---------------------------


# Importation des données depuis le fichier Excel
donnees_medailles <- Porjet_JO

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

# Function to get ISO 3-letter codes from country names
get_iso_codes <- function(country_names) {
  # Load world data
  world <- ne_countries(scale = "medium", returnclass = "sf")

  # Create lookup table with lowercase names for better matching
  lookup <- world %>%
    select(name, admin, iso_a3, iso_a3_eh) %>%
    mutate(name_lower = tolower(name),
           admin_lower = tolower(admin))

  # Custom mapping for Olympic country names that don't match standard names
  custom_mapping <- c(
    "United States of America" = "USA",
    "Great Britain" = "GBR",
    "People's Republic of China" = "CHN",
    "Chinese Taipei" = "TPE",
    "Republic of Korea" = "KOR",
    "Democratic People's Republic of Korea" = "PRK"
    # Add more mappings as needed
  )

  # Process each country name
  iso_codes <- character(length(country_names))

  for (i in seq_along(country_names)) {
    country <- country_names[i]

    # First check custom mapping
    if (country %in% names(custom_mapping)) {
      iso_codes[i] <- custom_mapping[country]
      next
    }

    # Try matching with rnaturalearth data
    country_lower <- tolower(country)
    match_row <- lookup %>%
      filter(name_lower == country_lower | admin_lower == country_lower)

    if (nrow(match_row) > 0) {
      # Use first available ISO code
      if (!is.na(match_row$iso_a3[1]) && match_row$iso_a3[1] != "" && match_row$iso_a3[1] != "-99") {
        iso_codes[i] <- match_row$iso_a3[1]
      } else if (!is.na(match_row$iso_a3_eh[1]) && match_row$iso_a3_eh[1] != "") {
        iso_codes[i] <- match_row$iso_a3_eh[1]
      } else {
        iso_codes[i] <- NA_character_
      }
    } else {
      iso_codes[i] <- NA_character_
    }
  }

  return(iso_codes)
}

# Apply to your data
donnees_graphique$iso_code <- get_iso_codes(donnees_graphique$Pays)

custom_mapping <- c(
  "United States" = "USA",
  "Great Britain" = "GBR",
  "People's Republic of China" = "CHN",
  "China" = "CHN",
  "Chinese Taipei" = "TPE",
  "Republic of Korea" = "KOR",
  "Korea" = "KOR",
  "Democratic People's Republic of Korea" = "PRK",
  "North Korea" = "PRK",
  "ROC" = "RUS",  # Russian Olympic Committee
  "Hong Kong, China" = "HKG",
  "Iran, Islamic Republic of" = "IRN",
  "Netherlands" = "NLD",
  "New Zealand" = "NZL",
  "Republic of South Africa" = "ZAF",
  "Trinidad and Tobago" = "TTO",
  "Türkiye" = "TUR"
)

# Apply custom mappings directly
for (i in seq_len(nrow(donnees_graphique))) {
  country <- donnees_graphique$Pays[i]
  if (country %in% names(custom_mapping)) {
    donnees_graphique$iso_code[i] <- custom_mapping[country]
  }
}

# Check results
print(head(donnees_graphique[, c("Pays", "iso_code")]))

# Check for missing mappings
missing_codes <- donnees_graphique %>%
  filter(is.na(iso_code)) %>%
  select(Pays) %>%
  distinct()

if (nrow(missing_codes) > 0) {
  print("Countries still without ISO codes:")
  print(missing_codes)
}

#-----------------------RECUPERATION PAYS MEDAILLES ---------------------------


# Create a mapping between Olympic codes and ISO codes
olympic_to_iso <- c(
  "GER" = "DEU", "RSA" = "ZAF", "CHI" = "CHL"  # Germany
  # Add other mappings here if needed
  # For example: "USA" = "USA", "GBR" = "GBR", etc.
)

participants_iso <- test_R$Pays_unique

# Convert Olympic codes to ISO codes where needed
participants_iso_fixed <- participants_iso
for (i in 1:length(participants_iso)) {
  code <- participants_iso[i]
  if (code %in% names(olympic_to_iso)) {
    participants_iso_fixed[i] <- olympic_to_iso[code]
  }
}

# Load world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# Check Germany's data
germany_data <- world %>% filter(admin == "Germany")
print("Germany data:")
print(germany_data %>% select(admin, iso_a3, iso_a3_eh))

# Mark participating countries
world <- world %>%
  mutate(participant = ifelse(iso_a3 %in% donnees_graphique$iso_code |
                                iso_a3_eh %in% donnees_graphique$iso_code, "Oui", "Non"))

# Create the map
ggplot(data = world) +
  geom_sf(aes(fill = participant), color = "black") +
  scale_fill_manual(values = c("Oui" = "yellow", "Non" = "lightgrey")) +
  theme_minimal() +
  labs(title = "Pays ayant reçu au moins Une médaille aux Jeux Olympiques de Paris 2024",
       fill = "Médailles")