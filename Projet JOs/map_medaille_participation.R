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
if (!require("readxl")) {
  install.packages("readxl")
}

library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
library(readxl)

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

# Obtention d'un résumé du nombre de médailles par pays
nombre_medailles <- table(donnees_medailles$Country, donnees_medailles$Medal_type)
nombre_medailles <- as.data.frame.matrix(nombre_medailles)

# Ajout d'une colonne pour le total des médailles
if(ncol(nombre_medailles) > 0) {
  nombre_medailles$Total <- rowSums(nombre_medailles)
  # Tri par nombre total de médailles (décroissant)
  nombre_medailles <- nombre_medailles[order(-nombre_medailles$Total),]
}

# Conversion de nombre_medailles en un format adapté pour ggplot2
donnees_graphique <- data.frame(
  Pays = rownames(nombre_medailles),
  Total = nombre_medailles$Total
)

#-----------------------GESTION DES CODES ISO ---------------------------
# Function to get ISO 3-letter codes from country names
get_iso_codes <- function(country_names) {
  # Load world data
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  # Create lookup table with lowercase names for better matching
  lookup <- world %>%
    select(name, admin, iso_a3, iso_a3_eh) %>%
    mutate(name_lower = tolower(name),
           admin_lower = tolower(admin))
  
  # Process each country name
  iso_codes <- character(length(country_names))
  
  for (i in seq_along(country_names)) {
    country <- country_names[i]
    
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

# Apply to medal data
donnees_graphique$iso_code <- get_iso_codes(donnees_graphique$Pays)

# Expanded custom mapping for Olympic country names to ISO codes
custom_mapping <- c(
  "United States" = "USA", "United States of America" = "USA",
  "Great Britain" = "GBR",
  "People's Republic of China" = "CHN", "China" = "CHN",
  "Chinese Taipei" = "TPE",
  "Republic of Korea" = "KOR", "Korea" = "KOR",
  "Democratic People's Republic of Korea" = "PRK", "North Korea" = "PRK",
  "ROC" = "RUS",  # Russian Olympic Committee
  "Hong Kong, China" = "HKG",
  "Iran, Islamic Republic of" = "IRN",
  "Netherlands" = "NLD",
  "New Zealand" = "NZL",
  "Republic of South Africa" = "ZAF",
  "Trinidad and Tobago" = "TTO",
  "Türkiye" = "TUR"
)

# Apply custom mappings to medal data
for (i in seq_len(nrow(donnees_graphique))) {
  country <- donnees_graphique$Pays[i]
  if (country %in% names(custom_mapping)) {
    donnees_graphique$iso_code[i] <- custom_mapping[country]
  }
}

# Check for missing mappings
missing_codes <- donnees_graphique %>%
  filter(is.na(iso_code)) %>%
  select(Pays) %>%
  distinct()

if (nrow(missing_codes) > 0) {
  print("Medal countries without ISO codes:")
  print(missing_codes)
}

#-----------------------TRAITEMENT DES PAYS PARTICIPANTS ---------------------------
# Define Olympic code to ISO mapping
olympic_to_iso <- c(
  "GER" = "DEU", "RSA" = "ZAF", "CHI" = "CHL",
  "USA" = "USA", "GBR" = "GBR", "FRA" = "FRA",
  "JPN" = "JPN", "ITA" = "ITA", "CHN" = "CHN",
  "AUS" = "AUS", "NED" = "NLD", "KOR" = "KOR",
  "ESP" = "ESP", "BRA" = "BRA", "SUI" = "CHE",
  "CAN" = "CAN", "HUN" = "HUN", "NZL" = "NZL",
  "UKR" = "UKR", "SWE" = "SWE", "TUR" = "TUR",
  "ROU" = "ROU", "POL" = "POL", "NOR" = "NOR"
  # Add more mappings as needed
)

participants_iso <- test_R$Pays_unique

# Convert participant Olympic codes to ISO codes
participants_iso_fixed <- participants_iso
for (i in 1:length(participants_iso)) {
  code <- participants_iso[i]
  if (code %in% names(olympic_to_iso)) {
    participants_iso_fixed[i] <- olympic_to_iso[code]
  }
}

#-----------------------CRÉATION DE LA CARTE ---------------------------
# Load world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# Create a three-category classification
world$status <- "Non" # Default: didn't participate

# First mark participating countries
for (i in 1:nrow(world)) {
  if (world$iso_a3[i] %in% participants_iso_fixed ||
      world$iso_a3_eh[i] %in% participants_iso_fixed) {
    world$status[i] <- "Participant"
  }
}

# Then mark medal countries (overriding participant status)
for (i in 1:nrow(world)) {
  if (world$iso_a3[i] %in% donnees_graphique$iso_code ||
      world$iso_a3_eh[i] %in% donnees_graphique$iso_code) {
    world$status[i] <- "Médaille"
  }
}

# Create the map
map_jo <- ggplot(data = world) +
  geom_sf(aes(fill = status), color = "black") +
  scale_fill_manual(values = c(
    "Participants médaillés" = "yellow",
    "Participants non médaillés" = "lightgreen",
    "Non participants" = "lightgrey"
  )) +
  theme_minimal() +
  labs(
    title = "Pays aux Jeux Olympiques de Paris 2024",
    fill = "Statut",
    caption = "Jaune: Médaillés, Vert: Participants sans médaille, Gris: Non participants"
  )

# Display the map
print(map_jo)
