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


# Create a mapping between Olympic codes and ISO codes
olympic_to_iso <- c(
  "GER" = "DEU"  # Germany
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
  mutate(participant = ifelse(iso_a3 %in% participants_iso_fixed |
                                iso_a3_eh %in% participants_iso_fixed, "Oui", "Non"))

# Create the map
ggplot(data = world) +
  geom_sf(aes(fill = participant), color = "black") +
  scale_fill_manual(values = c("Oui" = "lightgreen", "Non" = "lightgrey")) +
  theme_minimal() +
  labs(title = "Pays participants aux Jeux Olympiques de Paris 2024",
       fill = "Participation")