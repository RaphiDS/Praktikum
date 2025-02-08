# Load required libraries
library(tidyverse)
library(usmap)
library(usdata)

########################################################################################################################
# Load pre-filtered data and prepare the main dataset

load("data_edit/filtered_data.Rdata")  # Load the Rdata file

# Create a dataset with adjusted factor levels for the 'NEWRACE2' variable
drug_data <- allfilterdata %>%
  mutate(NEWRACE2 = factor(NEWRACE2, levels = c(1, 7, 2, 5, 6, 3, 4)))

# Subset the data for the year 2019
data_2019 <- drug_data %>%
  filter(year == 2019)

########################################################################################################################
# Define and set a custom ggplot theme

theme_custom <- theme_light() +
  theme(
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20),
    axis.title.x = element_text(margin = margin(t = 20)),
    axis.title.y = element_text(margin = margin(r = 20)),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.position = "bottom"
  )

theme_set(theme_custom)  # Set the custom theme as default for all plots

# A wrapper function to add common plot components (percent y-axis and default labels) to a ggplot object
my_plot <- function(plot_obj) {
  plot_obj +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(y = "Prozent", title = "")
}

########################################################################################################################
# Define label mappings for demographic variables

# Mapping for race codes to labels (labels remain in German)
new_race2_vector <- c(
  "1" = "Weiße",
  "7" = "Hispanische",
  "2" = "Schwarze\nAfroamerikaner",
  "5" = "Asiaten",
  "6" = "Gemischte",
  "3" = "Am/Ak\nIndigene",
  "4" = "Indigene Hawaii\n/Paz. Inseln"
)

# Mapping for age groups (codes to labels)
age_group_vector <- c(
  "1" = "12-17",
  "2" = "18-25",
  "3" = "26+"
)

# Mapping for gender codes to labels
ir_sex_vector <- c(
  "1" = "Männer",
  "2" = "Frauen"
)

# Define color palettes, labels, and shape vectors for drug-related plots
drug_colors <- c("#0072B2", "#009E73", "#E69F00", "#be548e")
labels_drugs <- c("Alkohol", "Zigarette", "Kokain", "Heroin")
shapes_drugs <- c(15:18)

# Define color palettes, labels, and shape vectors for tobacco-related plots
tobacco_colors <- c("#009E73", "darkorchid4", "lightblue", "darkgrey")
labels_tobacco <- c("Zigarette", "Zigarre", "Rauchfreier Tabak", "Pfeife")
shapes_tobacco <- c(16, 15, 17, 18)

# Define color palettes for drug dependency plots

drug_dep_color <- c(
  "Alkohol" = "#0072B2",
  "Kokain" = "#E69F00",
  "Heroin" = "#CC79A7",
  "Mehrfachabhängigkeit" = "grey20"
)

drug_dep_colors_5 <- c(
  "Keine Abhängigkeit" = "gray60",
  "Alkohol" = "#0072B2",
  "Kokain" = "#E69F00",
  "Heroin" = "#CC79A7",
  "Mehrfache Abhängigkeit" = "grey20"
)