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

########################################################################################################################
# Function to generate demographic distribution plots for a given variable
demo_distribution_fun <- function(demo_var, label_vec, x_label) {
  my_plot(
    data_2019 %>%
      select(all_of(demo_var)) %>%                        # Select the demographic variable
      group_by(.data[[demo_var]]) %>%                     # Group by the variable's values
      summarise(count = n()) %>%                          # Count occurrences
      mutate(count = count / sum(count)) %>%              # Convert counts to proportions
      ggplot(aes(x = factor(.data[[demo_var]]), y = count)) +
      geom_col() +
      scale_x_discrete(labels = label_vec) +             # Apply custom labels
      labs(x = x_label)                                  # Set x-axis label
  )
}

# Create distribution plots for race, age, and gender
race_distribution <- demo_distribution_fun("NEWRACE2", new_race2_vector, "Race")
age_distribution <- demo_distribution_fun("CATAG2", age_group_vector, "Altersgruppen")
gender_distribution <- demo_distribution_fun("irsex", ir_sex_vector, "Geschlecht")

########################################################################################################################
# Functions to calculate drug use percentages

# Function to compute ever-use percentages for a given drug variable
ever_data_fun <- function(data_col, drug) {
  drug_data %>%
    group_by(year, .data[[data_col]]) %>%              # Group by year and drug variable
    summarise(count = n()) %>%                          # Count occurrences
    mutate(
      count = count / sum(count),                      # Calculate proportion
      drug = drug                                    # Add drug label
    ) %>%
    filter(.data[[data_col]] == 1)                      # Keep only records indicating use
}

# Function to compute percentages for 30-day use, aggregating usage counts
data_fun_30 <- function(data_col, drug) {
  drug_data %>%
    group_by(year, .data[[data_col]]) %>%              # Group by year and drug variable
    summarise(count = n()) %>%                          # Count occurrences
    mutate(
      count = count / sum(count),                      # Convert to proportion
      drug = drug                                    # Add drug label
    ) %>%
    filter(.data[[data_col]] > 0 & .data[[data_col]] < 31) %>%  # Filter usage within 1 to 30 days
    ungroup() %>%
    group_by(year) %>%                                # Group by year for aggregation
    mutate(count = sum(count)) %>%                    # Sum the counts for each year
    filter(.data[[data_col]] == 1)                    # Keep only records indicating minimal usage
}

# Combine ever-use data for four drugs into one data frame
four_drugs_ever <- as.data.frame(
  rbind(
    ever_data_fun("alcever", "Alkohol"),
    ever_data_fun("cigever", "Zigarette"),
    ever_data_fun("cocever", "Kokain"),
    ever_data_fun("herever", "Heroin")
  )
)

# Combine ever-use data for tobacco products into one data frame
tobacco_ever <- as.data.frame(
  rbind(
    ever_data_fun("cigever", "Zigarette"),
    ever_data_fun("smklssevr", "Rauchfreier Tabak"),
    ever_data_fun("pipever", "Pfeife"),
    ever_data_fun("cigarevr", "Zigarre")
  )
)

# Combine 30-day use data for four drugs into one data frame
four_drugs_30 <- as.data.frame(
  rbind(
    data_fun_30("alcdays", "Alkohol"),
    data_fun_30("CIG30USE", "Zigarette"),
    data_fun_30("COCUS30A", "Kokain"),
    data_fun_30("HER30USE", "Heroin")
  )
)

# Combine 30-day use data for tobacco products into one data frame
tobacco_30 <- as.data.frame(
  rbind(
    data_fun_30("CIG30USE", "Zigarette"),
    data_fun_30("SMKLSS30N", "Rauchfreier Tabak"),
    ever_data_fun("PIPE30DY", "Pfeife"),
    data_fun_30("CGR30USE", "Zigarre")
  )
)

# Combine ever-use data for drug dependency analysis
four_drugs_dependency <- as.data.frame(
  rbind(
    ever_data_fun("depndalc", "Alkohol"),
    ever_data_fun("ndssdnsp", "Zigarette"),
    ever_data_fun("depndcoc", "Kokain"),
    ever_data_fun("depndher", "Heroin")
  )
)

########################################################################################################################
# Function to generate timeline plots showing drug use percentages over the years
timeline_plot_fun <- function(table, label_vec, colors, limit, shapes) {
  my_plot(
    table %>%
      ggplot(aes(
        x = year,
        y = count,
        color = factor(drug, levels = label_vec),  # Ensure consistent ordering of drugs
        shape = factor(drug, levels = label_vec)
      )) +
      geom_point(size = 3) +
      geom_line(linewidth = 1) +
      labs(
        color = "Droge:",      # Label for color legend
        shape = "Droge:",      # Label for shape legend
        x = "Jahr",
        y = "Prozent"
      ) +
      theme(
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20)
      ) +
      scale_color_manual(values = colors) +
      scale_shape_manual(values = shapes)
  ) +
    scale_y_continuous(limits = c(0, limit), labels = scales::percent_format())
}

# Generate timeline plots for various drug use metrics
drugs_timeline_ever <- timeline_plot_fun(four_drugs_ever, labels_drugs, drug_colors, 0.85, shapes_drugs)
drugs_timeline_30 <- timeline_plot_fun(four_drugs_30, labels_drugs, drug_colors, 0.85, shapes_drugs)
drugs_timeline_dependency <- timeline_plot_fun(four_drugs_dependency, labels_drugs, drug_colors, 0.08, shapes_drugs)

tobacco_timeline_ever <- timeline_plot_fun(tobacco_ever, labels_tobacco, tobacco_colors, 0.6, shapes_tobacco)
tobacco_timeline_30 <- timeline_plot_fun(tobacco_30, labels_tobacco, tobacco_colors, 0.6, shapes_tobacco)

########################################################################################################################
# Function to create histograms for consumption days over the last 30 days

histogram_fun <- function(data_col, drug_name, limit, colorcode, yearplot) {
  # Prepare data by counting consumption days and converting counts to proportions
  data <- drug_data %>%
    group_by(year) %>%
    count(day = .data[[data_col]]) %>%
    mutate(`Relative share` = n / sum(n)) %>%
    filter(day >= 1 & day <= 30) %>%         # Focus on days 1 to 30
    filter(year == yearplot) %>%              # Subset for the specified year
    ungroup() %>%
    mutate(
      Drug = drug_name,
      day = factor(day, levels = as.character(1:30))
    )
  
  # Create and return the histogram plot
  my_plot(
    ggplot(data, aes(x = day, y = `Relative share`)) +
      geom_col(fill = colorcode, color = "black") +
      labs(
        x = paste0("Anzahl der Konsumtage in den letzten 30 Tagen (n = ", sum(data$n), ")"),
        y = "Prozent"
      ) +
      scale_x_discrete(breaks = c("1", "5", "10", "15", "20", "25", "30"), drop = FALSE)
  ) +
    scale_y_continuous(limits = c(0, limit), labels = scales::percent_format())
}

# Generate histograms for Alcohol, Cigarettes, Cocaine, and Heroin for various years
hist_alc_15 <- histogram_fun("alcdays", "Alcohol", 0.085, "#0072B2", "2015")
hist_alc_16 <- histogram_fun("alcdays", "Alcohol", 0.085, "#0072B2", "2016")
hist_alc_17 <- histogram_fun("alcdays", "Alcohol", 0.085, "#0072B2", "2017")
hist_alc_18 <- histogram_fun("alcdays", "Alcohol", 0.085, "#0072B2", "2018")
hist_alc_19 <- histogram_fun("alcdays", "Alcohol", 0.085, "#0072B2", "2019")

hist_cig_15 <- histogram_fun("CIG30USE", "Cigarettes", 0.12, "#009E73", "2015")
hist_cig_16 <- histogram_fun("CIG30USE", "Cigarettes", 0.12, "#009E73", "2016")
hist_cig_17 <- histogram_fun("CIG30USE", "Cigarettes", 0.12, "#009E73", "2017")
hist_cig_18 <- histogram_fun("CIG30USE", "Cigarettes", 0.12, "#009E73", "2018")
hist_cig_19 <- histogram_fun("CIG30USE", "Cigarettes", 0.12, "#009E73", "2019")

hist_coc_15 <- histogram_fun("COCUS30A", "Cocaine", 0.004, "#E69F00", "2015")
hist_coc_16 <- histogram_fun("COCUS30A", "Cocaine", 0.004, "#E69F00", "2016")
hist_coc_17 <- histogram_fun("COCUS30A", "Cocaine", 0.004, "#E69F00", "2017")
hist_coc_18 <- histogram_fun("COCUS30A", "Cocaine", 0.004, "#E69F00", "2018")
hist_coc_19 <- histogram_fun("COCUS30A", "Cocaine", 0.004, "#E69F00", "2019")

hist_her_15 <- histogram_fun("HER30USE", "Heroin", 0.0006, "#CC79A7", "2015")
hist_her_16 <- histogram_fun("HER30USE", "Heroin", 0.0006, "#CC79A7", "2016")
hist_her_17 <- histogram_fun("HER30USE", "Heroin", 0.0006, "#CC79A7", "2017")
hist_her_18 <- histogram_fun("HER30USE", "Heroin", 0.0006, "#CC79A7", "2018")
hist_her_19 <- histogram_fun("HER30USE", "Heroin", 0.0006, "#CC79A7", "2019")

########################################################################################################################
# Functions for nicotine dependency plots

# Function to generate nicotine dependency distribution plots (only dependent cases)
nicotine_dependency_demo <- function(demo_var, x_label, label_vec) {
  data_sum <- data_2019 %>%
    group_by(.data[[demo_var]]) %>%             # Group by demographic variable
    mutate(total = n()) %>%                      # Calculate total count per group
    filter(ndssdnsp == 1) %>%                    # Filter for nicotine dependency cases
    summarise(count = n(), total = first(total))
  
  lvls <- levels(factor(data_sum[[demo_var]]))
  new_labels <- sapply(seq_along(lvls), function(i) {
    paste0(label_vec[i], " \n(n = ", data_sum$count[data_sum[[demo_var]] == lvls[i]], ")")
  })
  
  data_sum <- data_sum %>% mutate(count = count / total)  # Convert count to proportion
  
  my_plot(
    data_sum %>%
      ggplot(aes(x = factor(.data[[demo_var]]), y = count)) +
      geom_col(fill = "#009E73") +
      scale_x_discrete(name = x_label, labels = new_labels)
  )
}

# Generate nicotine dependency plots for age, race, and gender
nicotine_dependency_age <- nicotine_dependency_demo("CATAG2", "Altersgruppen", age_group_vector)
nicotine_dependency_race <- nicotine_dependency_demo("NEWRACE2", "Race", new_race2_vector)
nicotine_dependency_gender <- nicotine_dependency_demo("irsex", "Geschlecht", ir_sex_vector)

# Function to generate nicotine dependency plots including a non-dependent category
nicotine_dependency_demo_with_none <- function(demo_var, x_label, label_vec) {
  data_sum <- data_2019 %>%
    group_by(.data[[demo_var]], ndssdnsp) %>%    # Group by demographic variable and dependency status
    summarise(count = n(), .groups = "drop") %>%
    group_by(.data[[demo_var]]) %>%
    mutate(total = sum(count)) %>%
    ungroup() %>%
    mutate(
      prop = count / total,                      # Calculate proportion
      NicotineStatus = ifelse(ndssdnsp == 1, "Abhängigkeit", "Keine Abhängigkeit"),
      NicotineStatus = factor(NicotineStatus, levels = c("Keine Abhängigkeit", "Abhängigkeit"))
    )
  
  lvls <- levels(factor(data_sum[[demo_var]]))
  new_labels <- sapply(seq_along(lvls), function(i) {
    paste0(label_vec[i], " \n(n = ", sum(data_sum$count[data_sum[[demo_var]] == lvls[i]]), ")")
  })
  
  my_plot(
    data_sum %>%
      ggplot(aes(x = factor(.data[[demo_var]]), y = prop, fill = NicotineStatus)) +
      geom_col() +
      scale_fill_manual(
        name = "Nikotinabhängigkeit",
        values = c("Abhängigkeit" = "#009E73", "Keine Abhängigkeit" = "gray60")
      ) +
      scale_x_discrete(name = x_label, labels = new_labels)
  )
}

# Generate nicotine dependency plots (with none) for age, race, and gender
nicotine_dependency_age_wn <- nicotine_dependency_demo_with_none("CATAG2", "Altersgruppen", age_group_vector)
nicotine_dependency_race_wn <- nicotine_dependency_demo_with_none("NEWRACE2", "Race", new_race2_vector)
nicotine_dependency_gender_wn <- nicotine_dependency_demo_with_none("irsex", "Geschlecht", ir_sex_vector)

########################################################################################################################

# Function to generate drug dependency distribution plots (excluding non-dependent cases)
drug_dependency_demo <- function(demo_var, label_vec, x_label) {
  data_filtered <- data_2019 %>%
    mutate(Dependency = case_when(
      depndalc == 1 & depndcoc == 0 & depndher == 0 ~ "Alkohol",
      depndcoc == 1 & depndalc == 0 & depndher == 0 ~ "Kokain",
      depndher == 1 & depndalc == 0 & depndcoc == 0 ~ "Heroin",
      depndalc == 1 & depndcoc == 1 | depndalc == 1 & depndher == 1 | depndcoc == 1 & depndher == 1 ~ "Mehrfachabhängigkeit"
    )) %>%
    filter(!is.na(Dependency)) %>%              # Exclude records without a dependency label
    mutate(Dependency = factor(Dependency, levels = c("Alkohol", "Kokain", "Heroin", "Mehrfachabhängigkeit")))
  
  counts <- data_filtered %>% count(.data[[demo_var]])
  lvls <- levels(factor(data_filtered[[demo_var]]))
  new_labels <- sapply(seq_along(lvls), function(i) {
    paste0(label_vec[i], " \n(n = ", counts$n[counts[[demo_var]] == lvls[i]], ")")
  })
  
  my_plot(
    data_filtered %>%
      group_by(.data[[demo_var]], Dependency) %>%
      ggplot(aes(x = factor(.data[[demo_var]]), fill = Dependency)) +
      geom_bar(position = "fill") +
      scale_fill_manual(name = "Drogen", values = drug_dep_color) +
      scale_x_discrete(labels = new_labels) +
      labs(x = x_label)
  )
}

# Generate drug dependency plots for age, gender, and race (without including non-dependent cases)
drug_dependency_age <- drug_dependency_demo("CATAG2", age_group_vector, "Altersgruppen")
drug_dependency_gender <- drug_dependency_demo("irsex", ir_sex_vector, "Geschlecht")
drug_dependency_race <- drug_dependency_demo("NEWRACE2", new_race2_vector, "Race")

# Function to generate drug dependency distribution plots including a non-dependent category
drug_dependency_demo_with_none <- function(demo_var, label_vec, x_label) {
  data_filtered <- data_2019 %>%
    mutate(
      Dependency = case_when(
        depndalc == 1 & depndcoc == 0 & depndher == 0 ~ "Alkohol",
        depndcoc == 1 & depndalc == 0 & depndher == 0 ~ "Kokain",
        depndher == 1 & depndalc == 0 & depndcoc == 0 ~ "Heroin",
        depndalc == 1 & depndcoc == 1 | depndalc == 1 & depndher == 1 | depndcoc == 1 & depndher == 1 ~ "Mehrfachabhängigkeit",
        TRUE ~ "Keine Abhängigkeit"          # Include non-dependent cases
      )
    ) %>%
    mutate(
      Dependency = factor(Dependency, levels = c("Keine Abhängigkeit", "Alkohol", "Kokain", "Heroin", "Mehrfachabhängigkeit"))
    )
  counts <- data_filtered %>% count(.data[[demo_var]])
  lvls <- levels(factor(data_filtered[[demo_var]]))
  new_labels <- sapply(seq_along(lvls), function(i) {
    paste0(label_vec[i], " \n(n = ", counts$n[counts[[demo_var]] == lvls[i]], ")")
  })
  my_plot(
    data_filtered %>%
      group_by(.data[[demo_var]], Dependency) %>%
      ggplot(aes(x = factor(.data[[demo_var]]), fill = Dependency)) +
      geom_bar(position = "fill") +
      scale_fill_manual(
        name = "Drogen",
        values = c(
          "Keine Abhängigkeit" = "gray80",
          "Alkohol" = drug_dep_color[["Alkohol"]],
          "Kokain" = drug_dep_color[["Kokain"]],
          "Heroin" = drug_dep_color[["Heroin"]],
          "Mehrfachabhängigkeit" = drug_dep_color[["Mehrfachabhängigkeit"]]
        )
      ) +
      scale_x_discrete(labels = new_labels) +
      labs(x = x_label)
  )
}

# Generate drug dependency plots (including non-dependent cases) for age, gender, and race
drug_dependency_age_wn <- drug_dependency_demo_with_none("CATAG2", age_group_vector, "Altersgruppen")
drug_dependency_gender_wn <- drug_dependency_demo_with_none("irsex", ir_sex_vector, "Geschlecht")
drug_dependency_race_wn <- drug_dependency_demo_with_none("NEWRACE2", new_race2_vector, "Race")

########################################################################################################################
# Define a detailed color palette for mental health dependency analysis

color_palette <- c(
  "Keine Abhängigkeit_0" = "#e6e6e6", "Keine Abhängigkeit_1" = "#b3b3b3",
  "Keine Abhängigkeit_2" = "#808080", "Keine Abhängigkeit_3" = "#4d4d4d",
  "Alkohol_0" = "#7fd0ff", "Alkohol_1" = "#19abff",
  "Alkohol_2" = "#0072B2", "Alkohol_3" = "#00304c",
  "Kokain_0" = "#ffe7b3", "Kokain_1" = "#ffc74d",
  "Kokain_2" = "#E69F00", "Kokain_3" = "#805700",
  "Heroin_0" = "#f6e8f0", "Heroin_1" = "#da9ebf",
  "Heroin_2" = "#be548e", "Heroin_3" = "#7d2f5a",
  "Mehrfachabhängigkeit_0" = "#e6e6e6", "Mehrfachabhängigkeit_1" = "#b3b3b3",
  "Mehrfachabhängigkeit_2" = "#808080", "Mehrfachabhängigkeit_3" = "#4d4d4d"
)

# Generate a mental health dependency plot based on the MI_CAT_U variable
dependency_mental <- my_plot(
  data_2019 %>%
    mutate(Dependency = case_when(
      depndalc == 1 & depndcoc == 0 & depndher == 0 ~ "Alkohol",
      depndcoc == 1 & depndalc == 0 & depndher == 0 ~ "Kokain",
      depndher == 1 & depndalc == 0 & depndcoc == 0 ~ "Heroin",
      (depndalc == 1 & depndcoc == 1) | (depndalc == 1 & depndher == 1) | (depndcoc == 1 & depndher == 1) ~ "Mehrfachabhängigkeit",
      TRUE ~ "Keine Abhängigkeit"
    )) %>%
    filter(MI_CAT_U >= 0) %>%                      # Filter valid mental health scores
    mutate(FillGroup = paste(Dependency, MI_CAT_U, sep = "_")) %>%  # Create a composite variable for fill color
    group_by(Dependency, MI_CAT_U, FillGroup) %>%  # Group by dependency and mental health score
    summarise(count = n(), .groups = "drop") %>%   # Count occurrences
    ggplot(aes(
      x = factor(Dependency, levels = c("Keine Abhängigkeit", "Alkohol", "Kokain", "Heroin", "Mehrfachabhängigkeit")),
      y = count,
      fill = FillGroup
    )) +
    geom_bar(stat = "identity", position = "fill") +
    scale_x_discrete(labels = c(
      "Keine Abhängigkeit" = "Keine",
      "Alkohol" = "Alkohol",
      "Kokain" = "Kokain",
      "Heroin" = "Heroin",
      "Mehrfachabhängigkeit" = "Mehrere"
    )) +
    scale_fill_manual(
      name = "Mentale Erkrankungen:",
      values = color_palette,
      breaks = c("Keine Abhängigkeit_0", "Keine Abhängigkeit_1", "Keine Abhängigkeit_2", "Keine Abhängigkeit_3"),
      labels = c("Keine", "Leichte", "Mittlere", "Schwere")
    ) +
    labs(x = "Abhängigkeiten")
)

########################################################################################################################
# Create a US states data frame with population and interview counts for mapping

state_data <- data.frame(
  state = c(
    "California", "Texas", "Florida", "New York", "Pennsylvania",
    "Illinois", "Ohio", "Georgia", "North Carolina", "Michigan",
    "New Jersey", "Virginia", "Washington", "Arizona", "Massachusetts",
    "Tennessee", "Indiana", "Missouri", "Maryland", "Wisconsin",
    "Colorado", "Minnesota", "South Carolina", "Alabama", "Louisiana",
    "Kentucky", "Oregon", "Oklahoma", "Connecticut", "Utah", "Iowa", "Nevada", 
    "Arkansas", "Mississippi", "Kansas", "New Mexico", "Nebraska", "Idaho", 
    "West Virginia", "Hawaii", "New Hampshire", "Maine", "Montana", "Rhode Island", 
    "Delaware", "South Dakota", "North Dakota", "Alaska", "Vermont", "Wyoming"
  ),
  population = c(
    39512223, 28995881, 21477737, 19453561, 12801989,
    12671821, 11689100, 10617423, 10488084, 9986857,
    8882190, 8535519, 7614893, 7278717, 6892503,
    6829174, 6732219, 6137428, 6045680, 5822434,
    5758736, 5639632, 5148714, 4903185, 4648794,
    4467673, 4217737, 3956971, 3565287, 3205958, 3155070, 3080156,
    3017804, 2976149, 2913314, 2096829, 1934408, 1787065,
    1792147, 1415872, 1359711, 1344212, 1068778, 1059361,
    973764, 884659, 762062, 731545, 623989, 578759
  ),
  interviews = c(
    4560, 3300, 3300, 3300, 2400,
    2400, 2400, 1500, 1500, 2400,
    1500, 1500, 960, 960, 960,
    960, 960, 960, 960, 960,
    960, 960, 960, 960, 960,
    960, 960, 960, 960, 960, 960, 960,
    960, 960, 960, 960, 960, 960,
    960, 967, 960, 960, 960, 960,
    960, 960, 960, 960, 960, 960
  )
)

# Calculate interviews per 100k population for mapping purposes
state_data <- state_data %>%
  mutate(interviews_per_100k = (interviews / population) * 100000)

# Generate a US map plot using the usmap package, coloring states by interviews per 100k
usa_map <- plot_usmap(
  data = state_data,
  regions = "states",
  values = "interviews_per_100k"
) +
  scale_fill_continuous(
    low = "grey90",
    high = "grey0",
    name = "Umfragen\n(pro 100k)"
  ) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12)
  )

########################################################################################################################
# Calculate odds ratios for depression in relation to drug dependency

df_odds <- data_2019 %>%
  filter(amdeyr %in% c(1, 2)) %>%                 # Filter records with depression status codes 1 and 2
  mutate(
    Dependency = case_when(
      depndalc == 0 & depndcoc == 0 & depndher == 0 ~ "Keine Abhängigkeit",
      depndalc == 1 & depndcoc == 0 & depndher == 0 ~ "Alkohol",
      depndalc == 0 & depndcoc == 1 & depndher == 0 ~ "Kokain",
      depndalc == 0 & depndcoc == 0 & depndher == 1 ~ "Heroin",
      TRUE ~ "Mehrfache Abhängigkeit"
    )
  ) %>%
  group_by(amdeyr, Dependency) %>%
  summarise(Frequency = n(), .groups = "drop") %>%  # Count frequency per depression status and dependency type
  pivot_wider(
    names_from = Dependency,
    values_from = Frequency,
    values_fill = 0
  ) %>%
  mutate(
    amdeyr = recode(amdeyr, `1` = "Depression Ja", `2` = "Depression Nein")
  ) %>%
  tibble::column_to_rownames("amdeyr") %>%
  t() %>%                                          # Transpose the data for easier calculation
  as.data.frame() %>%
  rownames_to_column("Dependency") %>%
  mutate(
    Dependency = factor(Dependency, levels = c("Keine Abhängigkeit", "Alkohol", "Kokain", "Heroin", "Mehrfache Abhängigkeit")),
    Total = `Depression Ja` + `Depression Nein`,
    Odds_Yes = ifelse(`Depression Nein` > 0, `Depression Ja` / `Depression Nein`, NA),
    total_dep_ja = sum(`Depression Ja`),
    total_dep_nein = sum(`Depression Nein`),
    overall_odds = total_dep_ja / total_dep_nein,
    Odds_Ratio = Odds_Yes / overall_odds
  )

# Plot the Odds Ratio (log-scale) for each dependency category with annotations
ggplot(df_odds, aes(x = Dependency, y = Odds_Ratio, color = Dependency)) +
  geom_point(size = 5) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray60") +
  annotate("text", x = 4, y = 1.05, label = "Gesamtdurchschnitt", color = "gray60", vjust = -0.5, size = 5) +
  scale_color_manual(values = drug_dep_colors_5) +
  scale_y_log10(
    limits = c(0.5, max(df_odds$Odds_Ratio, na.rm = TRUE) * 1.2),
    breaks = c(0.1, 0.2, 0.5, 1, 2, 5, 10),
    labels = scales::number_format(accuracy = 0.1)
  ) +
  labs(
    x = "Abhängigkeiten",
    y = "Odds Ratio (log-Skala)",
    color = "Abhängigkeitstyp"
  ) +
  theme(legend.position = "none")
