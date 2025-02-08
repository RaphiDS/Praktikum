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