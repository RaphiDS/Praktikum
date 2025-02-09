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

# create the n
dependency_counts <- data_2019 %>%
  mutate(Dependency = case_when(
    depndalc == 1 & depndcoc == 0 & depndher == 0 ~ "Alkohol",
    depndcoc == 1 & depndalc == 0 & depndher == 0 ~ "Kokain",
    depndher == 1 & depndalc == 0 & depndcoc == 0 ~ "Heroin",
    (depndalc == 1 & depndcoc == 1) | (depndalc == 1 & depndher == 1) | (depndcoc == 1 & depndher == 1) ~ "Mehrfachabhängigkeit",
    TRUE ~ "Keine Abhängigkeit"
  )) %>%
  filter(MI_CAT_U >= 0) %>%   # just cases shown in the plot
  count(Dependency)

# defines the labels
dependency_order <- c("Keine Abhängigkeit", "Alkohol", "Kokain", "Heroin", "Mehrfachabhängigkeit")
base_labels <- c("Keine", "Alkohol", "Kokain", "Heroin", "Mehrere")

# Creates new labels for (n = )
new_labels <- sapply(seq_along(dependency_order), function(i) {
  count_value <- dependency_counts$n[dependency_counts$Dependency == dependency_order[i]]
  paste0(base_labels[i], "\n(n = ", count_value, ")")
})
# ----------------------------------------------------------------------


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
    scale_x_discrete(labels = new_labels) +
    scale_fill_manual(
      name = "Mentale Erkrankungen:",
      values = color_palette,
      breaks = c("Keine Abhängigkeit_0", "Keine Abhängigkeit_1", "Keine Abhängigkeit_2", "Keine Abhängigkeit_3"),
      labels = c("Keine", "Leichte", "Mittlere", "Schwere")
    ) +
    labs(x = "Abhängigkeiten")
)
ggsave("presentation_files/plots/dependency_mental.png", plot = dependency_mental, width = 18, height = 10, dpi = 300)
########################################################################################################################