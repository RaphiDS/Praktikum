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
odds <- ggplot(df_odds, aes(x = Dependency, y = Odds_Ratio, color = Dependency)) +
  geom_point(size = 5) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray60") +
  annotate("text", x = 4, y = 1.05, label = "Gesamtdurchschnitt", color = "gray60", vjust = -0.5, size = 5) +
  scale_color_manual(values = drug_dep_colors_5) +
  scale_y_log10(
    limits = c(0.5, max(df_odds$Odds_Ratio, na.rm = TRUE) * 1.2),
    breaks = c(0.1, 0.2, 0.5, 1, 2, 4, 8),
    labels = scales::number_format(accuracy = 0.1)
  ) +
  labs(
    x = "Abhängigkeiten",
    y = "Odds Ratio (log-Skala)",
    color = "Abhängigkeitstyp"
  ) +
  theme(legend.position = "none")

ggsave("presentation_files/plots/odds.png", plot = odds, width = 18, height = 10, dpi = 300)
########################################################################################################################