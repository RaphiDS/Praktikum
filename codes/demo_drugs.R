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