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
ggsave("presentation_files/plots/nicotine_dependency_age.png",
       plot = nicotine_dependency_age, width = 18, height = 9, dpi = 300)
nicotine_dependency_race <- nicotine_dependency_demo("NEWRACE2", "Race", new_race2_vector)
ggsave("presentation_files/plots/nicotine_dependency_race.png",
       plot = nicotine_dependency_race, width = 18, height = 9, dpi = 300)
nicotine_dependency_gender <- nicotine_dependency_demo("irsex", "Geschlecht", ir_sex_vector)
ggsave("presentation_files/plots/nicotine_dependency_gender.png",
       plot = nicotine_dependency_gender, width = 18, height = 9, dpi = 300)

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
      scale_x_discrete(name = x_label, labels = new_labels) +
      theme(legend.text = element_text(size = 20),
            legend.title = element_text(size = 20))
  )
}

# Generate nicotine dependency plots (with none) for age, race, and gender
nicotine_dependency_age_wn <- nicotine_dependency_demo_with_none("CATAG2", "Altersgruppen", age_group_vector)
ggsave("presentation_files/plots/nicotine_dependency_age_wn.png",
       plot = nicotine_dependency_age_wn, width = 18, height = 9, dpi = 300)
nicotine_dependency_race_wn <- nicotine_dependency_demo_with_none("NEWRACE2", "Race", new_race2_vector)
ggsave("presentation_files/plots/nicotine_dependency_race_wn.png",
       plot = nicotine_dependency_race_wn, width = 18, height = 9, dpi = 300)
nicotine_dependency_gender_wn <- nicotine_dependency_demo_with_none("irsex", "Geschlecht", ir_sex_vector)
ggsave("presentation_files/plots/nicotine_dependency_gender_wn.png",
       plot = nicotine_dependency_gender_wn, width = 18, height = 9, dpi = 300)
########################################################################################################################