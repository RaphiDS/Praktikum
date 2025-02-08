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
ggsave("presentation_files/plots/race_distribution.png",
       plot = race_distribution, width = 18, height = 9, dpi = 300)
age_distribution <- demo_distribution_fun("CATAG2", age_group_vector, "Altersgruppen")
ggsave("presentation_files/plots/age_distribution.png",
       plot = age_distribution, width = 18, height = 9, dpi = 300)
gender_distribution <- demo_distribution_fun("irsex", ir_sex_vector, "Geschlecht")
ggsave("presentation_files/plots/gender_distribution.png",
       plot = gender_distribution, width = 18, height = 9, dpi = 300)

########################################################################################################################