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