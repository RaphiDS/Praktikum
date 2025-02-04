drug_dependency_demo_with_none <- function(demog, labelvec, xlabel) {
  data_filtered <- data2019 %>%
    mutate(
      Dependency = case_when(
        depndalc == 1 & depndcoc == 0 & depndher == 0 ~ "Alkohol",
        depndcoc == 1 & depndalc == 0 & depndher == 0 ~ "Kokain",
        depndher == 1 & depndalc == 0 & depndcoc == 0 ~ "Heroin",
        depndalc == 1 & depndcoc == 1 | depndalc == 1 & depndher == 1 | depndcoc == 1 & depndher == 1 ~ "Mehrfachabhängigkeit",
        TRUE ~ "Keine Abhängigkeit"
      )
    ) %>%
    mutate(
      Dependency = factor(
        Dependency,
        levels = c("Keine Abhängigkeit", "Alkohol", "Kokain", "Heroin", "Mehrfachabhängigkeit")
      )
    )
  counts <- data_filtered %>% count(.data[[demog]])
  lvls <- levels(factor(data_filtered[[demog]]))
  new_labels <- sapply(seq_along(lvls), function(i) {
    paste0(labelvec[i], " \n(n = ", counts$n[counts[[demog]] == lvls[i]], ")")
  })
  my_plot(
    data_filtered %>%
      group_by(.data[[demog]], Dependency) %>%
      ggplot(aes(x = factor(.data[[demog]]), fill = Dependency)) +
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
      labs(x = xlabel)
  )
}


Drug.Dependency.Age.WN <-
  drug_dependency_demo_with_none("CATAG2", catag2_vector, "Altersgruppen")

Drug.Dependency.Gender.WN <-
  drug_dependency_demo_with_none("irsex", irsex_vector, "Geschlecht")

Drug.Dependency.Race.WN <-
  drug_dependency_demo_with_none("NEWRACE2", newrace2_vector, "Race")