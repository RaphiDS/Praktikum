
# Plot ABhängigkeit Drogen

fourdrugsdependency <- as.data.frame(
  rbind(
    everdatafun("depndalc", "Alkohol"),
    everdatafun("ndssdnsp", "Zigarette"),
    everdatafun("depndcoc", "Kokain"),
    everdatafun("depndher", "Heroin")
  )
)

Substanzen.Verlauf.Abh <- ggplot(fourdrugsdependency, aes(x = Year, y = .data[["Anteil"]],
                                                color = factor(Drug, levels = c("Alkohol", "Zigarette", "Kokain", "Heroin")),
                                                shape = factor(Drug, levels = c("Alkohol", "Zigarette", "Kokain", "Heroin")))) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  theme_light() +
  labs(
    color = "Droge",
    shape = "Droge",
    x = "Jahr",
    y = "Anteil"
  ) +
  theme(
    axis.title = element_text(size = 20),  # Achsentitel
    axis.text  = element_text(size = 20),  # Achsbeschriftungen
    legend.title = element_text(size = 20),  # Legendentitel
    legend.text =  element_text(size = 20),  # Legendentext
  ) + scale_y_continuous(limits = c(0, NA)) +
  scale_color_manual(values = c("#0072B2", "#009E73", "#E69F00", "#CC79A7")) +
  scale_shape_manual(values = c(15:18))# beliebige Form-Codes


ggsave("Presentation_files/Pres_plots/Substanzen_Verlauf_Abhängigkeit_plot.png",
       plot = Substanzen.Verlauf.Abh, width = 15, height = 8, dpi = 300)


####################################

