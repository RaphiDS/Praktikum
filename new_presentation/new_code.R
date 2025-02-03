
library(tidyverse)
library(usmap)
library(usdata)

########################################################################################################################

load("data_edit/filtered_data.Rdata")

drugdata <- allfilterdata %>%
  mutate(NEWRACE2 =
           factor(NEWRACE2,
                  levels = c(1, 7, 2, 5, 6, 3, 4)
           ))

data2019 <- drugdata %>%
  filter(year == 2019)

########################################################################################################################

theme_custom <-
  theme_light() +
  theme(
    axis.title = element_text(size = 20),
    axis.text  = element_text(size = 20),
    axis.title.x = element_text(margin = margin(t = 20)),
    axis.title.y = element_text(margin = margin(r = 20)),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.position = "bottom"
    )

theme_set(theme_custom)

my_plot <- function(...) {
    ... +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(y = "Prozent", title = "")
}

newrace2_vector <-
  c(
    "1" = "Weiße",
    "2" = "Schwarze\nAfroamerikaner",
    "3" = "Am/Ak\nIndigene",
    "4" = "Indigene Hawaii\n/Paz. Inseln",
    "5" = "Asiaten",
    "6" = "Gemischte",
    "7" = "Hispanische"
    )

catag2_vector <-
  c(
    "1" = "12-17",
    "2" = "18-25",
    "3" = "26+"
    )

irsex_vector <-
  c(
    "1" = "Männer",
    "2"= "Frauen"
    )

colors_drugs <- c("#0072B2", "#009E73", "#E69F00", "#be548e")
labels_drugs <- c("Alkohol", "Zigarette", "Kokain", "Heroin")
shapes_drugs <- c(15:18)

colors_tobacco <- c("#009E73", "darkorchid4","lightblue", "darkgrey")
labels_tobacco <- c("Zigarette", "Zigarre", "Rauchfreier Tabak", "Pfeife")
shapes_tobacco <- c(c(16, 15, 17, 18))

########################################################################################################################

demo_distr_fun <- function(demovar, label_vec, xlab) {
  my_plot(data2019 %>%
  select(all_of(demovar)) %>%
  group_by(.data[[demovar]]) %>%
  summarize(count = n()) %>%
  mutate(count = count / sum(count)) %>%  
  ggplot(aes(
    x = factor(.data[[demovar]]),
    y = count)
    ) +
  geom_col() +
  scale_x_discrete(labels = label_vec) +
  labs(x = xlab))
}

Race.Distribution <-
  demo_distr_fun("NEWRACE2", newrace2_vector, "Race")

Age.Distribution <-
  demo_distr_fun("CATAG2", catag2_vector, "Altersgruppen")

Gender.Distribution <-
  demo_distr_fun("irsex", irsex_vector, "Geschlecht")

########################################################################################################################

everdatafun <- function(datacol, drug) {
  drugdata %>%
  group_by(year, .data[[datacol]]) %>%
  summarize(count = n()) %>%
  mutate(
    count = count / sum(count),
    drug = drug
    ) %>%
  filter(.data[[datacol]] == 1)
}

datafun30 <- function(datacol, drug) {
  drugdata %>%
  group_by(year, .data[[datacol]]) %>%
  summarise(count = n()) %>%
  mutate(
    count = count / sum(count),
    drug = drug
    ) %>%
  filter(
      .data[[datacol]] > 0 &
      .data[[datacol]] < 31
    ) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(count = sum(count)) %>%
  filter(.data[[datacol]] == 1)
}

########################################################################################################################

fourdrugsever <- as.data.frame(
  rbind(
    everdatafun("alcever", "Alkohol"),
    everdatafun("cigever", "Zigarette"),
    everdatafun("cocever", "Kokain"),
    everdatafun("herever", "Heroin")
  )
)

tobaccoever <- as.data.frame(
  rbind(
    everdatafun("cigever", "Zigarette"),
    everdatafun("smklssevr", "Rauchfreier Tabak"),
    everdatafun("pipever", "Pfeife"),
    everdatafun("cigarevr", "Zigarre")
  )
)

fourdrugs30 <- as.data.frame(
  rbind(
    datafun30("alcdays", "Alkohol"),
    datafun30("CIG30USE", "Zigarette"),
    datafun30("COCUS30A", "Kokain"),
    datafun30("HER30USE", "Heroin")
  )
)

tobacco30 <- as.data.frame(
  rbind(
    datafun30("CIG30USE", "Zigarette"),
    datafun30("SMKLSS30N", "Rauchfreier Tabak"),
    everdatafun("PIPE30DY", "Pfeife"),
    datafun30("CGR30USE", "Zigarre")
  )
)

fourdrugsdependency <- as.data.frame(
  rbind(
    everdatafun("depndalc", "Alkohol"),
    everdatafun("ndssdnsp", "Zigarette"),
    everdatafun("depndcoc", "Kokain"),
    everdatafun("depndher", "Heroin")
  )
)

########################################################################################################################

timeline_fun <- function(table, label_vec, colors, limit, shapes) {
  my_plot(
    table %>%
    ggplot(aes(
      x = year,
      y = count,
      color = factor(drug, levels = label_vec),
      shape = factor(drug, levels = label_vec)
    )) +
    geom_point(size = 3) +
    geom_line(linewidth = 1) +
    labs(
      color = "Droge:",
      shape = "Droge:",
      x = "Jahr",
      y = "Prozent"
    ) +
    theme(
      legend.title = element_text(size = 20),
      legend.text =  element_text(size = 20)
    ) +
    scale_color_manual(values = colors) +
    scale_shape_manual(values = shapes)
  )  +
    scale_y_continuous(limits = c(0, limit), labels = scales::percent_format())
}

########################################################################################################################
Drugs.Timeline.Ever <-
  timeline_fun(fourdrugsever, labels_drugs, colors_drugs, 0.85, shapes_drugs)

Drugs.Timeline.30 <-
  timeline_fun(fourdrugs30, labels_drugs, colors_drugs, 0.85, shapes_drugs)

Drugs.Timeline.Dependency <-
  timeline_fun(fourdrugsdependency, labels_drugs, colors_drugs, 0.08, shapes_drugs)


Tobacco.Timeline.Ever <-
  timeline_fun(tobaccoever, labels_tobacco, colors_tobacco, 0.6, shapes_tobacco)

Tobacco.Timeline.30 <-
  timeline_fun(tobacco30, labels_tobacco, colors_tobacco, 0.6, shapes_tobacco)

########################################################################################################################

histogram_fun <- function(datacol, drug_name, limit, colorcode, yearplot) {
  
  data <-
    drugdata %>%
    group_by(year) %>%
    count(day = .data[[datacol]]) %>%
    mutate(`Relative share` = n / sum(n)) %>%
    filter(
      day >= 1 &
        day <= 30
    ) %>%
    filter(year == yearplot) %>%
    ungroup() %>%
    mutate(
      Drug = drug_name,
      day  = factor(day, levels = as.character(1:30))
    )

  my_plot(
    ggplot(data, aes(x = day, y = `Relative share`)) +
    geom_col(fill = colorcode, color = "black") +
    labs(
      x =
        paste0(
        "Anzahl der Konsumtage in den letzten 30 Tagen (n = ",
        sum(data$n),
        ")"
      ),
      y = "Prozent"
    ) +
    scale_x_discrete(breaks = c("1", "5", "10", "15", "20", "25", "30"), drop = FALSE)
  ) +
  scale_y_continuous(limits = c(0, limit), labels = scales::percent_format())
}

########################################################################################################################

Histogram.Alc.15 <- histogram_fun("alcdays", "Alcohol", 0.085, "#0072B2", "2015")

Histogram.Alc.16 <- histogram_fun("alcdays", "Alcohol", 0.085, "#0072B2", "2016")

Histogram.Alc.17 <- histogram_fun("alcdays", "Alcohol", 0.085, "#0072B2", "2017")

Histogram.Alc.18 <- histogram_fun("alcdays", "Alcohol", 0.085, "#0072B2", "2018")

Histogram.Alc.19 <- histogram_fun("alcdays", "Alcohol", 0.085, "#0072B2", "2019")

Histogram.Cig.15 <- histogram_fun("CIG30USE", "Cigarettes", 0.12, "#009E73", "2015")

Histogram.Cig.16 <- histogram_fun("CIG30USE", "Cigarettes", 0.12, "#009E73", "2016")

Histogram.Cig.17 <- histogram_fun("CIG30USE", "Cigarettes", 0.12, "#009E73", "2017")

Histogram.Cig.18 <- histogram_fun("CIG30USE", "Cigarettes", 0.12, "#009E73", "2018")

Histogram.Cig.19 <- histogram_fun("CIG30USE", "Cigarettes", 0.12, "#009E73", "2019")

Histogram.Coc.15 <- histogram_fun("COCUS30A", "Cocaine", 0.004, "#E69F00","2015")

Histogram.Coc.16 <- histogram_fun("COCUS30A", "Cocaine", 0.004, "#E69F00","2016")

Histogram.Coc.17 <- histogram_fun("COCUS30A", "Cocaine", 0.004, "#E69F00","2017")

Histogram.Coc.18 <- histogram_fun("COCUS30A", "Cocaine", 0.004, "#E69F00","2018")

Histogram.Coc.19 <- histogram_fun("COCUS30A", "Cocaine", 0.004, "#E69F00","2019")

Histogram.Her.15 <- histogram_fun("HER30USE", "Heroin", 0.0006, "#CC79A7", "2015")

Histogram.Her.16 <- histogram_fun("HER30USE", "Heroin", 0.0006, "#CC79A7", "2016")

Histogram.Her.17 <- histogram_fun("HER30USE", "Heroin", 0.0006, "#CC79A7", "2017")

Histogram.Her.18 <- histogram_fun("HER30USE", "Heroin", 0.0006, "#CC79A7", "2018")

Histogram.Her.19 <- histogram_fun("HER30USE", "Heroin", 0.0006, "#CC79A7", "2019")

########################################################################################################################

nic_dependency_demo <- function(demog, label, labelvec) {
  data_sum <- data2019 %>%
    group_by(.data[[demog]]) %>%
    mutate(total = n()) %>%
    filter(ndssdnsp == 1) %>%
    summarise(count = n(), total = first(total))
  lvls <- levels(factor(data_sum[[demog]]))
  new_labels <- sapply(seq_along(lvls), function(i) {
    paste0(labelvec[i], " \n(n = ", data_sum$count[data_sum[[demog]] == lvls[i]], ")")
  })
  data_sum <- data_sum %>% mutate(count = count / total)
  my_plot(
    data_sum %>%
      ggplot(aes(
        x = factor(.data[[demog]]),
        y = count
      )) +
      geom_col(fill = "#009E73") +
      scale_x_discrete(
        name = label,
        labels = new_labels
      )
  )
}

########################################################################################################################

Nic.Dependency.Age <-
  nic_dependency_demo("CATAG2", "Altersgruppen", catag2_vector)

Nic.Dependency.Race <-
  nic_dependency_demo("NEWRACE2", "Race", newrace2_vector)

Nic.Dependency.Gender <-
  nic_dependency_demo("irsex", "Geschlecht", irsex_vector)

########################################################################################################################

drug_dep_color <- c(
  "Alkohol" = "#0072B2",
  "Kokain" = "#E69F00",
  "Heroin" = "#CC79A7",
  "Mehrfachabhängigkeit" = "grey20"
  )

drug_dep_colors_5 <- c("Keine Abhängigkeit" = "gray60",
                 "Alkohol" = "#0072B2",
                 "Kokain" = "#E69F00",
                 "Heroin" = "#CC79A7",
                 "Mehrfache Abhängigkeit" = "grey20")

########################################################################################################################

drug_dependency_demo <- function(demog, labelvec, xlabel) {
  data_filtered <- data2019 %>%
    mutate(Dependency = case_when(
      depndalc == 1 & depndcoc == 0 & depndher == 0 ~ "Alkohol",
      depndcoc == 1 & depndalc == 0 & depndher == 0 ~ "Kokain",
      depndher == 1 & depndalc == 0 & depndcoc == 0 ~ "Heroin",
      depndalc == 1 & depndcoc == 1 | depndalc == 1 & depndher == 1 | depndcoc == 1 & depndher == 1 ~ "Mehrfachabhängigkeit"
    )) %>%
    filter(!is.na(Dependency)) %>%
    mutate(Dependency = factor(Dependency, levels = c("Alkohol","Kokain","Heroin","Mehrfachabhängigkeit")))
  counts <- data_filtered %>% count(.data[[demog]])
  lvls <- levels(factor(data_filtered[[demog]]))
  new_labels <- sapply(seq_along(lvls), function(i) {
    paste0(labelvec[i]," \n(n = ", counts$n[counts[[demog]] == lvls[i]], ")")
  })
  my_plot(
    data_filtered %>%
      group_by(.data[[demog]], Dependency) %>%
      ggplot(aes(x = factor(.data[[demog]]), fill = Dependency)) +
      geom_bar(position = "fill") +
      scale_fill_manual(name = "Drogen", values = drug_dep_color) +
      scale_x_discrete(labels = new_labels) +
      labs(x = xlabel)
  )
}

########################################################################################################################

Drug.Dependency.Age <-
  drug_dependency_demo("CATAG2", catag2_vector, "Altersgruppen")

Drug.Dependency.Gender <-
  drug_dependency_demo("irsex", irsex_vector, "Geschlecht")

Drug.Dependency.Race <-
  drug_dependency_demo("NEWRACE2", newrace2_vector, "Race")

########################################################################################################################

Dependency.Mental <- data2019 %>%
  mutate(Dependency = case_when(
    depndalc == 1 & depndcoc == 0 & depndher == 0 ~ "Alkohol",
    depndcoc == 1 & depndalc == 0 & depndher == 0 ~ "Kokain",
    depndher == 1 & depndalc == 0 & depndcoc == 0 ~ "Heroin",
    (depndalc == 1 & depndcoc == 1) | (depndalc == 1 & depndher == 1) | (depndcoc == 1 & depndher == 1) ~ "Mehrfachabhängigkeit",
    TRUE ~ "Keine Abhängigkeit"
    )
  ) %>%
  filter(MI_CAT_U >= 0) %>%
  mutate(FillGroup = paste(Dependency, MI_CAT_U, sep = "_")) %>%
  group_by(Dependency, MI_CAT_U, FillGroup) %>%
  summarise(count = n(), .groups = "drop")

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
my_plot(
  ggplot(
    data = Drug.Dependency.Total,
    aes(
      x = factor(
        Dependency,
        levels = c(
          "Keine Abhängigkeit",
          "Alkohol",
          "Kokain",
          "Heroin",
          "Mehrfachabhängigkeit"
        )
      ),
      y = count,
      fill = FillGroup
    )
  ) +
    geom_bar(
      stat = "identity",
      position = "fill"
    ) +
    scale_x_discrete(
      labels = c(
        "Keine Abhängigkeit"     = "Keine",
        "Alkohol"                = "Alkohol",
        "Kokain"                 = "Kokain",
        "Heroin"                 = "Heroin",
        "Mehrfachabhängigkeit"   = "Mehrere"
      )
    ) +
    scale_fill_manual(
      name = "Mentale Erkrankungen:",
      values = color_palette,
      breaks = c(
        "Keine Abhängigkeit_0",
        "Keine Abhängigkeit_1",
        "Keine Abhängigkeit_2",
        "Keine Abhängigkeit_3"
      ),
      labels = c(
        "Keine",
        "Leichte",
        "Mittlere",
        "Schwere"
      )) +
    labs(
      x = "Abhängigkeiten",
    )
)

########################################################################################################################

SubsAbhängig.Gesundheit <-
  my_plot(
  data2019 %>%
  mutate(
    Dependency = case_when(
      depndalc == 1 & depndcoc == 0 & depndher == 0 ~ "Alkohol",
      depndcoc == 1 & depndalc == 0 & depndher == 0 ~ "Kokain",
      depndher == 1 & depndalc == 0 & depndcoc == 0 ~ "Heroin",
      depndalc == 1 & depndcoc == 1 | depndalc == 1 & depndher == 1 | depndcoc == 1 & depndher == 1 ~ "Mehrfachabhängigkeit",
      TRUE ~ "Keine"
    )) %>%
  filter(MI_CAT_U >= 0) %>%
  group_by(Dependency, MI_CAT_U) %>%
  summarise(count = n(), .groups = "drop") %>%
  ggplot(aes(
    x = factor(Dependency,
               levels = c(
                 "Keine",
                 "Alkohol",
                 "Kokain",
                 "Heroin",
                 "Mehrfachabhängigkeit"
                 )), 
      fill = factor(MI_CAT_U))) +
      geom_bar(
        stat = "identity",
        position = "fill",
        aes(y = count)
      ) +
      scale_fill_manual(name = "Mentale Erkrankungen:",
                        labels = c("0" = "Keine", 
                                   "1" = "Milde", 
                                   "2" = "Moderate", 
                                   "3" = "Schwere"),
                        values = c("grey80", "grey65", "grey45", "grey30")) + # Farben für Mental Health Kategorien
      scale_y_continuous(labels = scales::percent_format())+
      labs(x = "Abhängigkeiten", y = "Prozent")
      )

########################################################################################################################

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

state_data <- state_data %>%
  mutate(
    interviews_per_100k = (interviews / population) * 100000
  )

Karte.USA <-
  plot_usmap(
  data    = state_data,
  regions = "states",
  values  = "interviews_per_100k"
) +
  scale_fill_continuous(
    low   = "grey90",
    high  = "grey0",
    name  = "Umfragen\n(pro 100k)"
  ) +
  theme(legend.position = "right",
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12))

########################################################################################################################

df <- data2019 %>%
  filter(amdeyr %in% c(1, 2)) %>%
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
  summarise(Frequency = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = Dependency,
    values_from = Frequency,
    values_fill = 0
  ) %>%
  mutate(
    amdeyr = recode(amdeyr, `1` = "Depression Ja", `2` = "Depression Nein")
  ) %>%
  tibble::column_to_rownames("amdeyr") %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column("Dependency") %>%
  mutate(
    Dependency = factor(
      Dependency,
      levels = c(
        "Keine Abhängigkeit",
        "Alkohol",
        "Kokain",
        "Heroin",
        "Mehrfache Abhängigkeit"
      )
    ),
    Total = `Depression Ja` + `Depression Nein`,
    Odds_Yes = ifelse(`Depression Nein` > 0, `Depression Ja` / `Depression Nein`, NA),
    total_dep_ja = sum(`Depression Ja`),
    total_dep_nein = sum(`Depression Nein`),
    overall_odds = total_dep_ja / total_dep_nein,
    Odds_Ratio = Odds_Yes / overall_odds
  )

ggplot(df, aes(x = Dependency, y = Odds_Ratio, color = Dependency)) +
  geom_point(size = 5) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray60") +
  annotate(
    "text",
    x = 4,
    y = 1.05,
    label = "Gesamtdurchschnitt",
    color = "gray60",
    vjust = -0.5,
    size = 5
  ) +
  scale_color_manual(values = drug_colors) +
  scale_y_log10(
    limits = c(0.5, max(df$Odds_Ratio, na.rm = TRUE) * 1.2),
    breaks = c(0.1, 0.2, 0.5, 1, 2, 5, 10),
    labels = scales::number_format(accuracy = 0.1)
  ) +
  labs(
    x = "Abhängigkeiten",
    y = "Odds Ratio (log-Skala)",
    color = "Abhängigkeitstyp"
  ) +
  theme(
    legend.position = "none"
  )
