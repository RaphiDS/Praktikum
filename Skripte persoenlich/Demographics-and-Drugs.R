## kovarianz: filter arbeitslos und kokain/ mariuana/(alcohol /cigarette)




substanceUse.Work <- data2019 %>%
  select(wrkdpstyr, wrkselfem, cocrec, herrec) %>%
  filter(
    wrkdpstyr %in% c(1, 2) | wrkselfem %in% c(1, 2),          # Check if wrkdpstyr or wrkselfem equals 1 or 2
    cocrec %in% c(1, 2,91) & herrec %in% c(1, 2, 91) # Check if cocrec, crakrec, and herrec have valid values
  ) %>%
  mutate(
    employed = if_else(wrkdpstyr == 2 | wrkselfem == 2, 2, 1),  # Create "employed" column
    drug = case_when(                                         # Create "drug" column with multiple conditions
      cocrec == 91 & herrec == 1 ~ "heroin",
      cocrec %in% c(1, 2) & herrec %in% c(1, 2) ~ "both",
      cocrec %in% c(1, 2) ~ "cocain",
      TRUE ~ NA_character_                                    # Assign NA for rows that do not match any condition
    )
  ) %>%
  filter(is.na(drug) == FALSE) %>%
  group_by(employed) %>%
   count(drug)
## Verhältnis bilden ----

substanceUse.Work


## Verhältnis bilden

### FALSCH: macht kein sinn inhaltlich --> alles neu machen !!!

ggplot(substanceUse.Work, aes(x = employed[[2]], y = n/56136 )) +     # muss n Zahl aller Befragten sein? oder nur von der Frage?
  geom_col(aes(fill = as.factor(drug)), position = "dodge")+
  labs(x = "Unemployed people", y = "Percentage")


## zu zeigen: von allen die Drogen nehmen, wie viele sind nicht angestellt?


  