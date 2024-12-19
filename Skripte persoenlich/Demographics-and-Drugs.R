## Packages und Data set
## installing rquired packages
library(ggplot2)
library(tidyverse)

## create new, cleaned dataset for 2019 for everyone to use
data2019 <- allfilterdata %>%
  filter (year == 2019)
#--------------------------------------------------------------------------------------

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

#-----------------------------------------------------------------------------------------
## represent number of people who get health care for substance abuse
# categorize into yes(1), no(2), don't know (94), skip (prvhlthin = 2 --> no private insurance!, 97/98)
insurance.substance <- data2019 %>%
  select(hltinalc,hltindrg)%>%
  filter(hltinalc < 94 & hltindrg < 94) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "values") 

## plot comparing the values
ggplot(insurance.substance, aes(x = as.factor (values), fill = as.factor(values)))+
  geom_bar()+
  facet_wrap( ~ variable)+
  ggtitle("Drug and alcohol abuse covered by private Insurance")+ 
  labs( fill = " Insurance status", x = "Status")


#----------------------------------------------------------------------------------------------------
## Drinking in American Indian Areas (AI)

#----------------------------------------------------------------------------------------------------
## Trial Mosaikplot: Cocain and Gender
GenderCOCaine <- data2019 %>%
  select(irsex, cocever) %>%
  filter(cocever < 94)%>%
  group_by(irsex) %>%
  summarize(usage = sum(cocever ==1), clean = sum(cocever ==2))
  


DrugGender <-data.frame(matrix(c(data2019 %>%
                    filter(irsex ==1 & cocever == 1) %>%
                    count(), data2019 %>%
                    filter(irsex == 1 & cocever == 2) %>%
                    count(), data2019 %>%
                    filter(irsex == 2 & cocever ==1) %>%
                    count(), data2019 %>%
                    filter(irsex == 2 & cocever == 2) %>%
                    count ()), nrow =2), row.names = c("Cocain Yes", "Cocain No")) %>%
  rename("Male" = X1, "Female" = X2)
DrugGender

 mosaicplot(DrugGender, main = "Drug use dependend on gender",col = c("purple", "pink"))



