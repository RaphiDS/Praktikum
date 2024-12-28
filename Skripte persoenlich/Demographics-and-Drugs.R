## Packages und Data set
## installing rquired packages
library(ggplot2)
library(tidyverse)

## create new, cleaned dataset for 2019 for everyone to use
data2019 <- allfilterdata %>%
  filter (year == 2019)
#--------------------------------------------------------------------------------------

## kovarianz: filter arbeitslos und kokain/ mariuana/(alcohol /cigarette)



## zu zeigen: von allen die Drogen nehmen, wie viele sind nicht angestellt?

#------------------------------------------------------------------------------------------------------
## represent number of people who get health care for substance abuse
# categorize into yes(1), no(2), don't know (94), skip (prvhlthin = 2 --> no private insurance!, 97/98)
insurance.substance <- data2019 %>%
  select(hltinalc,hltindrg)%>%
  filter(hltinalc < 3 & hltindrg < 3) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "values") 

## plot comparing the values
ggplot(insurance.substance, aes(x = values, fill = as.factor(values)))+
  geom_bar()+
  facet_wrap( ~ variable)+
  scale_fill_manual (values = c("1" = "red", "2" = "turquoise"), labels = c("1" = "Yes", "2" = "No"))+
  scale_x_discrete(labels = c("1" = "Yes", "2" = "No")) +
  labs( title = "Drug and alcohol abuse covered by private Insurance", fill = " Insurance status", x = "Status")


#----------------------------------------------------------------------------------------------------
## Drinking in American Indian Areas (AI) ---------------> do i use rbind to join AI and other Race?
AI.Alcohol.Usage <- data2019 %>%
  select(MAIIN102, NEWRACE2,ALCBNG30D) %>%
  filter (ALCBNG30D < 94)%>%
  filter (remove(ALCBNG30D == 85)) ## wrong code

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

 mosaicplot(DrugGender, main = "Drug use dependend on gender",col = c("lavender", "skyblue"))



