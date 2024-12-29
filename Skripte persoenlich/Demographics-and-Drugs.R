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
  select(MAIIN102, cabingflg) %>% ## using this vauraible since more than 4/5 drinks is categorized as binge drinking
  filter (cabingflg < 85) #%>%
  
ggplot(AI.Alcohol.Usage, aes(x = MAIIN102, fill = factor(cabingflg))) +
  geom_bar(position = "fill")+
  scale_fill_discrete(labels = c("0" = "No report of binge drinking", "1"= "Binge alcohol use on 1+ days", "11" = "Report of 4+/5+ drinks"))+
  scale_x_discrete (name = c("1" = "lives in AI Segment", "2" = "Not in AI Segment"))
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

#-----------------------------------------------------------------------------------------------
## Drugs and Race
 Race.Cocaine <- data2019 %>%
   select(NEWRACE2, depndcoc)
 
ggplot(Race.Cocaine, aes(x = depndcoc, fill = factor(NEWRACE2)))+
  geom_bar(postion = "fill")

## Better --> only dependt users
Race.Coke.Dependendce <- data2019 %>%
  select(NEWRACE2, depndcoc) %>%
  filter( depndcoc == 1) 
  

ggplot(Race.Coke.Dependendce, aes(x = factor(NEWRACE2))) +
  geom_bar() +
  scale_x_discrete(
    labels = c(
      "1" = "White",
      "2" = "Afr.Am",
      "3" = "Am/AK Native",
      "4" = "Other Pac Isl",
      "5" = "Asian",
      "6" = "More than one race",
      "7" = "Hispanic"
    ),
    guide = guide_axis(angle = 45))


#-----------------------------------------------------------------------------------------
## Heroine and race 
# ---> combine it !!!

Dependency.Race <- data2019 %>%
  select(NEWRACE2, depndcoc, depndalc, depndher) %>%
  pivot_longer(cols = c(depndcoc,depndher, depndalc), names_to = "Drug", values_to = "Usage") %>%
  filter(Usage == 1)

ggplot(Dependency.Race , aes(x = factor(NEWRACE2), fill = factor(Drug)))+
         geom_bar(position = "fill")+
  scale_x_discrete(
    labels = c(
      "1" = "White",
      "2" = "Afr.Am",
      "3" = "Am/AK Native",
      "4" = "Other Pac Isl",
      "5" = "Asian",
      "6" = "More than one race",
      "7" = "Hispanic"
    ),
    guide = guide_axis(angle = 45))


       