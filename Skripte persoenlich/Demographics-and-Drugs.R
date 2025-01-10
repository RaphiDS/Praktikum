## Packages und Data set
## installing rquired packages
library(ggplot2)
library(tidyverse)

## create new, cleaned dataset for 2019 for everyone to use
data2019 <- allfilterdata %>%
  filter (year == 2019)
#------------------------------------------------------------------------------------------------------
## represent number of people who get health care for substance abuse

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

 ## Cocaine use ever and Dependency comparision:
 cocaine.Use.Dependency <- data.frame(matrix(c(data2019 %>%
                                  filter(cocever == 1 & depndcoc == 1) %>%
                                  count(), data2019 %>%
                                  filter (cocever == 1 & depndcoc == 0) %>%
                                    count(), data2019 %>%
                                    filter(cocever == 2 &  depndcoc == 1) %>%
                                    count(), data2019 %>%
                                    filter(cocever == 2 & depndcoc == 0) %>%
                                    count()), nrow = 2), row.names = c("Used Ever Yes", "Used Ever No")) %>%
   rename ("Drug Abuser" = X1, "Not Drug Abuser" = X2)
 cocaine.Use.Dependency
 mosaicplot(cocaine.Use.Dependency, main = "Drug Abuse if you ever tried cocaine", col = c("skyblue", "darkgreen"))  ##WRONG, Zahlen stimmen schon nicht
#-----------------------------------------------------------------------------------------------
## Nicotine Dependency Gender
 Nic.Dependency.Gender <- data2019 %>%
   select(ndssdnsp,irsex) %>%
   filter(ndssdnsp == 1) %>%
   group_by(irsex) %>%
   summarize(count = n()) %>%
   mutate(count = count/56136)
 
 ggplot(Nic.Dependency.Gender, aes(x = factor(irsex), y = count))+
   geom_col()+
   scale_x_discrete(labels =c("1" = "male", "2" = "Female"))+
   labs(title = "Nikotin Abhängigkeit von Frauen und Männern", x = "Geschlecht")
 
-## Nicotine Dependency Age
Nic.Dependency.Age <- data2019 %>%
   select(catage, ndssdnsp) %>%
   filter(ndssdnsp == 1) %>%
   group_by(catage)%>%
   summarise(count = n()) %>%
   mutate(count = count /56136)
 
ggplot(Nic.Dependency.Age, aes(x = factor(catage, y = count)))+
   geom_col()+
   scale_x_discrete(labels = c("1" = "12-17", "2" = "18-25", "3" = "26-34", "4" = "35+"))+
   labs(title = "Nikotin Abhängigkeit von den Altersgruppen", x = "Gruppe")
#------------------------------------------------------------------------------------------------------------
## Drug Dependency based on gender
Drug.Dependency.Gender <-data2019 %>%
  select(irsex, depndcoc, depndalc, depndher) %>%
  pivot_longer(cols = c(depndcoc,depndher, depndalc), names_to = "Drug", values_to = "Usage") %>%
  filter(Usage == 1)

ggplot(Drug.Dependency.Gender, aes(x = Drug, fill = factor(irsex)))+
  geom_bar(position = "fill")

ggplot(Drug.Dependency.Gender, aes(x = Drug, fill = factor(irsex)))+
  geom_bar()+
  labs(title = "Drug Dependency by Gender")+
  scale_x_discrete(labels = c("depndalc" = "Alcohol dependency", "depndcoc" = "Cocaine Dependency", "depndher" = "Heroine Dependency"))+
  scale_fill_discrete(labels = c("1" = "Male", "2" = "Female"))
 
#-----------------------------------------------------------------------------------------------------------
## Drug Abuse based on gender
Drug.Abuse.Gender <- data2019 %>%
  select(irsex, abusealc, abusecoc, abuseher) %>%
  pivot_longer(cols = c(abusealc, abusecoc, abuseher), names_to = "Drug", values_to = "Usage") %>%
  filter(Usage == 1)

ggplot(Drug.Abuse.Gender, aes(x = Drug, fill = factor(irsex)))+
  geom_bar()
#+geom_bar(position = "fill")

#------------------------------------------------------------------------------------------------------------
## Drug use by age group
Drug.Dependency.Age <- data2019 %>%
  select(catage,depndcoc,depndher, depndalc) %>%
  pivot_longer(cols = c(depndcoc, depndher, depndalc), names_to = "Drug", values_to = "Usage") %>%
  filter (Usage == 1) %>%
  group_by(catage, Drug) %>%
  summarise(count = n()) %>%
  mutate(count = count/56136)

ggplot(Drug.Dependency.Age, aes(x = factor(catage), y = count, fill = factor(Drug)))+
  geom_col(position = "dodge")+
  scale_x_discrete(labels = c("1" = "12-17", "2" = "18-25", "3" = "26-34", "4" = "35+"))+
  labs(title = "Drug Dependency by age group", x = "Age Group")
#------------------------------------------------------------------------------
##Drug use based on Gender and Age
Drug.Gender.Age <- data2019 %>%
  select(irsex,catage, cocever, herever, alcever, smklssevr, cigever) %>%
  filter(1 %in% c(cocever, herever, alcever, smklssevr, cigever)) %>%
  pivot_longer(cols = c(cocever, herever,alcever,smklssevr,cigever), names_to = "Substance") %>%
  filter (value == 1) %>%
  group_by(irsex,catage, Substance) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Relative = Count / 56136) # or maybe 81878?

ggplot(Drug.Gender.Age, aes(x = factor(catage),y = Relative, fill = factor (Substance)))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(~ irsex, labeller = as_labeller(c("1"="Male", "2" = "Female")), ncol = 1)+
  scale_fill_discrete(labels = c("alcever" = "Alcohol", "cigever" = "Cigarette", "cocever" = "Cocaine", "herever" = "Heroine", "smklssevr" = "Smokeless Tabacco"))+
  scale_x_discrete(labels = c("1" = "12-17", "2" = "18-25", "3" = "26-34", "4" = "35+"))

#-----------------------------------------------------------------------------------------
## Drug Abuse and Race

Drug.Abuse.Race <- data2019 %>%
  select(abusealc, abusecoc, abuseher, NEWRACE2) %>%
  pivot_longer(cols = c(abusealc,abusecoc, abuseher), names_to = "Drug", values_to = "Usage") %>%
  filter (Usage == 1)

ggplot(Drug.Abuse.Race, aes(x = factor(NEWRACE2), fill = Drug))+
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
    guide = guide_axis(angle = 45))+
  labs (title = "Drug Abusers by Race")

## Der Plot unten macht keinen Sinn, ich würde nur gerne zeigen/ erwähnen ,dass nur 1 oder 2 % der Bevölkerung wirklich abhängig ist
Drug.Abuse.Comparison <-  data2019 %>%
  select(abusealc, abusecoc, abuseher, NEWRACE2) %>%
  pivot_longer(cols = c(abusealc,abusecoc, abuseher), names_to = "Drug", values_to = "Usage")

ggplot(Drug.Abuse.Comparison, aes(x = Drug, fill = factor(NEWRACE2)))+
  geom_bar(poisition = "fill")+
  facet_wrap (~ Usage)
#-----------------------------------------------------------------------------------------
## Drug Dependency and Race

# Background weighted comparison of users and Non-users: "Who is and isnt doing drugs?"
Dependency.Race <- data2019 %>%
  select(NEWRACE2, depndcoc, depndalc, depndher) %>%
  pivot_longer(cols = c(depndcoc,depndher, depndalc), names_to = "Drug", values_to = "Usage")

ggplot(Dependency.Race , aes(x = Drug, fill = factor(NEWRACE2)))+
  geom_bar(position = "fill")+
  facet_wrap( ~ Usage)+
  scale_x_discrete(labels = c("depndalc" = "Alcohol dependency", "depndcoc" = "Cocaine Dependency", "depndher" = "Heroine Dependency"),
                   guide = guide_axis(angle = 45))+
  scale_fill_discrete (labels = c("1" = "White", "2" = "Afr.Am", "3" = "Am/AK Native", "4" ="Other Pac Isl", "5" = " Asian", "6" = "more than one Race", "7" = "Hispamic"))


# "Of Durg users, how much is each group using?"
Dependent.Users.Race <- data2019 %>%
  select(NEWRACE2, depndcoc, depndalc, depndher) %>%
  pivot_longer(cols = c(depndcoc,depndher, depndalc), names_to = "Drug", values_to = "Usage") %>%
  filter(Usage == 1) %>%
  group_by(NEWRACE2) %>%
  summarise()

ggplot(Dependent.Users.Race , aes(x = factor(NEWRACE2), fill = factor(Drug)))+
  geom_bar(position = "fill")+
  facet_wrap( ~ Usage)+
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
    guide = guide_axis(angle = 45))+
  labs(title = "Percentage of dependent Users of each Race")

#-----------------------------------------------------------------------------------------
## Nikotin Dependency (last month) and Race
Nikotin.Dependence.Race <- data2019 %>%
  select(NEWRACE2, ndssdnsp) %>%
  filter (ndssdnsp == 1)

ggplot(Nikotin.Dependence.Race, aes(x = factor(NEWRACE2)))+
  geom_bar()+
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
    guide = guide_axis(angle = 45))+
  labs(title = "Nicotine Dependency by Race")
