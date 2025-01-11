## Set-up
library(tidyverse)
## create new, cleaned dataset for 2019 for everyone to use
load("Daten bearbeitet/combi_redu_data.Rdata")
data2019 <- allfilterdata %>%
  filter (year == 2019)
#-------------------------------------------------------------------------------
## AGE grouped into allocated Categories of NSDUH
age.grouped <- data2019 %>%
  select(catage) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "Group") %>%
  group_by(Group) %>%
  summarize(count =n()/56136)

ggplot(age.grouped, aes(x= factor(Group),y = count, fill = factor(Group, labels = c("12-17", "18-25", "26-34", "35+")))) +
  geom_col() +
  labs(title = "Age in allocated Groups", y = "Percentage", fill = "Age groups", x = "Group") +
  theme_light() +
  theme(
    axis.title = element_text(size = 20),  # Achsentitel
    axis.text  = element_text(size = 20),  # Achsbeschriftungen
    legend.position = "none"  # Legendentext
  ) +
  scale_x_discrete(labels = c("12-17", "18-25", "26-34", "35+"))

#-------------------------------------------------------------------------------
## Distribution Race
Race.Destr <- data2019 %>%
  select(NEWRACE2) %>%
  pivot_longer(cols = everything(), names_to = "Var", values_to = "Answer") %>%
  group_by(Answer) %>%
  summarize(count = n()) %>%
  mutate(count = count/56136)

ggplot(Race.Destr, aes(x = factor(Answer), y = count, fill = factor(Answer)))+
  geom_col()+
  scale_x_discrete(labels = c("1" = "White", "2" = "Afr.Am", "3" = "Am/AK Native", "4" ="Other Pac Isl", "5" = " Asian", "6" = "more than one Race", "7" = "Hispamic"),
                   guide = guide_axis(angle = 45)) +
  labs(title = "Race ", y = "Percentage", x = "Background") +
  theme_light() +
  theme(
    axis.title = element_text(size = 20),  # Achsentitel
    axis.text  = element_text(size = 20),  # Achsbeschriftungen
    legend.position = "none"  # Legendentext
  )

#-------------------------------------------------------------------------------
## Nicotine Dependency Gender
Nic.Dependency.Gender <- data2019 %>%
  select(ndssdnsp,irsex) %>%
  filter(ndssdnsp == 1) %>%
  group_by(irsex) %>%
  summarize(count = n()) %>%
  mutate(count = count/56136)

#-------------------------------------------------------------------------------
## Nicotine Dependency Age
Nic.Dependency.Age <- data2019 %>%
  select(catage, ndssdnsp) %>%
  filter(ndssdnsp == 1) %>%
  group_by(catage)%>%
  summarise(count = n()) %>%
  mutate(count = count /56136)

#-------------------------------------------------------------------------------
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
#-------------------------------------------------------------------------------
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

#-------------------------------------------------------------------------------
## Drug Dependency by age group
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
## Drug Dependency and Race
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
##############
#Mental Health
##############
## Drug Dependency and Degree of Mental illness
Drug.Dependency.MI <- data2019 %>%
  select(depndalc,depndcoc,depndher,MI_CAT_U) %>%
  filter (MI_CAT_U >= 0) %>%
  pivot_longer(cols = c(depndalc, depndcoc,depndher), names_to = "Drug", values_to = "Response") %>%
  filter( Response == 1) 

ggplot(Drug.Dependency.MI, aes( x = factor(MI_CAT_U), fill = factor(Drug)))+
  geom_bar(position = "fill")+
  scale_x_discrete(labels = c("0" = "Keine Mentalen Gesundheitsprobleme" , "1" = "'Milde' Mentale Erkrankung", "2" = " 'Moderate' Mentale Erkrankung", "3" = "Ernste Mentale Erkrankungen"),
                   guide = guide_axis(angle = 45))+
  labs(x = "Art der Erkrankung", y = "Anteil")

##GLeicher Plot, nur achsen vertauscht
ggplot(Drug.Dependency.MI, aes(x = factor(Drug), fill = factor(MI_CAT_U)))+
  geom_bar(position = "fill")+
  scale_x_discrete(labels = c("depndalc" = "Alkohol", "depndcoc" = "Cokain","depndher" = "Heroin"))+
  labs(title = "Substanzkonsum und Mentale Gesundheit", x = " Substanz")+
  scale_fill_discrete(labels = c("0" = "Keine Mentalen Gesundheitsprobleme" , "1" = "'Milde' Mentale Erkrankung", "2" = " 'Moderate' Mentale Erkrankung", "3" = "Ernste Mentale Erkrankungen"))

#-------------------------------------------------------------------------------
## Overall Substance Dependence/Abuse and MI
MH.Substance <- data2019 %>%
  select(smisudpy, amisudpy, lmmisudpy) %>%
  pivot_longer(cols = everything(), names_to = "Type", values_to = "Response") %>%
  filter(Response == 1)%>%
  group_by(Type) %>%
  summarize(count = n()) %>%
  mutate(count = count/56136)

ggplot(MH.Substance, aes(x = Type, y = count))+
  geom_col()+
  scale_x_discrete(labels = c("smisudpy" = "Serious Mental Health", "amisudpy" ="Any Mental Health", "lmmisudpy" = "Light to medium Mental health"),
                   guide = guide_axis(angle = 45))+
  labs(title = "Mental Health Issues and Drug/Alcohol Dependence or Abuse", x = "Degree of Issues", y = "Percentage")

#-------------------------------------------------------------------------------
## Adult Mental Heath / Substance Treatment
## Variable zur Rekodierung: leute die Mh Treatment bekommen haben, Treatment for Drug Use
Adult.Treatment <- data2019 %>%
  select(depndalc, depndcoc, depndher,rcvmhnsptx, rcvsptxnmh, rcvmhasptx) %>%
  pivot_longer(cols = c(depndalc, depndcoc, depndher), names_to = "Drug", values_to = "Answer") %>%
    pivot_longer(cols = c(rcvmhnsptx, rcvsptxnmh, rcvmhasptx), names_to = "Treatment", values_to = "Response") %>%
  filter(Answer == 1, Response == 1)

ggplot(Adult.Treatment, aes(x = factor(Drug), fill = factor(Treatment)))+
  geom_bar(position = "fill") +
  scale_x_discrete(labels = c("depndalc" = "Alkhol","depndcoc" = "Cokain", "depndher" = "Heroin"))+
  labs(title = "SUbstanzkonsum und Art der Behandlung", x = "SUbstanz")

ggplot(Adult.Treatment, aes(x = factor(Treatment), fill = factor(Drug)))+
  geom_bar(position = "fill")+
  scale_x_discrete(labels = c("rcvmhnsptx" = "Nur für MI in Behandlung", "rcvsptxnmh" = "Behandlung für Substanzkonsum", "rcvmhasptx" = "Behandlung für beides"))+
  labs(title = "Behandlungsart und Substanzkonsum", x = "Behandlungsgrund")

#######
#YOUTH
######

Youth.Treatment <-data2019 %>%
  select(ymhnsptx, ysptxnmh, ymhasptx, depndalc, depndcoc, depndher) %>%
  pivot_longer(cols = c(depndalc, depndcoc, depndher), names_to = "Drug", values_to = "Answer") %>%
  pivot_longer(cols = c(ymhnsptx, ysptxnmh, ymhasptx), names_to = "Treatment", values_to = "Response") %>%
  filter(Answer == 1, Response == 1)

ggplot(Youth.Treatment, aes(x = factor(Drug), fill = factor(Treatment)))+
  geom_bar(position = "fill")+
  scale_x_discrete(labels = c("depndalc" = "Alkhol","depndcoc" = "Cokain", "depndher" = "Heroin"))+
  scale_fill_discrete(labels = c("ymhnsptx" = "Behandlung für MI", "ysptxnmh" = "Behandlung für Substanzkonsum", "ymhasptx" = "Behandlung für beides"))
  labs(title = "SUbstanzkonsum und Art der Behandlung YOUTH", x = "SUbstanz")  ## Kein Heroin --> stimmt das?
