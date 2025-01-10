library(ggplot2)
library(tidyverse)

data2019

#----------------------------------------------------------------------------
## Overnight stay for mental/emotional health issues (excluding SUbstance Abuse!)
Hospital.OvernightStay <-data2019 %>%
  select(auinpyr) %>%
  filter(auinpyr < 3) 

ggplot(Hospital.OvernightStay, aes(x = auinpyr))+
  geom_bar()

#-------------------------------------------------------------------------
## did you Need and recieve medical treatment in the last 12 months 
Treatment.recieved <- data2019%>%
  select(auunmtyr) %>%
  filter(auunmtyr < 3) %>%
  group_by(auunmtyr) %>%
  summarise(answer = n()) %>%
  mutate(answer = answer/56136)
  
ggplot(Treatment.recieved, aes(x = auunmtyr, y = answer))+
  geom_col()

# Recoded
recoded.
select(AHMRX2)
#------------------------------------------------------------------------
## Why didn't you receive Treatment?

Treatment.Denial <- data2019 %>%
  select(auuncost,auunnbr, auunjob,auunncov, auunenuf, auunwher, auuncfid, auuncmit) %>%
  pivot_longer(cols = everything(), names_to = "Reason", values_to = "Responded") %>%
  filter(Responded == 1)%>%
  group_by(Reason) %>%
  summarize(count = n()) %>%
  mutate(count = count/ 56136)

ggplot(Treatment.Denial, aes(x = Reason, y = count))+
  geom_col()+
  scale_x_discrete(labels = c("auuncost" = "couldn't afford", "auunnbr" = "opinion of neighbour", "auunjob" ="Effect on Job", "auunncov" = " No Health Insurance Cover", "auunenuf" = "Not enough Insurance Cover", "auuncfid" = "Confidentiality", "auuncmit" = "might be comitted"),
                   guide = guide_axis(angle = 45))+
  labs(title = "Reason for not getting Med", y = "Percentage", x = "Reasons")

##-------------------------------------------------------------------------
## Age destribution of AMI
AMI.Age <- data2019 %>%
  select(catage, AMIYR_U) %>%
  filter(catage > 1 & AMIYR_U > 0) %>%
  group_by(catage) %>%
  summarise(count = n())%>%
  mutate(count = count/56136)

ggplot(AMI.Age, aes(x = factor(catage), y = count, fill = factor(catage)))+
  geom_col()+
  scale_x_discrete(labels = c("2" = "18-25", "3" = "26-34", "4" = "35+"))

#RAUS
## age Destribution AMI, SMI, LMMI

##------------------------------------------------------------------------
# Mental Health and Drugs
#--> compare them with other variables related to SMI/suizide
# only counts people > 17


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

#--------------------------------------------------------------------------
## MDE : Mayor Depressive Episode
pastYear.MDE <- data2019 %>%
  select(AMDEY2_U) %>%
  filter(AMDEY2_U >= 0) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Response") %>%
  group_by(Response)%>%
  summarize(count = n()) %>%
  mutate(count = count /56136)

ggplot (pastYear.MDE, aes(x = factor(Response), y = count))+
  geom_col()+
  scale_x_discrete(labels = c("1" = "Yes", "0" = "No"))+
  labs(title = "Last Year MDE", x = "Response")

MDE.Age <- data2019 %>%
  select(AMDEY2_U, catage) %>%
  filter(catage > 1 & AMDEY2_U == 1) %>%
  group_by(catage) %>%
  summarise(count = n()) %>%
  mutate(count = count/56136)

ggplot(MDE.Age, aes(x = factor(catage), y = count))+
  geom_col()+
  scale_x_discrete(labels = c("2" = "18-25", "3" = "26-34", "4" = "35+"))+
  labs( title = "MDE Age Destribution", x = "Age groups", y = "Percentage")

#--------------------------------------------------------------------------
## YOUTH MENTAL HEALTH
#---------------------------------------

#youth receiving mental health severices or substance use treatmenr at specialty facility

MH.Drugs.Youth <- data2019 %>%
  select(ymhosptx) %>%
  filter(ymhosptx >= 0) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Response") %>%
  group_by(Response) %>%
  summarize(count = n()) %>%
  mutate(count = count/56136)

ggplot(MH.Drugs.Youth, aes(x= factor(Response), y = count))+
  geom_col()+
  scale_x_discrete(labels = c("0" = "No MH & No Sub Trt", "1" = "Mental Health or Substance"))+
  labs(title = "YOuts receiving facility treatment for MH or Substance Use", x = "Response")

## Either or Treatment
Specific.Treatment.Cause <-data2019 %>%
  select(ymhnsptx, ysptxnmh, ymhasptx) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Response") %>%
  filter(Response == 1)%>%
  group_by(Variable)%>%
  summarise(count = n()) %>%
  mutate(count = count/56136)

ggplot(Specific.Treatment.Cause, aes(x = Variable, y = count))+
  geom_col()+
  scale_x_discrete(labels = c("ymhnsptx" = "Mental Health only", "ysptxnmh" = "Only Substance Use", "ymhasptx" = "Both"))

#-------------------------------------------------------------------------
## Youth MDE in the last year
 Youth.MDE <- data2019 %>%
  select(ymdeyr) %>%
  filter(ymdeyr >= 0)%>%
  pivot_longer(cols = everything(), names_to = "Var", values_to = "Response")%>%
  group_by(Response)%>%
  summarise(count = n())%>%
  mutate(count = count/56136)

ggplot(Youth.MDE, aes(x = Response, y = count))+
  geom_col()+
  scale_x_discrete(labels = c("1" = "Yes", "2" = "No"))+
  labs(title = "Youth mith MDE in last Year")

#-------------------------------------------------------------------------
## YOuth with MDE and Substance Abuse
Youth.MDE.Substance <- data2019 %>%
  select(YMDEAUDPY, ymdeimudpy, ymdeudpy) # Variable fehlt