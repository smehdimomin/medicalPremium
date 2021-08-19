#install.packages("tidyverse")
#install.packages("GGally")
#install.packages(hrbrthemes)


library(tidyverse)
library(GGally)
library(hrbrthemes)

data <- read.csv("Medicalpremium.csv")
head(data)
str(data)

#applying factors to different colums
data$Diabetes <- as.factor(data$Diabetes)
data$BloodPressureProblems <- as.factor(data$BloodPressureProblems)
data$AnyTransplants <- as.factor(data$AnyTransplants)
data$AnyChronicDiseases <- as.factor(data$AnyChronicDiseases)
data$KnownAllergies <- as.factor(data$KnownAllergies)
data$HistoryOfCancerInFamily <- as.factor(data$HistoryOfCancerInFamily)
data$NumberOfMajorSurgeries <- as.factor(data$NumberOfMajorSurgeries)

str(data)

data$bmi <- 10000*(data$Weight/(data$Height)^2)

data <- data %>%
  mutate( bmiCategory = case_when(
                                  bmi<18.49999 ~ "under weight",
                                  bmi>18.5 & bmi<24.99999 ~ "normal weight",
                                  bmi>25 & bmi<29.99999 ~ "over weight",
                                  bmi>30 ~ "obesity"
  ))

data$bmiCategory <- as.factor(data$bmiCategory)

#correlation matrix

ggcorr(data, label = T, color = "black", size = 5)+
  labs(title = "Correlation Matrix")+
  theme(plot.title = element_text(family = "Roboto Condensed", size = 19, face = "bold",vjust = 0),
        plot.subtitle = element_text(family = "Roboto Condensed", size = 16,vjust = 0))

#difference in premium price for diabetics people

data %>% 
  select(Diabetes,PremiumPrice) %>%
  group_by(Diabetes) %>% 
  summarise( PremiumPrice = mean(PremiumPrice)) %>% 
  ggplot(.,aes(Diabetes,PremiumPrice))+
  geom_bar(stat = "identity",width = 0.2)+
  labs(title = "Bar plot for Diabetics People")
  
#distribution of premium for diabetics vs non diabetics
ggplot(data, aes(PremiumPrice))+
  geom_density(aes(fill = Diabetes), color = NA, alpha = 0.6)+
  labs(title = "Density plot for Diabetics and Non-diabetic proples")

#difference in premium price for blood pressure problem

data %>% 
  select(BloodPressureProblems,PremiumPrice) %>%
  group_by(BloodPressureProblems) %>% 
  summarise( PremiumPrice = mean(PremiumPrice)) %>% 
  ggplot(.,aes(BloodPressureProblems,PremiumPrice))+
  geom_bar(stat = "identity",width = 0.2)+
  labs(title = "Bar plot for blood pressure problem")
#distribution of premium for people with and without blood pressure problems
ggplot(data, aes(PremiumPrice))+
  geom_density(aes(fill = BloodPressureProblems), color = NA, alpha = 0.6)+
  labs(title = "Density plot for people with and without blood pressure problems")


#difference in premium price for people gone through any transplants

data %>% 
  select(AnyTransplants,PremiumPrice) %>%
  group_by(AnyTransplants) %>% 
  summarise( PremiumPrice = mean(PremiumPrice)) %>% 
  ggplot(.,aes(AnyTransplants,PremiumPrice))+
  geom_bar(stat = "identity",width = 0.2)+
  labs(title = "Bar plot for people gone through any transplants")

#distribution of premium for people gone through any transpalnts
ggplot(data, aes(PremiumPrice))+
  geom_density(aes(fill = AnyTransplants), color = NA, alpha = 0.6)+
  labs(title = "Density plot for people gone through any transpalnts")



#difference in premium price for people with cronic disease

data %>% 
  select(AnyChronicDiseases,PremiumPrice) %>%
  group_by(AnyChronicDiseases) %>% 
  summarise( PremiumPrice = mean(PremiumPrice)) %>% 
  ggplot(.,aes(AnyChronicDiseases,PremiumPrice))+
  geom_bar(stat = "identity",width = 0.2)+
  labs(title = "Bar plot for people with cronic disease")

#distribution of premium for having chronic diseases
ggplot(data, aes(PremiumPrice))+
  geom_density(aes(fill = AnyChronicDiseases), color = NA, alpha = 0.6)+
  labs(title = "Density plot for  having chronic diseases")




#difference in premium price for people having allergies

data %>% 
  select(KnownAllergies,PremiumPrice) %>%
  group_by(KnownAllergies) %>% 
  summarise( PremiumPrice = mean(PremiumPrice)) %>% 
  ggplot(.,aes(KnownAllergies,PremiumPrice))+
  geom_bar(stat = "identity",width = 0.2)+
  labs(title = "Bar plot for people having allergies")

#distribution of premium for people with and without allergies
ggplot(data, aes(PremiumPrice))+
  geom_density(aes(fill = KnownAllergies), color = NA, alpha = 0.6)+
  labs(title = "Density plot for people with and without allergies")




#difference in premium price for people with history of cancers

data %>% 
  select(HistoryOfCancerInFamily,PremiumPrice) %>%
  group_by(HistoryOfCancerInFamily) %>% 
  summarise( PremiumPrice = mean(PremiumPrice)) %>% 
  ggplot(.,aes(HistoryOfCancerInFamily,PremiumPrice))+
  geom_bar(stat = "identity",width = 0.2)+
  labs(title = "Bar plot for people with history of cancers")

#distribution of premium for people with and without history of cancers
ggplot(data, aes(PremiumPrice))+
  geom_density(aes(fill = HistoryOfCancerInFamily), color = NA, alpha = 0.6)+
  labs(title = "Density plot for people with and without history of cancers")



#difference in premium price for major surgeries

data %>% 
  select(NumberOfMajorSurgeries,PremiumPrice) %>%
  group_by(NumberOfMajorSurgeries) %>% 
  summarise( PremiumPrice = mean(PremiumPrice)) %>% 
  ggplot(.,aes(NumberOfMajorSurgeries,PremiumPrice))+
  geom_bar(stat = "identity",width = 0.2)+
  labs(title = "Bar plot for major surgeries")

#distribution of premium for people with differnt number of surgeries
#people with 4 are charged constant 28000 hence i have neglected thet
ggplot(data %>% 
         select(NumberOfMajorSurgeries,PremiumPrice) %>%
         filter(!NumberOfMajorSurgeries == 3),
         aes(PremiumPrice))+
  geom_density(aes(fill = NumberOfMajorSurgeries), color = NA, alpha = 0.6)+
  labs(title = "Density plot for people with differnt number of surgeries")



#box-plot based on bmi category and premium price


data %>% 
  mutate(bmiCategory = str_to_title(bmiCategory)) %>% 
  
  ggplot(aes(bmiCategory, PremiumPrice))+
  geom_boxplot()+
  geom_jitter(aes(color = bmiCategory),alpha = 0.4)+
  labs(title = "Distribution of Premium Price per BMI category")+
  theme_ipsum_rc()



