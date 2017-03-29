######### First, I will load the appropriate libraries in #########
library(dplyr)
library(data.table)

###Next, I load my data first (pulling a CSV from my github) ###

####---The data is from the Afrobarometer Household survey (see afrobarometer.org for more info) -----####
AfrobarometerData2015 <- read.csv("https://raw.githubusercontent.com/sayinde/ECOG314/Raw-Data/Raw%20Data/CSVnig_r6_data_2015.csv",header = TRUE,stringsAsFactors = FALSE)

####This  dataset provides household level responses on gender, employment,
#education, Urban/rural geography and reponses to questions about quality of life in Nigeria (2015)####

names(AfrobarometerData2015)

#########  Using data from Afrobarmoter, I am interested in how educational attainment affects informal
#to formal sector employment. Additionally, I am interested in how employment in formal sector vs. informal sector, shows differences in other responses#

#### I selected questions relevant to region (place of living), urban/rural, age, educational attainment, occupation, gender, and a few other questions
# about length of food insecurity, length of time without cash income, frequency without food, how often they've gone without clean water, and how often
# they've gone without food and so on. These questions (about frequency and length of) were based on a yearly basis (e.g. How often in the past year, have you been without food) ########

#############-------Using data from Afrobarometer Data from 2015 round 6-----------##########
### I named my dataset "Working Data 2015" to distinguish it from the larger Afrobarometer Dataset ###

WorkingData2015 <-
  select(
    AfrobarometerData2015,
    URBRUR, #this means URBAN/RURAL
    REGION, #region or province the reponsdent lives in
    AGE = Q1,
    edu_attainment = Q97,
   occupation = Q96A,
    gender = Q101,
   length_withoutfood = Q8A,
   length_withoutcleanh20 = Q8B,
   length_withoutmeds = Q8C, 
   length_withoutfuel = Q8D,
   length_withoutcash = Q8E,
   frequency_sansfood = Q8F
  )

#####---Now, for the informal to formal employment split---#####

# Currently, the respondent's questions to occupation are from a variety of fields,
#from mangerial positions, to clerical work, homemaker, agricultural, etc. the list goes on##

# For the purposes of this research, I want to reclassify these job occupations into two different sectors, "informal employment" and "formal employment"
### Informal employment is classified as unmonitored and un-taxed (lightly defined)
# With this information, I left out some positions (student), and seperated the rest of the employment options into two categories.

#I need to transform these values into 2 seperate categories in the "occupation" row in the Afrobarometer data
#formal sector jobs = 1
#informal sector jobs = 2
str(WorkingData2015$occupation)

#I'm using a filter because I'm only paying attention to the occupations that I'm coding for informal and formal

Occupationfilter <-
  filter(
    WorkingData2015,
    (occupation == 4) |
      (occupation == 6) | (occupation == 7) | (occupation == 9) |
      (occupation == 4) | (occupation == 11) | (occupation == 12) |
      (occupation == 8) | (occupation == 10) | (occupation == 5) | (occupation == 3) | (occupation == 2)
  )

# I made a data frome called Occupation Filter because I didn't want to override my previous dataset, called "Working Data 2015" (personal preference)
## now, with this code (below), I have reclassified certain occupation responses into "informal"-- which equals 2, and "formal" -- which equals 1

Occupationfilter <- Occupationfilter %>%
  mutate(
    occupation =
      ifelse(occupation == 99, NA, occupation),
    occupation =  ifelse(occupation == 4, 2, occupation),
    occupation =  ifelse(occupation == 6, 2, occupation),
    occupation = ifelse(occupation == 7, 2, occupation),
    occupation =  ifelse(occupation == 3, 2, occupation),
    occupation =  ifelse(occupation == 2, 2, occupation),
    occupation = ifelse(occupation == 9, 1, occupation),
    occupation = ifelse(occupation == 11, 1, occupation),
    occupation = ifelse(occupation == 12, 1, occupation),
    occupation =  ifelse(occupation == 8, 1, occupation),
    occupation = ifelse(occupation == 10, 1, occupation),
    occupation =  ifelse(occupation == 5, 1, occupation)
  )

#########recoding the "99's" in the educational attainment column as NAs (because that's what they are)
#also recoding the grading values so that they are a bit more intuitive to the reader
###Meaning >> Instead of "completing secondary school" being equivalent to 5, it'll be equivalent to 12

Occupationfilter <- Occupationfilter %>%
  mutate(
    edu_attainment =
      ifelse(edu_attainment == 99, NA, edu_attainment),
    edu_attainment =  ifelse(edu_attainment == 2, 3, edu_attainment),
    edu_attainment =  ifelse(edu_attainment == 3, 5, edu_attainment),
    edu_attainment =  ifelse(edu_attainment == 4, 8, edu_attainment),
    edu_attainment =  ifelse(edu_attainment == 5, 12, edu_attainment),
    edu_attainment =  ifelse(edu_attainment == 6, 14, edu_attainment),
    edu_attainment =  ifelse(edu_attainment == 7, 15, edu_attainment),
    edu_attainment =  ifelse(edu_attainment == 8, 16, edu_attainment),
    edu_attainment =  ifelse(edu_attainment == 9, 18, edu_attainment)
  )

##### Here, i'm recoding all the "dont knows"/aka 9's as NA's, because it'll make things easier when I'm conducting my analyses ####
Occupationfilter <- Occupationfilter %>%
  mutate(
    length_withoutfood =  ifelse(length_withoutfood == 9, NA, length_withoutfood))

Occupationfilter <- Occupationfilter %>%
  mutate(
    length_withoutcleanh20 =  ifelse(length_withoutcleanh20 == 9, NA, length_withoutcleanh20))

Occupationfilter <- Occupationfilter %>%
  mutate(
    length_withoutmeds =  ifelse(length_withoutmeds == 9, NA, length_withoutmeds))

Occupationfilter <- Occupationfilter %>%
  mutate(
    length_withoutfuel =  ifelse(length_withoutfuel == 9, NA, length_withoutfuel))

Occupationfilter <- Occupationfilter %>%
  mutate(
    length_withoutcash =  ifelse(length_withoutcash == 9, NA, length_withoutcash))


#----------------#################################################################################------------------------#

### Now, for some summary statistics #####

library(ggplot2)

###------ GG PLOT GRAPH 1 -----## 
## Here, I'm grouping informal-formal participation by avg. education
#-- I want to see the distribution of educational attainment by gender and occupation --#

occu_gender <- Occupationfilter %>%
  group_by(occupation, gender) %>% 
  summarise(
    avg_ed = mean(edu_attainment, na.rm = T),
    sd_edu = sd(edu_attainment, na.rm = T),
    count = n()
  ) %>%
mutate(type = ifelse(occupation == 2 & gender == 1, "Informal Male", NA),
       type = ifelse(occupation == 2 & gender == 2 , "Informal Female", type),
       type = ifelse(occupation == 1 & gender == 2, "Formal Female", type),
       type = ifelse(occupation == 1 & gender == 1, "Formal Male", type)
       )

# Error bars represent standard error of the mean
ggplot(occu_gender, aes(x=type, y=avg_ed, fill=as.factor(gender))) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=avg_ed-sd_edu/sqrt(count), ymax=avg_ed+sd_edu/sqrt(count)),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
                labs(title = "Average Education by Employment Type and Gender")+
                  labs(x = "", y="Average Education", fill= "") + 
  guides(fill=FALSE)


####### ----- GGPLOT with just occupation ----###

######### GRAPH 2: What average education look like between the two sectors? ######

occupation_only <- Occupationfilter %>%
  group_by(occupation) %>% 
  summarise(
    avg_ed = mean(edu_attainment, na.rm = T),
    sd_edu = sd(edu_attainment, na.rm = T),
    count = n()
  ) %>%
  mutate(type = ifelse(occupation == 2 , "Informal", NA),
         type = ifelse(occupation == 1, "Formal", type)
  )

# Error bars represent standard error of the mean
ggplot(occupation_only, aes(x=type, y=avg_ed, fill=as.factor(type))) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=avg_ed-sd_edu/sqrt(count), ymax=avg_ed+sd_edu/sqrt(count)),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  labs(title = "Average Education by Employment Type")+
  labs(x = "", y="Average Education", fill= "") + 
  guides(fill=FALSE)



######------------GRAPH 3: What happens when we add age, coupled with gender + occupation? ------------######
# I'm interested in seeing average age among two sectors #

Occup_gen_age <- Occupationfilter %>%
  group_by(occupation, gender) %>% 
  summarise(
    avg_age = mean(AGE, na.rm = T),
    sd_age = sd(AGE, na.rm = T),
    count = n()
  ) %>%
  mutate(type = ifelse(occupation == 2 & gender == 1, "Informal Male", NA),
         type = ifelse(occupation == 2 & gender == 2 , "Informal Female", type),
         type = ifelse(occupation == 1 & gender == 2, "Formal Female", type),
         type = ifelse(occupation == 1 & gender == 1, "Formal Male", type)
  )

# Error bars represent standard error of the mean
ggplot(Occup_gen_age, aes(x=type, y=avg_age, fill=as.factor(gender))) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=avg_age-sd_age/sqrt(count), ymax=avg_age+sd_age/sqrt(count)),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  labs(title = "Average Age by Employment Type")+
  labs(x = "", y="Average Age", fill= "") + 
  guides(fill=FALSE)


############## ------ GRAPH 4: FOOD INSECURITY --- #########

Times_sansFood <- Occupationfilter %>%
  group_by(occupation, gender) %>% 
  summarise(
    avg_lfood = mean(length_withoutfood, na.rm = T),
    sd_lfood = sd(length_withoutfood, na.rm = T),
    count = n()
  ) %>%
  mutate(type = ifelse(occupation == 2 & gender == 1, "Informal Male", NA),
         type = ifelse(occupation == 2 & gender == 2 , "Informal Female", type),
         type = ifelse(occupation == 1 & gender == 2, "Formal Female", type),
         type = ifelse(occupation == 1 & gender == 1, "Formal Male", type)
  )
# Error bars represent standard error of the mean
ggplot(Times_sansFood, aes(x=type, y=avg_lfood, fill=as.factor(gender))) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=avg_lfood-sd_lfood/sqrt(count), ymax=avg_lfood+sd_lfood/sqrt(count)),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  labs(title = "Food Insecurity by Employment Type")+
  labs(x = "", y="Average Age", fill= "") + 
  guides(fill=FALSE)


##########-------------------GRAPHS DONE --------------------##########

#now I want to see this across different regions + urban/rural differences
#please see longer codebook (word document) for region names, etc


Occupationfilter %>%
  group_by(occupation, gender, REGION, URBRUR) %>% 
  summarise(
    avg_ed = mean(edu_attainment, na.rm = T),
    sd_edu = sd(edu_attainment),
    count = n()
  )

summary(Occupationfilter)

##################################################

# FEEL FREE TO IGNORE THIS LINE BELOW. I was just messing around with these graphs #

Age_Employ <- ggplot(Occupationfilter,aes(AGE, occupation,color=as.factor(gender)))+
  geom_point()

Age_Employ

qplot(Occupationfilter$AGE,
      geom="histogram",
      binwidth = 5,
      main = "Histogram for Age",
      xlab = "Age",
      fill=I("blue"),
      col=I("red"),
      alpha=I(.2))

ggplot(data = Occupationfilter, aes(x=Occupationfilter$AGE)) +
  geom_histogram(breaks = seq(20, 50),
                 col = "red",
                 aes(fill = ..count..) )+
                labs(title = "Histogram for Age")+
                 labs(x = "Age", y="Count")
