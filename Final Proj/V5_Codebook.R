######### First, I will load the appropriate libraries in #########
library(dplyr)
library(data.table)
library(ggplot2)
library(mfx)
library(texreg)
library(stargazer)

###Next, I load my data first (pulling a CSV from my github) ###

####---The data is from the Afrobarometer Household survey (see afrobarometer.org for more info) -----####
AfrobarometerData2015q <- read.csv("https://raw.githubusercontent.com/sayinde/ECOG314/Raw-Data/Raw%20Data/CSVnig_r6_data_2015.csv",header = TRUE,stringsAsFactors = FALSE)

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

WorkingData20151 <-
  select(
    AfrobarometerData2015q,
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
#informal sector jobs = 0
str(WorkingData2015q$occupation)

#I'm using a filter because I'm only paying attention to the occupations that I'm coding for informal and formal

WorkingData2015q <-
  filter(
    WorkingData20151,
    (occupation == 4) |
      (occupation == 6) | (occupation == 7) | (occupation == 9) |
      (occupation == 4) | (occupation == 11) | (occupation == 12) |
      (occupation == 8) | (occupation == 10) | (occupation == 5) | (occupation == 2) | (occupation == 3)
  )

# I made a data frome called Occupation Filter because I didn't want to override my previous dataset, called "Working Data 2015" (personal preference)
## now, with this code (below), I have reclassified certain occupation responses into "informal"-- which equals 0, and "formal" -- which equals 1

WorkingData2015q <- WorkingData2015q %>%
  mutate(
    occupation =
      ifelse(occupation == 99, NA, occupation),
    occupation =  ifelse(occupation == 4, 0, occupation),
    occupation =  ifelse(occupation == 6, 0, occupation),
    occupation = ifelse(occupation == 7, 1, occupation),
    occupation =  ifelse(occupation == 2, 0, occupation),
    occupation =  ifelse(occupation == 3, 0, occupation),
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

WorkingData2015q <- WorkingData2015q %>%
  mutate( edu_attainment =  ifelse(edu_attainment == 9, 18, edu_attainment),
          edu_attainment =  ifelse(edu_attainment == 6, 14, edu_attainment),
          edu_attainment =  ifelse(edu_attainment == 7, 15, edu_attainment),
          edu_attainment =  ifelse(edu_attainment == 8, 16, edu_attainment),
          edu_attainment =  ifelse(edu_attainment == 5, 12, edu_attainment),
          edu_attainment = ifelse(edu_attainment == 99, NA, edu_attainment),
          edu_attainment =  ifelse(edu_attainment == 4, 8, edu_attainment),
          edu_attainment =  ifelse(edu_attainment == 3, 5, edu_attainment),
          edu_attainment =  ifelse(edu_attainment == 2, 3, edu_attainment))


unique(WorkingData2015q$edu_attainment)

Occupationfilterq <- WorkingData2015q

##### Here, i'm recoding all the "dont knows"/aka 9's as NA's, because it'll make things easier when I'm conducting my analyses ####
Occupationfilterq <- Occupationfilterq %>%
  mutate(
    length_withoutfood =  ifelse(length_withoutfood == 9, NA, length_withoutfood))

Occupationfilterq <- Occupationfilterq %>%
  mutate(
    length_withoutcleanh20 =  ifelse(length_withoutcleanh20 == 9, NA, length_withoutcleanh20))

Occupationfilterq <- Occupationfilterq %>%
  mutate(
    length_withoutmeds =  ifelse(length_withoutmeds == 9, NA, length_withoutmeds))

Occupationfilterq <- Occupationfilterq %>%
  mutate(
    length_withoutfuel =  ifelse(length_withoutfuel == 9, NA, length_withoutfuel))

Occupationfilterq <- Occupationfilterq %>%
  mutate(
    length_withoutcash =  ifelse(length_withoutcash == 9, NA, length_withoutcash))


Occupationfilterq <- Occupationfilterq %>%
  mutate(
    gender = ifelse(gender == 2, 0, gender),
    URBRUR = ifelse(URBRUR == 2, 0, URBRUR))

# 1 is male
# 0 is female
# 1 is urban
# 0 is rural


##### made a new dataset without NA values
OccupationNEWfilter  = Occupationfilterq[complete.cases(Occupationfilterq),]


#----------------###################################################--------------------####
####                             END OF CLEANING                                        ####
####                                                                                    ####
####-------------###################################################--------------------####
                
                        #### Now, for some summary statistics #####

### statistics about percentages of informal/formal based on years of schooling 
edu_work <- WorkingData2015q %>%
  group_by(occupation, edu_attainment) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))%>%
  mutate(type = ifelse(occupation == 0, "Informal", NA),
         type = ifelse(occupation == 1, "Formal", type))

##### general summary table
summary(Occupationfilterq)

##### GGPLOT graphs 1-3, below provide a better view of summary statistics for the data set #######

##########################################################################################
###                             GG PLOT GRAPHS                                        ####
###                                                                                   ####
##########################################################################################

###------ GG PLOT GRAPH 1 -----## 
## Here, I'm grouping informal-formal participation by avg. education
#-- I want to see the distribution of educational attainment by gender and occupation --#

occu_gender <- Occupationfilterq %>%
  group_by(occupation, gender) %>% 
  summarise(
    avg_edu = mean(edu_attainment, na.rm = T),
    sd_edu = sd(edu_attainment, na.rm = T),
    min_edu = min(edu_attainment, na.rm = T),
    max_edu = max(edu_attainment,na.rm = T),
    count = n()
  ) %>%
  mutate(type = ifelse(occupation == 0 & gender == 1, "Informal Male", NA),
         type = ifelse(occupation == 0 & gender == 0 , "Informal Female", type),
         type = ifelse(occupation == 1 & gender == 0, "Formal Female", type),
         type = ifelse(occupation == 1 & gender == 1, "Formal Male", type)
  )

# Error bars represent standard error of the mean
occu_gendergg <- ggplot(occu_gender, aes(x=type, y=avg_edu, fill=as.factor(gender))) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=avg_edu-sd_edu/sqrt(count), ymax=avg_edu+sd_edu/sqrt(count)),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  labs(title = "Average Education by Employment Type and Gender")+
  labs(x = "", y="Average Education", fill= "") + 
  guides(fill=FALSE) 

occu_gendergg + theme(axis.text=element_text(size=12),
                      axis.title=element_text(face="bold"))

####### ----- GGPLOT with just occupation ----###

######### GRAPH 2: What average education look like between the two sectors? ######

occupation_only <- Occupationfilterq %>%
  group_by(occupation) %>% 
  summarise(
    avg_edu = mean(edu_attainment, na.rm = T),
    sd_edu = sd(edu_attainment, na.rm = T),
    min_edu = min(edu_attainment, na.rm = T),
    max_edu = max(edu_attainment,na.rm = T),
    count = n()
  ) %>%
  mutate(type = ifelse(occupation == 0 , "Informal", NA),
         type = ifelse(occupation == 1, "Formal", type)
  )

# Error bars represent standard error of the mean
avg_edgg <- ggplot(occupation_only, aes(x=type, y=avg_edu, fill=as.factor(type))) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=avg_edu-sd_edu/sqrt(count), ymax=avg_edu+sd_edu/sqrt(count)),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  labs(title = "Average Education by Employment Type")+
  labs(x = "", y="Average Education", fill= "") + 
  guides(fill=FALSE) 

avg_edgg + theme(axis.text=element_text(size=12),
        axis.title=element_text(face="bold"))



######------------GRAPH 3: What happens when we add age, coupled with gender + occupation? ------------######
# I'm interested in seeing average age among two sectors #

Occup_gen_age <- Occupationfilterq %>%
  group_by(occupation, gender) %>% 
  summarise(
    avg_age = mean(AGE, na.rm = T),
    sd_age = sd(AGE, na.rm = T),
    min_age = min(AGE, na.rm = T),
    max_age = max(AGE,na.rm = T),
    count = n()
  ) %>%
  mutate(type = ifelse(occupation == 0 & gender == 1, "Informal Male", NA),
         type = ifelse(occupation == 0 & gender == 0 , "Informal Female", type),
         type = ifelse(occupation == 1 & gender == 0, "Formal Female", type),
         type = ifelse(occupation == 1 & gender == 1, "Formal Male", type)
  )

# Error bars represent standard error of the mean
avg_agegg <- ggplot(Occup_gen_age, aes(x=type, y=avg_age, fill=as.factor(gender))) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=avg_age-sd_age/sqrt(count), ymax=avg_age+sd_age/sqrt(count)),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  labs(title = "Average Age by Employment Type")+
  labs(x = "", y="Average Age", fill= "") + 
  guides(fill=FALSE)

avg_agegg + theme(axis.text=element_text(size=12),
                  axis.title=element_text(face="bold"))


############## ------------- GRAPH 4: Food Insecurity---------------- #####################

Food_insecure  <- OccupationNEWfilter %>%
  group_by(occupation, length_withoutfood) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  mutate(type = ifelse(occupation == 0, "Informal", NA),
         type = ifelse(occupation == 1, "Formal", type)
  )

 Fsecurity_ggplot <- ggplot(workpercent, aes(x=length_withoutfood,
                             y=freq,
                             fill=as.factor(type)
)) + 
  geom_histogram(position=position_dodge(),
                 stat = "identity") +
  labs(title = "Food Insecurity by Employment Type")+
  labs(x = "Frequency without Food (annual)", y="Percentage of sector", fill= "Employment Types") +
  scale_fill_manual(values=c("#79c36a", "#599ad3"))

 
 Fsecurity_ggplot + theme(axis.text=element_text(size=12),
         axis.title=element_text(face="bold"))
 
 ############## ------------- GRAPH 5: Cash Volatility---------------- #####################
 
 cash_volatile <- OccupationNEWfilter %>%
   group_by(occupation, length_withoutcash) %>%
   summarise (n = n()) %>%
   mutate(freq = n / sum(n)) %>%
   mutate(type = ifelse(occupation == 0, "Informal", NA),
          type = ifelse(occupation == 1, "Formal", type)
   )
 
 Cvolatileggplot <- ggplot(cash_volatile, aes(x=length_withoutcash, y=freq, fill=as.factor(type))) + 
   geom_histogram(position=position_dodge(),
                  stat = "identity")+
   labs(title = "Cash Volitality by Employment Type")+
   labs(x = "Frequency without Cash (annual)", y="Percentage of sector", fill= "Employment Types") +
   scale_fill_manual(values=c("#79c36a", "#599ad3"))
 
 
 Cvolatileggplot + theme(axis.text=element_text(size=12),
         axis.title=element_text(face="bold")) +
   xlim(-.5,4.5)
 
 ############## ------------- GRAPH 5: Medicine inconsistency --------------- #####################
 
 meds <- OccupationNEWfilter %>%
   group_by(occupation, length_withoutmeds) %>%
   summarise (n = n()) %>%
   mutate(freq = n / sum(n)) %>%
   mutate(type = ifelse(occupation == 0, "Informal", NA),
          type = ifelse(occupation == 1, "Formal", type)
   )
 
 medsggplot <- ggplot(meds, aes(x=length_withoutmeds, y=freq, fill=as.factor(type))
) + 
   geom_histogram(position=position_dodge(),
                  stat = "identity")+
   labs(title = "Inconsistency of Medicine by Employment Type")+
   labs(x = "Frequency without Medicine (annual)", y="Percentage of sector", fill= "Employment Types") +
   scale_fill_manual(values=c("#79c36a", "#599ad3"))
 
 medsggplot + theme(axis.text=element_text(size=12),
                    axis.title=element_text(face="bold")) +
   xlim(-.5,4.5)
 
 ############## ------------- GRAPH 6: Fuel Inconsistency --------------- #####################
 
 fuel <- OccupationNEWfilter %>%
   group_by(occupation, length_withoutfuel) %>%
   summarise (n = n()) %>%
   mutate(freq = n / sum(n)) %>%
   mutate(type = ifelse(occupation == 0, "Informal", NA),
          type = ifelse(occupation == 1, "Formal", type)
   )
 
 fuelggplot <- ggplot(fuel, aes(x=length_withoutfuel, y=freq, fill=as.factor(type))) + 
   geom_histogram(position=position_dodge(),
                  stat = "identity")+
   labs(title = "Inconsistency of Cooking Fuel by Employment Type")+
   labs(x = "Frequency without Cooking Fuel (annual)", y="Percentage of sector", fill= "Employment Types") +
   scale_fill_manual(values=c("#79c36a", "#599ad3"))
 
 fuelggplot +
   xlim(-.5,4.5)
 
 
 ############## ------------- GRAPH 7: Clean Water Consistency --------------- #####################
 
 h20 <- OccupationNEWfilter %>%
   group_by(occupation, length_withoutcleanh20) %>%
   summarise (n = n()) %>%
   mutate(freq = n / sum(n)) %>%
   mutate(type = ifelse(occupation == 0, "Informal", NA),
          type = ifelse(occupation == 1, "Formal", type)
   )
 
 h20ggplot <- ggplot(h20, aes(x=length_withoutcleanh20, y=freq, fill=as.factor(type))) + 
   geom_histogram(position=position_dodge(),
                  stat = "identity")+
   labs(title = "Inconsistency of Clean Water by Employment Type")+
   labs(x = "Frequency without Clean Water (annual)", y="Percentage of sector", fill= "Employment Types") +
   scale_fill_manual(values=c("#79c36a", "#599ad3"))
 
 
h20ggplot +
   xlim(-.5,4.5)
 
#################-------------------GRAPHS DONE --------------------#########################
 
############################################################################################



##############################################################################################
######                           REGRESSIONS                                           #######
#####                                                                                   ######
##############################################################################################


########--------------------------- LOGIT MODEL REGRESSIONS --------------------------########
##############################################################################################

### I will be performing a logit model regression because my outcome/dependent variable 
### (which will be occupation) is binary. Because I am using a logit model regression model,
### I will have to seperate the educational variable into binary code, by seperating the categories
### based on years of schooling.


###########       Step 1: Transforming education values into binary code     #########

## primary school completed
OccupationNEWfilter <- mutate(OccupationNEWfilter,
                              Educ1 = edu_attainment)
OccupationNEWfilter <- mutate(OccupationNEWfilter,
                              Educ1 =  ifelse(edu_attainment < 5, 0, 1))

### greater than primary school but less than high school
OccupationNEWfilter <- mutate(OccupationNEWfilter,
                              Educ2 = edu_attainment)
OccupationNEWfilter <- mutate(OccupationNEWfilter,
                              Educ2 =  ifelse(edu_attainment < 9, 0, 1))

### college completed
OccupationNEWfilter <- mutate(OccupationNEWfilter,
                              Educ3 = edu_attainment)
OccupationNEWfilter <- mutate(OccupationNEWfilter,
                              Educ3 =  ifelse(edu_attainment < 16, 0, 1))

### post-graduate
OccupationNEWfilter <- mutate(OccupationNEWfilter,
                              Educ4 = edu_attainment)
OccupationNEWfilter <- mutate(OccupationNEWfilter,
                              Educ4 =  ifelse(edu_attainment < 18, 0, 1))


###########       Step 2: Transforming AGE values into binary code     ###########


## Those greater than 25
OccupationNEWfilter <- mutate(OccupationNEWfilter,
                              Age1 = AGE)
OccupationNEWfilter <- mutate(OccupationNEWfilter,
                              Age1 =  ifelse(AGE < 25, 0, 1))

## Those greater than 33
## The number 33 was chosen because this was the mean value in the age category
## median(OccupationNEWfilter$AGE)
OccupationNEWfilter <- mutate(OccupationNEWfilter,
                              Age2 = AGE)
OccupationNEWfilter <- mutate(OccupationNEWfilter,
                              Age2 =  ifelse(AGE < 33, 0, 1))

## Those greater than 45
OccupationNEWfilter <- mutate(OccupationNEWfilter,
                              Age3 = AGE)
OccupationNEWfilter <- mutate(OccupationNEWfilter,
                              Age3 =  ifelse(AGE < 45, 0, 1))


########---------------------------  REGRESSION TIME----------------------------------########
##############################################################################################

### regression 1: Gender as an independent variable, and occupation as a dependent variable
regression1 <- logitmfx(occupation ~ gender,
         data = OccupationNEWfilter)

## regression 2: Age(s) as independent vaiables, and occupation as a dependent variable

regression2 <- logitmfx(occupation ~ gender + Age1 + Age2 + Age3,
                        data = OccupationNEWfilter)

## regression 3: Education levels as independent variables, and occupation as a dependent variable

regression3 <-logitmfx(occupation ~ gender + Age1 + Age2 + Age3 +  Educ1 + Educ2 + Educ3 + Educ4,
                       data = OccupationNEWfilter)

## regression 4: Urban/rural location as an independent variable, and occupation as a dependent variable
regression4 <- logitmfx(occupation ~ gender + Age1 + Age2 + Age3 +  Educ1 + Educ2 + Educ3 + Educ4 + URBRUR,
                        data = OccupationNEWfilter)

########-------------------------------------------------#########


texreg(regression1)
texreg(regression2)
texreg(regression3)
texreg(regression4)


