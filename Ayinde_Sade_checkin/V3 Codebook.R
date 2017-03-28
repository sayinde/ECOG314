######### First, I will load the appropriate libraries in #########
library(dplyr)
library(data.table)

###Next, I load my data first (pulling a CSV from my github###

####---The data is from the Afrobarometer Household survey (see afrobarometer.org for more info) -----####
AfrobarometerData2015 <- read.csv("https://raw.githubusercontent.com/sayinde/ECOG314/Raw-Data/Raw%20Data/CSVnig_r6_data_2015.csv",header = TRUE,stringsAsFactors = FALSE)

####This  dataset provides household level responses on gender, employment, education, Urban/rural geography and reponses to questions about quality of life####

names(AfrobarometerData2015)

#########  Using data from Afrobarmoter, I am interested in how educational attainment affects informal
#to formal sector employment. Additionally, I am interested in how employment in formal sector vs. informal sector, shows differences in other responses#

#### I choose to filter out questions relevant to region (place of living), urban/rural, age, educational attainment, occupation, gender, and a few questions
# about length of food insecurity, length of time without cash income, frequency without food, how often they've gone without clean water, and how often
# they've gone without food and so on. These questions (about frequency and length of) were asking on a yearly basis ########

#############-------Afrobarometer Data from 2015 round 6-----------##########
### I named my dataset "Working Data 2015" to distinguish it from the larger Afrobarometer Dataset ###

WorkingData2015 <-
  select(
    AfrobarometerData2015,
#    RespondentNum = RESPNO,
    URBRUR,
    REGION,
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
# Currently, the respondent's questions to occupation are from a variety of fields, from mangerial positions, to clerical work, homemaker, agricultural, etc. the list goes on##
# For the purposes of this research, I want to reclassify these job occupations into two different sectors, "informal employment" and "formal employment"
### Informal employment is classified as non-agricultural based positions, typically low-skilled, and more specifically, unmonitored and un-taxed (lightly defined)
# With this information, I left out some positions (student, agriculture), and seperated the rest into two categories.

#I need to transform these values into 2 seperate categories in the "occupation" row in the Afrobarometer data
#formal sector jobs = 1
#informal sector jobs = 2
str(WorkingData2015$occupation)

#I'm using a filter because I only "care" about the occupations that I'm coding for informal and formal

Occupationfilter <-
  filter(
    WorkingData2015,
    (occupation == 4) |
      (occupation == 6) | (occupation == 7) | (occupation == 9) |
      (occupation == 4) | (occupation == 11) | (occupation == 12) |
      (occupation == 8) | (occupation == 10) | (occupation == 5)
  )

# I made a data frome called Occupation Filter because I didn't want to override my previous dataset, "Working Data 2015" (personal preference)
## now, with this code, I have reclassified certain occupation responses into "informal"-- which equals 1, and "formal" -- which equals 2
Occupationfilter <- Occupationfilter %>%
  mutate(occupation =
           ifelse(occupation == 99, NA, occupation),
         occupation =  ifelse(occupation == 4, 1, occupation),
         occupation =  ifelse(occupation == 6, 1, occupation),
         occupation = ifelse(occupation == 7, 1, occupation),
         occupation = ifelse(occupation == 9, 2, occupation),
         occupation = ifelse(occupation == 11, 2, occupation),
         occupation = ifelse(occupation == 12, 2, occupation),
         occupation =  ifelse(occupation == 8, 2, occupation),
         occupation = ifelse(occupation == 10, 2, occupation),
         occupation =  ifelse(occupation == 5, 2, occupation)
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

### Now, I'm just checking out my data #####

## Here, I'm grouping informal-formal participation by avg. education

Occupationfilter %>%
  group_by(occupation, gender) %>% 
  summarise(
    avg_ed = mean(edu_attainment, na.rm = T),
    sd_edu = sd(edu_attainment),
    count = n()
  )

#fyi 1 = male, 2 = female
#reminder 1 = formal 2 = informal

### What happens when we add age, coupled with gender + occupation?

Occupationfilter %>%
  group_by(occupation, gender, AGE) %>% 
  summarise(
    avg_ed = mean(edu_attainment, na.rm = T),
    sd_edu = sd(edu_attainment),
    count = n()
  )

##########---------------------------------------##########

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

names(Occupationfilter)

hist(
  log(Occupationfilter$edu_attainment),
  breaks = 10,
  main = "Histogram of Educational Attainment",
  xlab = "Educational level",
  col = "green",
  xlim=c(0,6)
)

#save as a reference
#https://www.datacamp.com/community/tutorials/15-questions-about-r-plots#q6

boxplot(URBRUR ~ edu_attainment,
        data = Occupationfilter,
        main = "Educational Attainment split by Urban Rural",
        xlab = "Educational Attainment",
        ylab = "Urban/Rural")

boxplot(REGION ~ occupation,
        data = Occupationfilter,
        main = "Informal-Formal participation by region",
        xlab = "Occupation",
        ylab = "Region")

library(ggplot2)

edu_employ <- ggplot(Occupationfilter,aes(occupation,edu_attainment,color=as.factor(gender)))+
geom_point()

edu_employ

#reminder/look @ word doc codebook
#1 is male
# 2 is gender
#1 is formal
#2 is informal

#plot.data1 <- Occupationfilter %>% mutate(value = as.numeric(AGE))
#ignore prev. line^

Age_Employ <- ggplot(Occupationfilter,aes(AGE, occupation,color=as.factor(gender)))+
  geom_point()

Age_Employ


Agehist <- ggplot(data=Occupationfilter, aes(Occupationfilter$AGE)) + geom_histogram()

Agehist


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

#ggplot(Occupationfilter, aes(Occupationfilter$edu_attainment, fill = Occupationfilter$occupation)) + 
#  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')
