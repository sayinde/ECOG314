#project proposal part 2
#	Mar. 3rd, Code for loading/cleaning/validation – 5 points

library(dplyr)
library(data.table)

# then I read it in as a data frame to make my life easier (just alittle)
#load all data first

#nolonger need these datasets (at this moment)
#AfrobarometerData2012 <- read.csv("https://raw.githubusercontent.com/sayinde/ECOG314/Raw-Data/Raw%20Data/CSVnig_r5_data_2012.csv",header = TRUE,stringsAsFactors = FALSE)
#REGIONFERTCHILD <- read.csv("https://raw.githubusercontent.com/sayinde/ECOG314/Raw-Data/Raw%20Data/NigeriaWvaluesdata2011.csv",head=TRUE,stringsAsFactors = FALSE)

AfrobarometerData2015 <- read.csv("https://raw.githubusercontent.com/sayinde/ECOG314/Raw-Data/Raw%20Data/CSVnig_r6_data_2015.csv",header = TRUE,stringsAsFactors = FALSE)

names(AfrobarometerData2015)

# Afrobarometer Data from 2015 round 6

WorkingData2015 <-
  select(
    AfrobarometerData2015,
    RespondentNum = RESPNO,
    URBRUR,
    REGION,
    respondentAGE = Q1,
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


# I need to revalue between informal and "formal"

#is_formal = ifelse(WorkingData2015$occupation %in% c(4,6,7,3,2), 1,0)
#is_informal = ifelse(WorkingData2015$occupation %in% c(9,11,12,8,10,5), 1,0)

# I need to transform these values into 2 seperate categories in the "occupation" row
#formal sector jobs = 1
#informal sector jobs = 2

#hey simeon, thoughts on this? I believe I did this correctly, 
#but what to do with the "NA" values.. drop them?

str(WorkingData2015$occupation)

WorkingData2015$occupation <- as.numeric((
  dplyr::recode(
    (WorkingData2015$occupation),
    "4" = "1",
    "6" = "1",
    "7" = "1",
    "3" = "1",
    "9" = "2",
    "11" = "2",
    "12" = "2",
    "8" = "2",
    "10" = "2",
    "5" = "2"
  )
))


#Afrobarometer Data from 2012 round 5
#WorkingData2012 <-
#  select(
#    AfrobarometerData2012,
#    RespondentNum = RESPNO,
#    URBRUR,
# respondentAGE = Q1,
  #  edu_attainment = Q97,
  #  occupation = Q96A,
  #  gender = Q101,
  #  REGION
#  )

# Editing World Values Data about Fertility and Marital Status (2010-2014)

#REGIONFERTCHILD <-
#  select(
#    REGIONFERTCHILD,
#    marital_stat = V57,
#    num_children = V58,
#    region = V256B
#  )

#merging the 2 dataframes fyi is <-rbind()

var(WorkingData2015$length_withoutmeds)
range(WorkingData2015$length_withoutmeds)
summary(WorkingData2015$edu_attainment)

##################################################


names(WorkingData2015)

hist(
  log(WorkingData2015$edu_attainment),
  breaks = 15,
  main = "Histogram of Educational Attainment",
  xlab = "Educational level",
  col = "green",
  xlim=c(0,6)
)

#save as a reference
#https://www.datacamp.com/community/tutorials/15-questions-about-r-plots#q6

boxplot(URBRUR ~ edu_attainment,
        data = WorkingData2015,
        main = "Educational Attainment split by Urban Rural",
        xlab = "Educational Attainment",
        ylab = "Urban/Rural")

plot(WorkingData2015$edu_attainment,
     WorkingData2015$URBRUR,
     main = "Educational Attainment split by Urban Rural",
     xlab = "Educational Attainment",
     ylab = "Urban/Rural",
     xlim = c(0,11))
grid(10,10)
#lines(predict()

mod <- lm(formula = WorkingData2015$occupation ~ WorkingData2015$respondentAGE)
summary(mod)
plot(mod)

mod2 <- lm(formula = WorkingData2015$edu_attainment ~ WorkingData2015$URBRUR)
plot(mod2)
summary(mod2)

attach(WorkingData2015)
plot(occupation, edu_attainment)
abline(lm(edu_attainment ~ occupation))
title("Regression of Informal/Sector on Educational Attainment")

 plot(WorkingData2015$gender, WorkingData2015$length_withoutfood, type = "o")  ## Index plot
