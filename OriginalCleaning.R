
#project proposal part 2
#	Mar. 3rd, Code for loading/cleaning/validation – 5 points
#QUESTION:What is the write-up?


# I had to get a package that was able to read SPSS files
library(foreign)

# then I read it in as a data frame to make my life easier (just alittle)
NigeriaData <- read.spss("nig_r6_data_2015.sav",
                         to.data.frame = TRUE,
                         use.value.labels = FALSE)

library(dplyr)

#renaming variables
dim(NigeriaData)

WorkingData2015 <-
  select(
    NigeriaData,
    RespondentNum = RESPNO,
    URBRUR,
    ADULT_CT,
    cashincome_without = Q8E,
    respondentAGE = Q1,
    edu_attainment = Q97,
    occupation = Q96A,
    gender = Q101,
    REGION
  )


NigeriaData2012 <- read.spss("nig_r5_data_july_2015.sav",
                         to.data.frame = TRUE,
                         use.value.labels = FALSE)

WorkingData2012 <-
  select(
    NigeriaData,
    RespondentNum = RESPNO,
    URBRUR,
    ADULT_CT,
    respondentAGE = Q1,
    cashincome_without = Q8E,
    edu_attainment = Q97,
    occupation = Q96A,
    gender = Q101,
    REGION
  )

# I need to code between informal and "formal"

load("/Volumes/SADE USB/Documents/Classes/Ecog314_Spring2017-master/Project Data/WV6_Data_R_v_2016_01_01.rdata")

# Editing World Values DATA SET

NigeriaWValues2011 <-
  select(
    WV6_Data_R,
    marital_stat = V57,
    num_children = V58
  )

write.csv(NigeriaWValues2011, file='NigeriaWvaluesdata2011.csv')


names(WorkingData2012)
names(WorkingData2015)

Merge_NG2012_15 <- merge(WorkingData2012, WorkingData2015, by="REGION")

write.csv(Merge_NG2012_15, file='Merge_NG2012_15.csv')


#WorkingData$gender <- as.character(WorkingData$gender)
#WorkingData$gender[WorkingData$gender %in% "1"] <- "male"
#WorkingData$gender[WorkingData$gender %in% "2"] <- "female"


var(WorkingData2015$URBRUR)
range(WorkingData2015$URBRUR)
summary(WorkingData2015$URBRUR)

hist(log(WorkingData2012$edu_attainment))


mod <- lm(formula = WorkingData2012$edu_attainment ~ WorkingData2012$respondentAGE)
summary(mod)
plot(mod)



mod2 <- lme(formula = WorkingData2012$edu_attainment ~ WorkingData2012$URBRUR)

