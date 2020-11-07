### Load Libraries ###
library(dplyr)
library(ggplot2)
library(Amelia)

### Load Data Files ###
StudentsPerformance <- read.csv('StudentsPerformance.csv',header = T)


### Rename the columns ###
names(StudentsPerformance)[c(2:3, 5:8)] <- c("race_ethnicity", "parental_level_of_education", "test_preparation_course", "math_score", "reading_score", "writing_score")

### Create the column of average score ###
StudentsPerformance <- StudentsPerformance %>% mutate(average_score = (math_score+reading_score+writing_score)/3)

### Check Missing Data ###
missmap(StudentsPerformance)

#############################
# Exploratory Data Analysis #
#############################

#=========#
# Gender  #
#=========#
StudentsPerformance$gender %>%
  summary()

### Convert Character Variables into Factor Variables
StudentsPerformance$gender <- sapply(StudentsPerformance$gender,factor)

### Plot for frequency of each gender ###
ggplot(StudentsPerformance, aes(x=gender, y=..count.., fill=gender)) +
  geom_bar() +
  labs(x="Gender", y="Frequency", fill="Gender", title="Frequency of different Gender") +
  theme_minimal()

### Plot for the relationship between average score and each gender ###
ggplot(StudentsPerformance, aes(x=gender, y=average_score, fill=gender)) +
  geom_boxplot() +
  labs(x="Gender", y="Average Score", fill="Gender", title="Average score VS. Gender") +
  theme_minimal()
  

#=================#
# Race Ethnicity  #
#=================#
StudentsPerformance$race_ethnicity %>%
  summary()

### Convert Character Variables into Factor Variables
StudentsPerformance$race_ethnicity <- sapply(StudentsPerformance$race_ethnicity,factor)

### Plot for frequency of each race ethnicity group ###
ggplot(StudentsPerformance, aes(x=race_ethnicity, y=..count.., fill=race_ethnicity)) +
  geom_bar() +
  labs(x="Race Ethnicity", y="Frequency", fill="Race Ethnicity", title="Frequency of different Race Ethnicity") +
  theme_minimal()


### Plot for the relationship between average score and race ethnicity group ###
ggplot(StudentsPerformance, aes(x=race_ethnicity, y=average_score, fill=race_ethnicity)) +
  geom_boxplot() +
  labs(x="Race Ethnicity", y="Average Score", fill="Race Ethnicity", title="Average score VS. Race Ethnicity") +
  theme_minimal()


#==============================#
# Parental Level of Education  #
#==============================#
StudentsPerformance$parental_level_of_education %>%
  summary()

### Convert Character Variables into Factor Variables
StudentsPerformance$parental_level_of_education <- sapply(StudentsPerformance$parental_level_of_education,factor)

### Plot for frequency of each education level ###
ggplot(StudentsPerformance, aes(x=parental_level_of_education, y=..count.., fill=parental_level_of_education)) +
  geom_bar() +
  labs(x="Parental Level of Education", y="Frequency", fill="Parental Level of Education", title="Frequency of different Parental Level of Education") +
  theme_minimal()


### Plot for the relationship between average score and Parental Level of Education ###
ggplot(StudentsPerformance, aes(x=parental_level_of_education, y=average_score, fill=parental_level_of_education)) +
  geom_boxplot() +
  labs(x="Parental Level of Education", y="Average Score", fill="Parental Level of Education", title="Average score VS. Parental Level of Education") +
  theme_minimal()


#=========#
# Lunch   #
#=========#
StudentsPerformance$lunch %>%
  summary()

### Convert Character Variables into Factor Variables
StudentsPerformance$lunch <- sapply(StudentsPerformance$lunch,factor)

### Plot for frequency of each lunch group ###
ggplot(StudentsPerformance, aes(x=lunch, y=..count.., fill=lunch)) +
  geom_bar() +
  labs(x="Lunch", y="Frequency", fill="Lunch", title="Frequency of different Lunch group") +
  theme_minimal()


### Plot for the relationship between average score and lunch group ###
ggplot(StudentsPerformance, aes(x=lunch, y=average_score, fill=lunch)) +
  geom_boxplot() +
  labs(x="Lunch", y="Average Score", fill="Lunch", title="Average score VS. Lunch") +
  theme_minimal()


#==========================#
# Test Preparation course  #
#==========================#
StudentsPerformance$test_preparation_course %>%
  summary()

### Convert Character Variables into Factor Variables
StudentsPerformance$test_preparation_course <- sapply(StudentsPerformance$test_preparation_course,factor)

### Plot for frequency of Taking test preparation course or not ###
ggplot(StudentsPerformance, aes(x=test_preparation_course, y=..count.., fill=test_preparation_course)) +
  geom_bar() +
  labs(x="Test Preparation Course", y="Frequency", fill="Test Preparation Course", title="Frequency of Taking test preparation course or not") +
  theme_minimal()


### Plot for the relationship between average score and Taking test preparation course or not ###
ggplot(StudentsPerformance, aes(x=test_preparation_course, y=average_score, fill=test_preparation_course)) +
  geom_boxplot() +
  labs(x="Test Preparation Course", y="Average Score", fill="Test Preparation Course", title="Average score VS. Taking test preparation course or not") +
  theme_minimal()


#=============#
# Math score  #
#=============#
### Create Bins on Math score ###
Math_Score_bin <- function(ms=0){
  if(ms >= 0 && ms < 10)
    return ("0-10")
  else if(ms >= 10 && ms < 20)
    return ("10-20")
  else if(ms >= 20 && ms < 30)
    return ("20-30")
  else if(ms >= 30 && ms < 40)
    return ("30-40")
  else if(ms >= 40 && ms < 50)
    return ("40-50")
  else if(ms >= 50 && ms < 60)
    return ("50-60")
  else if(ms >= 60 && ms < 70)
    return ("60-70")
  else if(ms >= 70 && ms < 80)
    return ("70-80")
  else if(ms >= 80 && ms < 90)
    return ("80-90")
  else if(ms >= 90 && ms <= 100)
    return ("90-100")
}

### Create Math Score Bin field ###
StudentsPerformance$math_score_bin <- StudentsPerformance$math_score %>% sapply(Math_Score_bin) %>% as.factor()
attributes(StudentsPerformance$math_score_bin)

### Plot for frequency of Math Score ###
ggplot(StudentsPerformance, aes(x=math_score_bin, y=..count.., fill=math_score_bin)) +
  geom_bar() +
  labs(x="Math Score", y="Frequency", fill="Math Score", title="Frequency of Math Score") +
  theme_minimal()

### Plot for the relationship between average score and math score ###
ggplot(StudentsPerformance, aes(x=math_score_bin, y=average_score, fill=math_score_bin)) +
  geom_boxplot() +
  labs(x="Math Score", y="Average Score", fill="Math Score", title="Average score VS. Math Score") +
  theme_minimal()


#================#
# Reading score  #
#================#
### Create Bins on reading score ###
Reading_Score_bin <- function(rs=0){
  if(rs >= 0 && rs < 10)
    return ("0-10")
  else if(rs >= 10 && rs < 20)
    return ("10-20")
  else if(rs >= 20 && rs < 30)
    return ("20-30")
  else if(rs >= 30 && rs < 40)
    return ("30-40")
  else if(rs >= 40 && rs < 50)
    return ("40-50")
  else if(rs >= 50 && rs < 60)
    return ("50-60")
  else if(rs >= 60 && rs < 70)
    return ("60-70")
  else if(rs >= 70 && rs < 80)
    return ("70-80")
  else if(rs >= 80 && rs < 90)
    return ("80-90")
  else if(rs >= 90 && rs <= 100)
    return ("90-100")
}

### Create Reading Score Bin field ###
StudentsPerformance$reading_score_bin <- StudentsPerformance$reading_score %>% sapply(Reading_Score_bin) %>% as.factor()
attributes(StudentsPerformance$reading_score_bin)

### Plot for frequency of Reading Score ###
ggplot(StudentsPerformance, aes(x=reading_score_bin, y=..count.., fill=reading_score_bin)) +
  geom_bar() +
  labs(x="Reading Score", y="Frequency", fill="Reading Score", title="Frequency of Reading Score") +
  theme_minimal()

### Plot for the relationship between average score and reading score ###
ggplot(StudentsPerformance, aes(x=reading_score_bin, y=average_score, fill=reading_score_bin)) +
  geom_boxplot() +
  labs(x="Reading Score", y="Average Score", fill="Reading Score", title="Average score VS. Reading Score") +
  theme_minimal()


#================#
# Writing score  #
#================#
### Create Bins on writing score ###
Writing_Score_bin <- function(wrs=0){
  if(wrs >= 0 && wrs < 10)
    return ("0-10")
  else if(wrs >= 10 && wrs < 20)
    return ("10-20")
  else if(wrs >= 20 && wrs < 30)
    return ("20-30")
  else if(wrs >= 30 && wrs < 40)
    return ("30-40")
  else if(wrs >= 40 && wrs < 50)
    return ("40-50")
  else if(wrs >= 50 && wrs < 60)
    return ("50-60")
  else if(wrs >= 60 && wrs < 70)
    return ("60-70")
  else if(wrs >= 70 && wrs < 80)
    return ("70-80")
  else if(wrs >= 80 && wrs < 90)
    return ("80-90")
  else if(wrs >= 90 && wrs <= 100)
    return ("90-100")
}

### Create Writing Score Bin field ###
StudentsPerformance$writing_score_bin <- StudentsPerformance$writing_score %>% sapply(Writing_Score_bin) %>% as.factor()
attributes(StudentsPerformance$writing_score_bin)

### Plot for frequency of Writing Score ###
ggplot(StudentsPerformance, aes(x=writing_score_bin, y=..count.., fill=writing_score_bin)) +
  geom_bar() +
  labs(x="Writing Score", y="Frequency", fill="Writing Score", title="Frequency of Writing Score") +
  theme_minimal()

### Plot for the relationship between average score and writing score ###
ggplot(StudentsPerformance, aes(x=writing_score_bin, y=average_score, fill=writing_score_bin)) +
  geom_boxplot() +
  labs(x="Writing Score", y="Average Score", fill="Writing Score", title="Average score VS. Writing Score") +
  theme_minimal()
