#----------------------    Seminar Exercise 2       ----------------------#
# Author: Andrew Proctor                                                  
# This is the script file for suggested solutions to seminar exercise 2,   
# in which basic analysis of the NLSY97 dataset is conducted.             

# Load libraries
library(rio)
library(tidyverse)

# Set working directory
setwd("/home/andrew/Documents/rcourse/module2")

#-------------------------------------------------------------------------#

# Import data
nlsy97 <- import("m2_nlsy97.rds")

# Explore structure of the data

  ## Look at the structure of the data
  glimpse(nlsy97)
  str(nlsy97)
  
  ## Save the column names to a tibble
  cols_nlsy97 <-colnames(nlsy97) %>% as_tibble()

# Rename variables and "tidy" the structure of the dataset
  
  ## Rename Time-Invariant Variables
  nlsy97 <- nlsy97 %>% rename(
    personid = PUBID_1997,
    sex = KEY_SEX_1997,
    race = KEY_RACE_ETHNICITY_1997,
    birthyr = KEY_BDATE_Y_1997,
    motheredyrs = CV_HGC_RES_MOM_1997)
  
  ## Gather time-varying variables
  nlsy97 <- nlsy97 %>% 
    gather(variable, value, -c(personid,sex,race,birthyr,motheredyrs))
  
  ## Extract year from variable names in 'variable' column
  nlsy97$year <- str_extract(nlsy97$variable, "[0-9]+")
  
  ## Recode problematic year values
  nlsy97$year <- nlsy97$year %>% recode(
    "1993"="9293", 
    "1994"="9394",
    "9495"="1995",
    "9697"="1997",
    "9798"="1998",
    "9899"="1999",
    "9900"="2000",
    "0001"="2001",
    "0102"="2002",
    "0203"="2003",
    "0304"="2004")
  
  ## Remove year from variable names in 'variable' column
  nlsy97$variable <- str_replace(nlsy97$variable, "[0-9]+","")
  
  ## Spread variables into individual columns
  nlsy97 <- nlsy97 %>% spread(key = variable, value=value)
  
  ## Rename time-varying variables
  nlsy97 <- nlsy97 %>% rename(
    region = CV_CENSUS_REGION_,
    parentincome = CV_INCOME_GROSS_YR_,
    schooltype = CV_SCHOOL_TYPE_,
    highestgrade = CV_HGC_EVER_EDT_,
    absences = TRANS_AB_AY_HSTR,
    gpa = TRANS_CRD_GPA_YR__HSTR,
  )

# Subset the dataset and apply format data type
  
  ## Keep only designated variables
  nlsy97 <- nlsy97 %>% select(
    personid, year, sex,race, birthyr, region, parentincome,
    schooltype,motheredyrs,highestgrade, absences, gpa)
  
  ## Change data type of variables
  nlsy97$personid <- as.factor(nlsy97$personid)
  nlsy97$year <- as.numeric(nlsy97$year)
  nlsy97$sex <- as.factor(nlsy97$sex)
  nlsy97$race <- as.factor(nlsy97$race)
  nlsy97$region <-as.factor(nlsy97$region)
  nlsy97$schooltype <- as.factor(nlsy97$schooltype)
  
  # Restrict sample to indiduals in the South-East, between 1997-2007, with 
  # highest grade complete between 8 and 17
  nlsy97 <- nlsy97 %>% filter(
    year %in% 1997:2007,
    region == 4,
    highestgrade %in% 8:17
  )

# Create and modify variable values
  
  # Create new variables
  nlsy97 <- nlsy97 %>% mutate(
    age = year - birthyr,
    gradesback = (age - 7) - highestgrade,
    female = (sex == 2),
    nonwhite = (race != 4)
  )
  
  # Rescale GPA
  nlsy97 <- nlsy97 %>% mutate(
    gpa = gpa / 100
  )

# Statistical Analysis

  ## Summary Statistics by Age
  summarybyage <- nlsy97 %>% group_by(age) %>% 
    summarize(
      avg_gradecompleted = mean(highestgrade, na.rm = TRUE),
      avg_gradesback = mean(gradesback, na.rm = TRUE))
  summarybyage
  
  ## Summary Statistics by Grade
  summarybygrade <- nlsy97 %>% group_by(highestgrade) %>% 
    summarize(
      avg_parincome = mean(parentincome, na.rm = TRUE),
      avg_mothersed = mean(motheredyrs, na.rm = TRUE),
      avg_gpa = mean(gpa, na.rm = TRUE),
      share_female = mean(female, na.rm = TRUE),
      share_nonwhite = mean(nonwhite, na.rm = TRUE))
  summarybyage
