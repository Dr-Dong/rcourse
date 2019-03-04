#----------------------    Seminar Exercise 3       ----------------------#
# Author: Andrew Proctor                                                  
# This is the script file for suggested solutions to seminar exercise 3,   
# in which basic analysis of movie performance is conducted using data 
# from IMDB as well as box office earnings.

# Load libraries
library(rvest)
library(httr)
library(magrittr)
library(tidyverse)
library(rio)
library(glue)

# Set working directory
setwd("/home/andrew/Documents/rcourse/module3")

#-------------------------------------------------------------------------#

# Revisit NLSY97 dataset

  ## Import data
  nlsy97 <-import("nlsy97.rds")
  
  ## Create female indicator
  nlsy97 <- nlsy97 %>% mutate(female = ifelse(sex == 2, 1, 0))
  
  ## Recode school type
  nlsy97 <- nlsy97 %>% mutate(schooltype = case_when(
    schooltype == 1 ~ "Public",
    schooltype == 2 ~ "Private, religious",
    schooltype == 3 ~ "Private, non-religious",
    schooltype == 4 ~ "Other"
  ))
  
# Load IMDB 250 Dataset
  
  # Basic approach (localized titles)
  top250_basic <- read_html("https://www.imdb.com/chart/top/") %>%
    html_table() %>% as.data.frame()
  
  ## IMDB 250 - English Titles
  top250_eng.pre <- html_session("https://www.imdb.com/chart/top/", 
      add_headers("Accept-Language"="en-US, en;q=0.5")) %>% 
    html_table %>% as.data.frame()
  
  ## Keep only and rename Title and Year Columns
  top250_eng <- top250_eng.pre %>% select("Rank...Title", "IMDb.Rating") %>%
    rename(Title = Rank...Title,
           Rating = IMDb.Rating) 

# Data preparation using string manipulation (with stringr)
  
  ## Extract ranking into new variable
  top250_eng$Ranking <- top250_eng$Title %>% str_extract("[0-9]+(?=(.\n))")
  
  ## Extract movie year into new variable
  top250_eng$Year <- str_extract(top250_eng$Title, "(?<=[:punct:])[:digit:]+") 
  
  ## Redefine title variable
  top250_eng$Title <- top250_eng$Title %>% str_extract("(?<=(.\n)).+")
  
  ## Trim white space in Title variable
  top250_eng$Title %<>% str_trim(side = "both")
  
  ## View final IMDB250 data frame
  head(top250_eng)
  
# Get box office statistics for top 500 all-time US box office earners
  
  ## Scrape data from pages of boxofficemojo.com
  domesticgross <- list()
  for(i in 1:5){
    domesticgross[[i]] <-read_html(
      glue("https://www.boxofficemojo.com/alltime/domestic.htm?page={i}")) %>% 
      html_nodes(xpath = "//table") %>% html_table(fill=TRUE)  %>% 
      extract2(6)
  }
  
  ## Append all observations into single data frame
  topearners <- domesticgross %>% bind_rows()
  
  ## Ensure column names are assigned and not part of table
  colnames(topearners) <-topearners[1,]
  topearners %<>%  filter(Rank !="Rank")
  
  ## Select and rename title and box office earnings variables
  topearners %<>% rename("Title" = "Title(click to view)",
                         "Gross"="Lifetime Gross") %>%
    select(Title,Studio, Gross)
  
  ## View the box office earnings dataset
  head(topearners)

# Prepare a dataset with both IMDB performance and earnings
  
  ## Merge data only for movies found in both datasets
  expensive_movies <-inner_join(top250_eng, topearners, by="Title")
  
  ## Remove characters from box office earnings variable
  expensive_movies$Gross %<>% str_replace_all("[$,]+","")

  ## Change data type of numeric variables
  expensive_movies[,c("Ranking","Gross","Year")] %<>% map(as.numeric)
  
  ## Create a log earnings variable
  expensive_movies %<>%  mutate(logearnings = log(Gross))
  
# Regression Analysis
  
  ## Create an OLS function
  myols <- function(depvar,indvars) {
    x <- indvars %>% as_tibble()
    x %<>% as_tibble() %>% mutate(Constant = 1) %>% as.matrix()
    y <- depvar %>% as.matrix()
    beta_myols <- t(solve(t(x) %*% x) %*% (t(x) %*% y))
    colnames(beta_myols) <-colnames(x)
    rownames(beta_myols) <- "Estimate"
    beta_myols <- t(beta_myols)
    return(beta_myols)
  }
  
  ## Regress log earnings on rating and year
  myols(expensive_movies$logearnings,expensive_movies[,c("Rating","Year")])
  