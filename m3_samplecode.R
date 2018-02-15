# Initialize packages
library(rio)
library(tidyverse)

# Set working directory
setwd("C:/Users/AN.4271/OneDrive/sseR")

# Import files
characteristics <- import("./data/fmli161x.sas7bdat") %>% as.tibble()
expenditures <- import("./data/mtbi161x.sas7bdat")%>% as.tibble()

# Make column names lower case
colnames(characteristics) <- colnames(characteristics) %>% map(tolower)
colnames(expenditures) <- colnames(expenditures) %>% map(tolower)

# Select only desired variables in each data set
characteristics <- characteristics %>% select(newid,hh_cu_q,educ_ref,creditx, 
                                              region, fincbtxm)
expenditures <- expenditures %>% select(newid,cost,ref_mo,ref_yr)

# Rename selected variables
characteristics <- characteristics %>% rename(hh_size=hh_cu_q, 
                                              hh_income=fincbtxm)

# Make columns except for "newid" numeric
characteristics[,-1] <- characteristics[,-1] %>% map(as.numeric)
expenditures[,-1] <- expenditures[,-1] %>% map(as.numeric)

# Create 80% sample of data frames
characteristics <- sample_frac(characteristics, 0.8)
expenditures <- sample_frac(expenditures, 0.8)

# Practice different joins
cex_data <- left_join(expenditures, characteristics, by="newid")
cex_data_inner <- semi_join(expenditures, characteristics, by="newid")
cex_data_semi <- semi_join(expenditures, characteristics, by="newid")
cex_data_anti <- anti_join(expenditures, characteristics, by="newid")

# Create regions indicators
cex_data <- cex_data %>% mutate(region1 = ifelse(region == 1,1,0),
                                region2 = ifelse(region == 2,1,0),
                                region3 = ifelse(region == 3,1,0),
                                region4 = ifelse(region == 4,1,0),
                                region4 = ifelse(is.na(region),1,0)
                                )
                                
# Define ols function
my_ols <- function(indvars,depvar){
  
  # Keep only observations with no missing values for indvars and depvar
  X <- indvars[(!is.na(indvars)) & (!is.na(depvar))] 
  y <- depvar[(!is.na(indvars)) & (!is.na(depvar))]  
 
  # Create constant vector
  ones_vec <- matrix(rep(1), length(X)) 
  
  # Create matrix X equal to constant vec and indvars
  X <- cbind(ones_vec, X)     
  
  # Name constant column "constant"
  colnames(X)[1] <- "constant" 

  # Solve for coefficients
  beta <- solve(t(X)%*%X) %*% (t(X)%*%y) 
  colnames(beta) <- "Estimate"
  
  # Convert to data frame
  beta <- as.data.frame(beta)  

}

coeffs_schooling <- my_ols(cex_data$educ_ref, cex_data$cost)
coeffs_schooling

