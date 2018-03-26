# Initialize libraries
library(rio)
library(tidyverse)

# Part 2 -----------------------------------------------------------------------

  # Import data
  world.dev.data <- import("WDIData.csv")
  
  # Convert dataset to a tibble
  world.dev.data <- world.dev.data %>% as.tibble()


# Part 3:  Filter the data set for the desired indicators ----------------------
  
  # Vector of indicators to keep
  myindics <- c("NY.GDP.PCAP.KD", "SP.POP.TOTL", "SL.TLF.CACT.FM.ZS", 
                "SE.SEC.CUAT.UP.ZS", "SL.UEM.NEET.ZS", "SL.UEM.BASC.ZS", 
                "SI.POV.UMIC","SI.DST.FRST.20","SI.DST.02ND.20",
                "SI.DST.03RD.20","SI.DST.10TH.10")
  
  # Rename the indicator code column to just "indicator"
  world.dev.data <- world.dev.data %>% rename(indicator = "Indicator Code")
  
  # Filter for indicators that are in myindics
  world.dev.data <- world.dev.data %>% filter(indicator %in% myindics)

# Part 4: Tidy the data set ----------------------------------------------------
  
  # Look at structure of dataset
  glimpse(world.dev.data)
  
  # Drop the following columns: "v63", "Indicator Name"
  world.dev.data[,"v63"] <- NULL
  world.dev.data[,"Indicator Name"] <- NULL
  
  # Format the dataset so that it follows "tidy data" principles
  world.dev.data <- world.dev.data %>% gather(key="year", value="value","1960":"2017") %>% 
    spread(key="indicator", value="value")

# Part 5: Reformat variables ---------------------------------------------------
  
  # Change to numeric
  world.dev.data$NY.GDP.PCAP.KD <-as.numeric(world.dev.data$NY.GDP.PCAP.KD) 
  world.dev.data$year <-as.numeric(world.dev.data$year) 
  world.dev.data$SP.POP.TOTL <-as.numeric(world.dev.data$SP.POP.TOTL) 
  world.dev.data$SL.TLF.CACT.FM.ZS <-as.numeric(world.dev.data$SL.TLF.CACT.FM.ZS) 
  world.dev.data$SE.SEC.CUAT.UP.ZS <-as.numeric(world.dev.data$SE.SEC.CUAT.UP.ZS) 
  world.dev.data$SL.UEM.NEET.ZS <-as.numeric(world.dev.data$SL.UEM.NEET.ZS) 
  world.dev.data$SL.UEM.BASC.ZS <-as.numeric(world.dev.data$SL.UEM.BASC.ZS) 
  world.dev.data$SI.POV.UMIC <-as.numeric(world.dev.data$SI.POV.UMIC) 
  world.dev.data$SI.DST.FRST.20 <-as.numeric(world.dev.data$SI.DST.FRST.20) 
  world.dev.data$SI.DST.02ND.20 <-as.numeric(world.dev.data$SI.DST.02ND.20) 
  world.dev.data$SI.DST.03RD.20 <-as.numeric(world.dev.data$SI.DST.03RD.20) 
  world.dev.data$SI.DST.10TH.10 <-as.numeric(world.dev.data$SI.DST.10TH.10) 
  
  # Rename variables
  world.dev.data <- world.dev.data %>% 
    rename(countrycode = "Country Code", country = "Country Name",
           pcgdp = NY.GDP.PCAP.KD, pop = SP.POP.TOTL, 
           femaletomale = SL.TLF.CACT.FM.ZS, 
           secondarycomplet = SE.SEC.CUAT.UP.ZS,
           disengaged_youth = SL.UEM.NEET.ZS,
           unemp.basic.educ. = SL.UEM.BASC.ZS,
           povertyrte = SI.POV.UMIC,
           incshare_low20 = SI.DST.FRST.20, incshare_2nd20 = SI.DST.02ND.20,
           incshare_3rd20 = SI.DST.03RD.20,incshare_top10 = SI.DST.10TH.10)

# Part 6: Further restrict the sample ------------------------------------------

  ## Import regions.csv
  regions <- import("regions.csv") 
  regions <- as.character(regions$CountryCode)
  
  ## Filter for country codes not in `regions`
  world.dev.data <- world.dev.data %>% filter(!(countrycode %in% regions))
  
  ## Drop the `countrycode` column
  world.dev.data$countrycode <- NULL
  
  ##Restrict sample to countries with a population greater than 4 million,
  ## GDP per capita greater than $12,000, and year greater than or equal to 2007.
  world.dev.data <- world.dev.data %>% 
    filter(year >= 2000, pcgdp >= 12000,pop > 4000000)
  
# Part 7: Create new variables  ------------------------------------------------

  ## Create  - log of GDP per capita
      # log of GDP per capita
      # Absolute # of people in poverty (at $5.50 a day)
      # Income share held by the bottom 60% of the population
      # Ratio of income held by top 10% to bottom 60%  
  
  world.dev.data <- world.dev.data %>% 
    mutate(logGDP = log(pcgdp), 
           numpoverty = povertyrte * pop,
           income60 = incshare_low20 + incshare_2nd20 + incshare_3rd20,
           income10to60 = incshare_top10 / income60)
  
# Part 8: Further restrict the sample ------------------------------------------
  
  ##  By country, create average values (with option `na.rm=TRUE`) for:
  
      # Ratio of income held by top 10% to bottom 60%
      # GDP per capita
      # Ratio of female to male labor force partipation
      # Share of population having completely at least completed upper secondary schooling
      # Share of youth not in education, employment or training, total (% of youth population)
      # Unemployment with basic education (% of total labor force with basic education)
  
  my_summary <- world.dev.data %>% group_by(country, year) %>% 
    summarize(income10to60.avg = mean(income10to60, na.rm=TRUE),
              gdp.avg = mean(pcgdp, na.rm=TRUE),
              emp_femaletomale.avg = mean(emp_femaletomale, na.rm=TRUE),
              secondary.complet.avg = mean(secondary.complet, na.rm=TRUE),
              disengaged.yth.avg = mean(disengaged.youth, na.rm=TRUE),
              unemp.basic.educ.avg = mean(unemp.basic.educ, na.rm=TRUE)
    )
  
  
  ## Then usepercent_rank to create a table of percentile ranks for each of the 
  ## indicators in created in my_summary by country.\\
  my_summary_prank <- my_summary %>% 
    mutate(income10to60.xtile=percent_rank(income10to60.avg),
           gdp.xtile = percent_rank(gdp.avg),
           emp_femaletomale.xtile = percent_rank(emp_femaletomale.avg),
           secondary.complet.xtile = percent_rank(secondary.complet.avg),
           disengaged.yth.xtile = percent_rank(disengaged.yth.avg),
           unemp.basic.xtile = percent_rank(unemp.basic.educ.avg))
  
  ## Keep only the percentile rank columns and sort by per capita GDP percentile rank.
  my_summary_prank <- my_summary_prank %>% 
    select(income10to60.xtile, gdp.xtile, emp_femaletomale.xtile, 
           secondary.complet.xtile, disengaged.yth.xtile, unemp.basic.xtile) %>% 
    arrange(desc(gdp.xtile))
  
  my_summary_prank[1:20, 1:5]

# Part 9: Display basic descriptive statistics ---------------------------------
  
  summary(world.dev.data)
  
  