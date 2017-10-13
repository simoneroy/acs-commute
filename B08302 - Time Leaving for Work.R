#B08302 - Time Leaving for Work
library(tidyverse) 
library(tidycensus) #for acs data
library(forcats) #for recoding

census_api_key('xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx') #insert API key here

#FIPS codes from selected states and counties
dmv_state_fips <- c("11","24", "51")
dmv_counties_fips <- c("11001", "24031", "24033", "51013", "51059", "51107", "51153", "51510")

acs_years <- c("2012", "2013", "2014", "2015", "2016") #select years

b08302_vars <-sprintf("B08302_%03d", seq(2,15)) #select values for variables

#get time leaving for work data for both DC MSA and selected counties within the DC MSA
#DC MSA
time_leaving_for_work_msa <- map_df(acs_years,  function(x) {
  get_acs(geography = "metropolitan statistical area/micropolitan statistical area", 
          variables = b08302_vars, summary_var = "B08302_001", 
          survey="acs1", endyear = x) %>%  filter(GEOID == "47900") %>% 
    mutate(Year = x,
           Percent = round((estimate/summary_est) * 100, 2))
})


#DC Counties
time_leaving_for_work_dmv <- map_df(acs_years, function(x) {
  get_acs(geography = "county", variables = b08302_vars, summary_var = "B08302_001",
          state = dmv_state_fips, survey="acs1", endyear = x) %>% 
    filter(GEOID %in% dmv_counties_fips) %>% 
    mutate(Year = x,
           Percent = round((estimate/summary_est) * 100, 2))
})


#combine data frames for msa and counties
time_leaving_for_work_dmv <- rbind(time_leaving_for_work_msa, time_leaving_for_work_dmv)

time_leaving_for_work_dmv %>% glimpse

B08302_varnames <- read.csv("B08302_varnames.csv") #read in variable labels

#join variable names to the time leaving for work data frame
time_leaving_for_work_df <- right_join(time_leaving_for_work_dmv, B08302_varnames)


#reorder factors
time_leaving_for_work_df$time <- ordered(time_leaving_for_work_df$time, 
                                         levels = c("12:00 a.m. to 4:59 a.m.",
                                                    "5:00 a.m. to 5:29 a.m.",
                                                    "5:30 a.m. to 5:59 a.m.",
                                                    "6:00 a.m. to 6:29 a.m.",
                                                    "6:30 a.m. to 6:59 a.m.",
                                                    "7:00 a.m. to 7:29 a.m.",
                                                    "7:30 a.m. to 7:59 a.m.",
                                                    "8:00 a.m. to 8:29 a.m.",
                                                    "8:30 a.m. to 8:59 a.m.",
                                                    "9:00 a.m. to 9:59 a.m.",
                                                    "10:00 a.m. to 10:59 a.m.",
                                                    "11:00 a.m. to 11:59 a.m.",
                                                    "12:00 p.m. to 3:59 p.m.",
                                                    "4:00 p.m. to 11:59 p.m."
                                         ))


#reverse factor order so that times will be in order when plotted
time_leaving_for_work_df$time <- factor(time_leaving_for_work_df$time, 
                                        levels=rev(levels(time_leaving_for_work_df$time)))

#create alternative labels for plotting
time_leaving_for_work_df <- time_leaving_for_work_df %>%
  mutate(Location = fct_recode(GEOID,
                               "District of Columbia" = "11001",
                               "Montgomery County, Maryland" = "24031",
                               "Prince George's County, Maryland" = "24033",
                               "Arlington County, Virginia" = "51013",
                               "Fairfax County, Virginia" ="51059",
                               "Loudoun County, Virginia" ="51107",
                               "Prince William County, Virginia" = "51153",
                               "Alexandria" = "51510",
                               "D.C. Metro Area" = "47900")) 

time_leaving_for_work_df <- time_leaving_for_work_df %>%
  mutate(Short_Name = fct_recode(GEOID,
                                 "D.C." = "11001",
                                 "Montgomery County" = "24031",
                                 "Prince George's County" = "24033",
                                 "Arlington County" = "51013",
                                 "Fairfax County" ="51059",
                                 "Loudoun County" ="51107",
                                 "Prince William County" = "51153",
                                 "Alexandria" = "51510",
                                 "D.C. Metro" = "47900")) 


#--------------------------plot using ggplot and plotly--------------------------#


p <- time_leaving_for_work_df %>% filter(Year == "2016") %>% 
  ggplot(aes(x=time, y=Percent)) +
  geom_bar(stat="identity",  fill="#527394") +
  facet_wrap(~Short_Name) +
  coord_flip() +
  labs(caption = "Source:  U.S. Census Bureau, 2016 American Community Survey 1-Year Estimates",
       title = "Time Leaving Home to go to Work: 2016",
       x = "",
       y = "% Leaving for Work") +
  theme_minimal() +
  theme(plot.caption = element_text(hjust=0, colour="grey50"))


library(plotly)
ggplotly(p)

time_leaving_for_work_df %>% glimpse

long_commutes <- c("24033", "51153")
late_and_early_commutes <- c("11001","24033", "51153", "47900")

p <- time_leaving_for_work_df %>% filter(Year == "2016", GEOID %in% long_commutes) %>% 
  ggplot(aes(x=time, y=Percent)) +
  geom_bar(stat="identity",  fill="#527394") +
  facet_wrap(~Short_Name) +
  coord_flip() +
  labs(caption = "Source:  U.S. Census Bureau, 2016 American Community Survey 1-Year Estimates",
       title = "Time Leaving Home to go to Work: 2016",
       x = "",
       y = "% Leaving for Work") +
  theme_minimal() +
  theme(plot.caption = element_text(hjust=0, colour="grey50"))



time_leaving_for_work_df %>% filter(Year == "2016", GEOID %in% late_and_early_commutes) %>% 
  ggplot(aes(x=time, y=Percent)) +
  geom_bar(stat="identity",  fill="#527394") +
  facet_wrap(~Short_Name) +
  coord_flip() +
  labs(caption = "Source:  U.S. Census Bureau, 2016 American Community Survey 1-Year Estimates",
       title = "Time Leaving Home to go to Work: 2016",
       x = "",
       y = "% Leaving for Work") +
  theme_minimal() +
  theme(plot.caption = element_text(hjust=0, colour="grey50"))



