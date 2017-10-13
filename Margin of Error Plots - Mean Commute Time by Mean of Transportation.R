#Commute Margin of Error

library(tidyverse)
library(extrafont) #to load fonts

commute_time_by_means <- read.csv("commute_time_by_means_new.csv")

commute_time_by_means %>% filter(Method == "Public Transportation") %>%  #filter for public transportation
  ggplot(aes(x=value, y=Year)) +
  geom_errorbarh(aes(xmin = value - moe, xmax = value + moe)) + #error bar
  geom_point(color = "red", size = 2) + #estimate
  xlim(30, 80) + #set limits so that both charts are scaled similarly
  facet_wrap(~Short_Name, scales = "free_x") + #small multiples by location
  labs(caption = "Source: U.S. Census Bureau, 2012-2016 American Community Survey 1-Year Estimates",
       title = "Mean Commute Length - Public Transportation, 2012-2016",
       x = "Commute Length (Minutes)",
       y = "") +
  theme_minimal() +
  theme(plot.margin = margin(.25, .25, .25, .25, "cm"),
        panel.spacing = unit(.5, "lines"),
        panel.grid = element_line(colour = "grey95"),
        strip.text = element_text(family = "Lato", size=10, hjust=0),
        plot.caption = element_text(hjust=0, family = "Lato", colour="grey50",
                                    margin = margin(t = 5, r=0, b = 0, l =0)),
        plot.title = element_text(family = "Roboto Condensed", size=13, face = "bold"),
        axis.text = element_text(family = "Roboto Condensed", colour="grey40"),
        axis.text.x = element_text(size = 8),
        axis.title = element_text(family = "Lato", size=10),
        axis.title.x = element_text(margin = margin(t = 10, r=0, b = 0, l =0)))


commute_time_by_means %>% filter(Method == "Drove Alone") %>% #filter for drove alone
  ggplot(aes(x=value, y=Year)) +
  geom_errorbarh(aes(xmin = value - moe, xmax = value + moe)) + #error bar
  geom_point(color = "red", size = 2) + #estimate
  xlim(0, 50) + #set limits so that both charts are scaled similarly
  facet_wrap(~Short_Name, scales = "free_x") + #small multiples by location
  labs(caption = "Source: U.S. Census Bureau, 2012-2016 American Community Survey 1-Year Estimates",
       title = "Mean Commute Length - Drove Alone, 2012-2016",
       x = "Commute Length (Minutes)",
       y = "") +
  theme_minimal() +
  theme(plot.margin = margin(.25, .25, .25, .25, "cm"),
        panel.spacing = unit(.5, "lines"),
        panel.grid = element_line(colour = "grey95"),
        strip.text = element_text(family = "Lato", size=10, hjust=0),
        plot.caption = element_text(hjust=0, family = "Lato", colour="grey50",
                                    margin = margin(t = 5, r=0, b = 0, l =0)),
        plot.title = element_text(family = "Roboto Condensed", size=13, face = "bold"),
        axis.text = element_text(family = "Roboto Condensed", colour="grey40"),
        axis.text.x = element_text(size = 8),
        axis.title = element_text(family = "Lato", size=10),
        axis.title.x = element_text(margin = margin(t = 10, r=0, b = 0, l =0)))