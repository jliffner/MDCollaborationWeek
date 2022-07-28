# this is just some very rough code to get everyone started
# once you've cloned the repo you can make changes to the working directory in this file and start playing around with the data

# I can't recall specifically which package is for what - I kind just load them all for this kind of stuff
library(dplyr)
library(tidyr)
library(lubridate)
library(fansi)
library(utf8)
library(cli)
library(labeling)
library(scales)
library(tidyverse)
library(RColorBrewer)
library(gridExtra)
library(digest)
library(ggplot2)
library(ggfortify)
library(factoextra)
library(ggpubr)
library(clue)
library(stringr)
library(reshape2)
library(ggforce)
library(zoo)
library(grid)
library(extrafont)
library(forcats)
library(gtable)
library(useful)
library(cowplot)

# some specific table colours
table_theme <- ttheme_default(
  # Use hjust and x to left justify the text
  # Alternate the row fill colours
  core = list(bg_params = list(fill = c("#E2E2E3", "#134A71")),
              fg_params = list(col = c("#134A71", "#E2E2E3"))),
  
  # Change column header background
  colhead = list(fg_params = list(col = "white"), bg_params = list(fill = "#901216", alpha = 0.7))
)

# change the working directory to the repository
setwd("C:\Users\Joel Liffner\Documents\MDCollaborationWeek")

# read in the data
health <- read.csv("Health_Expenditure.csv")
housing <- read.csv("SA_Housing_stress.csv")
energy <- read.csv("HH_energy.csv")
population <- read.csv("ACT_Population_Projections.csv")

# energy dataset
# HH energy consumption fuel type by dwelling type
# melt data in to long format
energy_melt <- melt(energy, id.vars = "Dwelling.type", value.name = "Consumption")
names(energy_melt)[names(energy_melt) == 'variable'] <- 'Energy type'

# plot the data - grouped bar chart
plot_1 <- energy_melt %>% 
  ggplot(., aes(x = Dwelling.type, y = Consumption)) + 
  geom_bar(aes(fill = `Energy type`), position = "dodge", stat = "identity") +
  scale_x_discrete(labels = function(x)stringr::str_wrap(x, width = 15)) + 
  ggtitle("Energy consumption by dwelling type") + 
  xlab("Dwelling type")

# saving the plot as an image
png("energy_plot.png", width=1200, height=700)
plot_1
dev.off()

# health dataset
# national health expenditure  for a specific financial year - filtered by year, then grouped the data and summed the expenditure before plotting
plot_2 <- health %>% filter(financial_year == "1997-98") %>% group_by(area_of_expenditure, broad_source_of_funding) %>% summarise(total_exp = sum(real_expenditure_millions)) %>% 
  ggplot(aes(x = area_of_expenditure, y = total_exp)) + 
  geom_bar(aes(fill = broad_source_of_funding), position = "dodge", stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  ggtitle("Total national health expenditure for 1997-98") +
  ylab("Total expenditure ($M)") + xlab("Area of Expenditure") + 
  guides(fill = guide_legend(title = "Source of funding"))

# saving the plot as an image
png("health_expenditure_plot1.png", width=1200, height=700)
plot_2
dev.off()

# state health expenditure for a specific financial year
plot_3 <- health %>% filter(financial_year == "1997-98") %>% group_by(area_of_expenditure, state) %>% summarise(total_exp = sum(real_expenditure_millions)) %>% 
  ggplot(aes(x = area_of_expenditure, y = total_exp)) + 
  geom_bar(aes(fill = state), position = "dodge", stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  ggtitle("State health expenditure for 1997-98") + 
  ylab("Total expenditure ($M)") + 
  xlab("Area of Expenditure") + 
  guides(fill = guide_legend(title = "State"))

# saving the plot as an image
png("health_expenditure_plot2.png", width=1200, height=700)
plot_3
dev.off()

# total health expenditure over time
plot_4 <- health %>% na.omit() %>% group_by(area_of_expenditure, financial_year) %>% summarise(total_exp = sum(real_expenditure_millions)) %>% 
  ggplot(aes(x = financial_year, y = total_exp, group = area_of_expenditure)) + 
  geom_line(aes(colour = area_of_expenditure)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  ggtitle("Total national health expenditure over time") + 
  ylab("Total expenditure ($M)") + 
  xlab("Area of Expenditure") + 
  guides(fill = guide_legend(title = "Area of expenditure"))

# saving the plot as an image
png("health_expenditure_plot3.png", width=1200, height=700)
plot_4
dev.off()

# housing dataset
# number of SA households under income stress by tenure type
housing_summary <- housing %>% group_by(Tenure.type) %>% summarise(`Very low` = sum(Very.low.income...603.per.wk),
                                                                   `Low` = sum(Low.income..603..964.per.wk),
                                                                   `Moderate` = sum(Moderate.income..965..1446.per.wk),
                                                                   Total = sum(Total))

housing_summary$`Very Low (%)` <- round(housing_summary$`Very low`/housing_summary$Total,2 )*100
housing_summary$`Low (%)` <- round(housing_summary$`Low`/housing_summary$Total,2 )*100
housing_summary$`Moderate (%)` <- round(housing_summary$`Moderate`/housing_summary$Total,2 )*100

housing_summary  <- tableGrob(housing_summary, rows = NULL, theme = table_theme)
housing_summary <- gtable_add_grob(housing_summary, grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)), t = 2, b = nrow(housing_summary), l = 1, r = ncol(housing_summary))
housing_summary <- gtable_add_grob(housing_summary, grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)), t = 1, l = 1, r = ncol(housing_summary))

housing_summary <- as_ggplot(housing_summary)

# saving the plot as an image
png("housing_figure.png", width=1200, height=700)
housing_summary
dev.off()

# population dataset
# total population projections over time by age group
plot_5 <- population %>%
  ggplot(aes(x = Year, y = Persons, group = Age.Group)) + 
  geom_line(aes(colour = Age.Group)) + 
  ggtitle("Projected ACT populations by age group") + 
  ylab("# of persons") + 
  guides(fill = guide_legend(title = "Age Group"))

# saving the plot as an image
png("population_plot1.png", width=1200, height=700)
plot_5
dev.off()