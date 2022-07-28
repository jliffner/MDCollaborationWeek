# this is just some very rough code to get everyone started
# once you've cloned the repo you can make changes to the working directory in this file and start playing around with the data

# I can't recall specifically which package is for what - I kind just load them all for this kind of stuff
# I also started this in NGD - the withRepo() should be removed outside of NGD
withRepo(library(dplyr))
withRepo(library(tidyr))
withRepo(library(lubridate))
withRepo(library(fansi))
withRepo(library(utf8))
withRepo(library(cli))
withRepo(library(labeling))
withRepo(library(scales))
withRepo(library(tidyverse))
withRepo(library(RColorBrewer))
withRepo(library(gridExtra))
withRepo(library(digest))
withRepo(library(ggplot2))
withRepo(library(ggfortify))
withRepo(library(factoextra))
withRepo(library(ggpubr))
withRepo(library(clue))
withRepo(library(stringr))
withRepo(library(reshape2))
withRepo(library(ggforce))
withRepo(library(zoo))
withRepo(library(grid))
withRepo(library(extrafont))
withRepo(library(forcats))
withRepo(library(gtable))
withRepo(library(useful))
withRepo(library(cowplot))

# change the working directory to the repository
setwd("\\corp\peopledfs\liffjo\Documents\MD Collaboration Week")

# read in the data
health <- read.csv("Health_Expenditure.csv")
housing <- read.csv("SA_Housing_stress.csv")
energy <- read.csv("HH_energy.csv")
population <- read.csv("ACT_Population_Projections.csv")

# HH energy consumption fuel type by dwelling type
# melt data in to long format
energy_melt <- melt(energy, id.vars = "Dwelling.type", value.name = "Consumption")

# plot the data - grouped bar chart
# we can probably tidy this up a bit by renaming factor levels, modifying axis etc.
plot_1 <- energy_melt %>% ggplot(., aes(x = Dwelling.type, y = Consumption)) + geom_bar(aes(fill = variable), position = "dodge", stat = "identity")

# saving the plot as an image
png("plot1.png", width=600, height=350)
plot_1
dev.off()

# national health expenditure  for a specific financial year - filtered by year, then grouped the data and summed the expenditure before plotting
plot_2 <- health %>% filter(financial_year == "1997-98") %>% group_by(area_of_expenditure, broad_source_of_funding) %>% 
  summarise(total_exp = sum(real_expenditure_millions)) %>% ggplot(aes(x = area_of_expenditure, y = total_exp)) + 
  geom_bar(aes(fill = broad_source_of_funding), position = "dodge", stat = "identity") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  ggtitle("Total national health expenditure for 1997-98")

# saving the plot as an image
png("plot2.png", width=600, height=350)
plot_2
dev.off()




