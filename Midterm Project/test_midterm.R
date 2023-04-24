library(tidyr)
library(dplyr)
library(ggplot2)

################################################################################
#               General setup for choosing files and setup                     #  
################################################################################

# Finding the path for the file
my.file <- file.choose()

# Reading file into a data gram
# File has a header
fryr_collg_demogr <-
  read.csv(my.file,
           header = TRUE,
           sep = ",",
           stringsAsFactors = F)

# Filters down the results by the year 2017
fryr_collg_demo_2017 <- fryr_collg_demogr %>% filter(year == 2017)

# Shortens the number of variables to columns starting with dif, col, mkt
fryr_2017_short <-
  fryr_collg_demo_2017 %>% select(fips_ipeds,
                                  inst_name,
                                  fourcat,
                                  starts_with("dif_"),
                                  starts_with("mkt_"),
                                  starts_with("col_")
                                 )

# Lengthens the columns starting with dif to use in plots
fryr_2017_short_dif <-
  fryr_2017_short %>% pivot_longer(cols = dif_white:dif_twora,
                                   names_to = "dif_pct_race",
                                   values_to = "dif_in_pct") 

################################################################################
#               Plots and Tables for Research Question 1                       #  
################################################################################

# Groups college demographics by state
fryr_collg_demo_2017 <- fryr_collg_demo_2017 %>% group_by(fips_ipeds)

# Filters out Private More Selective for more accurate min/max in col columns
#fryr_collg_demo_2017 <- fryr_collg_demo_2017 # %>% 
#  filter(fourcat != "Private More Selective")

# Setting up data for all ggplots
fryr_plot <- ggplot(data = fryr_2017_short_dif)

# Plot for research question 1
# Displays a graph of ethnicity on the x axis and the percent difference
# between College demographic percentages and Market demographic percentages
# separated by state
fryr_plot + geom_point(
  aes(x = dif_pct_race, y = dif_in_pct, color = fourcat),
  alpha = 0.3,
  position = 'jitter'
) +
  facet_wrap(~ fips_ipeds) + xlab("Ethnicity") + 
  ylab("Difference Between College and Market Percent Demographics") +
  ggtitle(label = "Percent Difference b/w College and Market Vs Ethnicity") +
  theme(axis.text.x = element_text(
    angle = 90,
    vjust = 1,
    hjust = -0.1
  ))

# Table for research question 1
# Summary of statistics for each state
# This will have the min, max, median, mean, and standard deviation of different 
# ethnicities. Excludes Private More Selective as this will impact the data
sum_fryr_cllg_2017 <- fryr_collg_demo_2017 %>% summarise(
  min(dif_white),
  max(dif_white),
  median(dif_white),
  mean(dif_white),
  sd(dif_white),
  min(dif_hispa),
  max(dif_hispa),
  median(dif_hispa),
  mean(dif_hispa),
  sd(dif_hispa),
  min(dif_black),
  max(dif_black),
  median(dif_black),
  mean(dif_black),
  sd(dif_black),
  min(dif_asian),
  max(dif_asian),
  median(dif_asian),
  mean(dif_asian),
  sd(dif_asian),
  min(dif_amind),
  max(dif_amind),
  median(dif_amind),
  mean(dif_amind),
  sd(dif_amind),
  min(dif_pacis),
  max(dif_pacis),
  median(dif_pacis),
  mean(dif_pacis),
  sd(dif_pacis),
  min(dif_twora),
  max(dif_twora),
  median(dif_twora),
  mean(dif_twora),
  sd(dif_twora)
)

# Writes the summary table above to a .txt file
write.table(
  sum_fryr_cllg_2017,
  file = "E:/MTH 3270 Data Science/Midterm Project/resQuest1.txt",
  sep = ",",
  quote = F,
  row.names = T
)

################################################################################
#                 Plots and Tables for Research Question 2                     #
################################################################################

# Plot for research question 2
# x-axis: ethnicity
# y-axis: Difference in percents b/w College demographics and Market demographics
# graph isn't separated by states
# will show the different types of schools by color
fryr_plot + geom_point(mapping = aes(x = dif_pct_race, 
                                     y = dif_in_pct, 
                                     color = fourcat),
                       alpha = 0.5, position = 'jitter') +
  xlab("Ethnicity") + 
  ylab("Percent Difference b/w Market and College Demographics") +
  ggtitle(label = "College-Market Difference of Percent Versus Ethnicity") +
  theme(axis.text.x = element_text(angle = 90))

# Table to put differences between college, market, and difference of the two
coll4_long<-fryr_2017_short%>%
  pivot_longer(cols = dif_white:col_twora)%>%
  mutate(rID = case_when(name=="col_white"|name=="mkt_white"|name=="dif_white"~ 1,
                         name=="col_black"|name=="mkt_black"|name=="dif_black"~ 2,
                         name=="col_hispa"|name=="mkt_hispa"|name=="dif_hispa"~ 3,
                         name=="col_asian"|name=="mkt_asian"|name=="dif_asian"~ 4,
                         name=="col_amind"|name=="mkt_amind"|name=="dif_amind"~ 5,
                         name=="col_pacis"|name=="mkt_pacis"|name=="dif_pacis"~ 6,
                         name=="col_twora"|name=="mkt_twora"|name=="dif_twora"~ 7)) %>% 
  group_by(rID)

# Plot for both research question 1 and research question 2
# x-axis: ethnicity type
# y-axis: percentages of races
# separated by type of school
ggplot(data = coll4_long, mapping=aes(x=reorder(name,rID),y=value,))+
  geom_point(mapping=aes(color=fourcat),alpha=.3,position = "jitter")+
  geom_boxplot(outlier.shape = NA,fill=NA)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0))+
  facet_wrap(~rID,scales = "free") +
  xlab("College, Market, and Difference by Ethnicity Types") + 
  ylab("Percent") + ggtitle(label = "Percentages by Ethnicity")

# Summary table to show min/max of each race
sum_fryr_2017_by_race <- fryr_collg_demo_2017 %>% ungroup() %>% 
  summarise(
            min(dif_white),
            max(dif_white),
            min(dif_hispa),
            max(dif_hispa),
            min(dif_black),
            max(dif_black),
            min(dif_asian),
            max(dif_asian),
            min(dif_amind),
            max(dif_amind),
            min(dif_pacis),
            max(dif_pacis),
            min(dif_twora),
            max(dif_twora)
            )


# Writes the summary table above to a .txt file
write.table(
  sum_fryr_cllg_2017,
  file = "E:/MTH 3270 Data Science/Midterm Project/resQuest2.txt",
  sep = ",",
  quote = F, 
  row.names = T
)


################################################################################
#                 Plots and Tables for Research Question 3                     #
################################################################################

# Setup data frame for graph
fryr_2017_short_dif <- fryr_2017_short_dif %>% group_by(fourcat)

# Setup data frame for table
fryr_2017_type_cllg <- fryr_2017_short %>% group_by(fourcat)

# Plotting of percent difference versus type of school
fryr_plot + geom_point(mapping = aes(x = fourcat, 
                                     y = dif_in_pct, 
                                     color = dif_pct_race),
                       position = 'jitter',
                       alpha = 0.3) +
  ggtitle(label = "Grouped by Institution type displaying Difference between College and Market per Ethnicity") +
  xlab("Ethnicity") + ylab("Percentage") + 
  theme(axis.text.x = element_text(angle = 90))

# Table to summarize data based on type of college including min, max, median
# mean, and standard deviation of each race
sum_type_cllg <- fryr_2017_type_cllg %>% 
  summarise(min(dif_white),
            max(dif_white),
            median(dif_white),
            mean(dif_white),
            sd(dif_white),
            min(dif_hispa),
            max(dif_hispa),
            median(dif_hispa),
            mean(dif_hispa),
            sd(dif_hispa),
            min(dif_black),
            max(dif_black),
            median(dif_black),
            mean(dif_black),
            sd(dif_black),
            min(dif_asian),
            max(dif_asian),
            median(dif_asian),
            mean(dif_asian),
            sd(dif_asian),
            min(dif_amind),
            max(dif_amind),
            median(dif_amind),
            mean(dif_amind),
            sd(dif_amind),
            min(dif_pacis),
            max(dif_pacis),
            median(dif_pacis),
            mean(dif_pacis),
            sd(dif_pacis),
            min(dif_twora),
            max(dif_twora),
            median(dif_twora),
            mean(dif_twora),
            sd(dif_twora)
  )

# Writes the summary table above to a .txt file
write.table(
  sum_fryr_cllg_2017,
  file = "E:/MTH 3270 Data Science/Midterm Project/resQuest3.txt",
  sep = ",",
  quote = F, 
  row.names = T
)
