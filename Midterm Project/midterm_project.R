library(dplyr)
library(ggplot2)
library(tidyr)

# Reading file into dataframe
four_year.file <- file.choose()
four_year <-
  read.csv(
    four_year.file,
    header = TRUE,
    sep = ",",
    stringsAsFactors = FALSE
  )

# Filter data frame above for 2017 year only
four_year_2017 <- filter(four_year, year == 2017)
View(four_year_2017)

# Grouping based on state
grp_fryr_2017 <- group_by(.data = four_year_2017, fips_ipeds)
View(grp_fryr_2017)

grp_fryr_2017 <-
  pivot_longer(
    grp_fryr_2017,
    c(
      mkt_white,
      col_white,
      mkt_hispa,
      col_hispa,
      mkt_black,
      col_black,
      mkt_asian,
      col_asian,
      mkt_amind,
      col_amind,
      mkt_pacis,
      col_pacis,
      mkt_twora,
      col_twora
    )
  )

# Plot of Markets with College Demographic Percentages
ggplot(data = grp_fryr_2017,
       mapping = aes(x = name, y = value, color = name)) +
  geom_boxplot(alpha = 0.2) +
  theme(axis.text.x = element_text(
    angle = 90,
    vjust = 1,
    hjust = 0
  )) +
  xlab("Demographic of ethnicity") + ylab("Percent of Ethnicity") +
  facet_wrap( ~ fourcat)

# Stats for demographics including min, max, median, mean
sum_stat_fryr_2017 <- summarise(
  .data = four_year_2017,
  mean(mkt_white),
  mean(col_white),
  mean(mkt_hispa),
  mean(col_hispa),
  mean(mkt_black),
  mean(col_black),
  mean(mkt_asian),
  mean(col_asian),
  mean(mkt_amind),
  mean(col_amind),
  mean(mkt_pacis),
  mean(col_pacis),
  mean(mkt_twora),
  mean(col_twora),
  median(mkt_white),
  median(col_white),
  median(mkt_hispa),
  median(col_hispa),
  median(mkt_black),
  median(col_black),
  median(mkt_asian),
  median(col_asian),
  median(mkt_amind),
  median(col_amind),
  median(mkt_pacis),
  median(col_pacis),
  median(mkt_twora),
  median(col_twora),
  min(mkt_white),
  min(col_white),
  min(mkt_hispa),
  min(col_hispa),
  min(mkt_black),
  min(col_black),
  min(mkt_asian),
  min(col_asian),
  min(mkt_amind),
  min(col_amind),
  min(mkt_pacis),
  min(col_pacis),
  min(mkt_twora),
  min(col_twora),
  max(mkt_white),
  max(col_white),
  max(mkt_hispa),
  max(col_hispa),
  max(mkt_black),
  max(col_black),
  max(mkt_asian),
  max(col_asian),
  max(mkt_amind),
  max(col_amind),
  max(mkt_pacis),
  max(col_pacis),
  max(mkt_twora),
  max(col_twora)
)

View(sum_stat_fryr_2017)

#ggplot(data = grp_fryr_2017) +
#  geom_point(
#    mapping = aes(x = mkt_white, y = col_white),
#    alpha = 0.2,
#    color = 'blue'
#  ) +
#  geom_point(
#    mapping = aes(x = mkt_hispa, y = col_hispa),
#    alpha = 0.2,
#    color = 'red'
#  ) +
#  geom_point(
#    mapping = aes(x = mkt_black, y = col_black),
#    alpha = 0.2,
#    color = 'orange'
#  ) +
#  geom_point(
#    mapping = aes(x = mkt_asian, y = col_asian),
#    alpha = 0.2,
#    color = 'green'
#  ) +
#  geom_point(
#    mapping = aes(x = mkt_amind, y = col_amind),
#    alpha = 0.2,
#    color = 'cyan'
#  ) +
#  geom_point(
#    mapping = aes(x = mkt_pacis, y = col_pacis),
#    alpha = 0.2,
#    color = 'purple'
#  ) +
#  geom_point(
#    mapping = aes(x = mkt_twora, y = col_twora),
#    alpha = 0.2,
#    color = 'gray'
#  ) +
#  facet_wrap(~ fourcat) +
#  ylab("College Percent per Ethnicity") + xlab("Market Percent per Ethnicity") +
#  scale_color_manual(
#    name = 'Demographics',
#    breaks = c(
#      'mkt_white',
#      'mkt_hispa',
#      'mkt_black',
#      'mkt_asian',
#      'mkt_amind',
#      'mkt_pacis',
#      'mkt_twora'
#    ),
#    values = c(
#      'mkt_white' = 'blue',
#      'mkt_hispa' = 'red',
#      'mkt_black' = 'orange',
#      'mkt_asian' = 'green',
#      'mkt_amind' = 'cyan',
#      'mkt_pacis' = 'purple',
#      'mkt_twora' = 'gray'
#    )
#  )

# ggplot(data = grp_fryr_2017) +
#  geom_histogram(mapping = aes(x = c(col_white:dif_twora)))

# Summary of stats for differences in question 2
sum_stat_dif_fryr_2017 <-
  summarise(
    .data = four_year_2017,
    min(dif_white),
    min(dif_hispa),
    min(dif_black),
    min(dif_asian),
    min(dif_amind),
    min(dif_pacis),
    min(dif_twora),
    max(dif_white),
    max(dif_hispa),
    max(dif_black),
    max(dif_asian),
    max(dif_amind),
    max(dif_pacis),
    max(dif_twora)
  )

View(sum_stat_dif_fryr_2017)

# adding columns for market percentages and college percentages
coll4dat2017c <-
  four_year %>%  mutate(mkt_Sum = rowSums(.[c(
    "mkt_white",
    "mkt_hispa",
    "mkt_black",
    "mkt_asian",
    "mkt_amind",
    "mkt_pacis",
    "mkt_twora"
  )]), col_Sum = rowSums(.[c(
    "col_white",
    "col_hispa",
    "col_black",
    "col_asian",
    "col_amind",
    "col_pacis",
    "col_twora"
  )]))

coll4dat2017c <-
  coll4dat2017c %>% mutate(dif_Sum = col_Sum - mkt_Sum)

# Plots for research question 2
coll4dat2017c %>% select(starts_with("dif"), fourcat) %>% group_by(fourcat) %>%
  pivot_longer(dif_white:dif_twora) %>% ggplot() + geom_boxplot(aes(x = value, y =
                                                                      name), alpha = .2) + facet_wrap(~ fourcat)
#  Plots for research question 3
coll4dat2017c %>% select(starts_with("dif"), fourcat) %>% group_by(fourcat) %>%
  pivot_longer(dif_white:dif_twora) %>% ggplot() + geom_boxplot(aes(x = value, y =
                                                                      name), alpha = .2)

