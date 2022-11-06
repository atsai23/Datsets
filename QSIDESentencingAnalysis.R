# Tidyverse has everything we need for the data analysis
# install.packages("tidyverse")
library(tidyverse)

# If we want to plot data, ggplot2 is needed
# install.packages("ggplot2")
library(ggplot2)

montana_data_demographics <- read.csv(file = "/Users/bfijabi/Downloads/Copy of MT_COR_Database.xlsx - Offender Demographics.csv")
montana_data_names <- read.csv(file = "/Users/bfijabi/Downloads/Copy of MT_COR_Database.xlsx - Offender Names.csv")
montana_data_legal <- read.csv(file = "/Users/bfijabi/Downloads/Copy of MT_COR_Database.xlsx - Offender Legal.csv")

# Looking at our individual data sheets, we see that they all contain a DOC_ID
# row. It seems like a good idea to join them together!

montana_data_full <- montana_data_demographics %>%
  inner_join(montana_data_legal, by = "DOC_ID")

# The research question that I want to answer: 
# Is there a disparity observed w.r.t. people of color?

# First, get a total count of sentences by race
total_count_by_race <- montana_data_full %>%
  count(RACE)

# One way to measure that is through grouping rows by race:
montana_data_full_grouped_by_race_crime <- montana_data_full %>%
  group_by(OFFENSE, COUNTS, RACE, .add = TRUE)

# And then summarizing the TOTAL_SENTENCED_MONTHS column
c <- montana_data_full_grouped_by_race_crime %>%
  summarise(mean = mean(TOTAL_SENTENCED_MONTHS, na.rm = TRUE), std = sd(TOTAL_SENTENCED_MONTHS, na.rm = TRUE))

# Count all offenses/counts
counts_off_cts <- montana_data_full_grouped_by_race_crime %>%
  count(OFFENSE, COUNTS)

counts_off_cts$n

c$sample <- counts_off_cts$n

# Save c as a csv
write.csv(c, "/Users/bfijabi/Downloads/c.csv", row.names = FALSE)

# Thought experiment: 
montana_data_full_grouped_by_race_judge <- montana_data_full %>%
  group_by(JUDGE, RACE, .add = TRUE)

# Count all races/judges
counts_off_jdg_rce <- montana_data_full_grouped_by_race_judge %>%
  count(JUDGE, RACE) 

# And then summarizing the TOTAL_SENTENCED_MONTHS column
q <- montana_data_full_grouped_by_race_judge %>%
  summarise(mean = mean(TOTAL_SENTENCED_MONTHS, na.rm = TRUE), std = sd(TOTAL_SENTENCED_MONTHS, na.rm = TRUE))
 
q$sample <- counts_off_jdg_rce$n

# Save q as a csv
write.csv(q, "/Users/bfijabi/Downloads/q.csv", row.names = FALSE)
