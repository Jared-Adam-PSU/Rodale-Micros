# Jared Adam
# Goal: 
  # I have two time points and all of my micros 
  # First: Diversity index? 
  # Second: Permanova and NMDS? 
  # Assign micro scores to these?

# Packages ####

library(tidyverse)
library(vegan)

# Load in data and explore ####
rodale <- as_tibble(Rodale_counts)

# sort by date and look at orb summary
rodale_sort <- rodale %>%
  group_by(date) %>% 
  summarize(
    mean_orb = mean(Orb),
    sd_orb = sd(Orb),
    var_orb = var(Orb)
  )
rodale_sort

# create column for all arthropoda
rodale_totals <- rodale %>% 
  mutate(total_arth = select(.,3:32) %>% # add across all rows from columns 3 through 32
           rowSums(na.rm = TRUE))
rodale_totals$total_arth

rodale_look <- rodale_totals %>% 
  group_by(date, plot) %>% 
  summarise(
    mean = mean(total_arth),
    sd = sd(total_arth),
    var = var(total_arth),
    median = median(total_arth),
    IQr = IQR(total_arth)
  ) 
rodale_look

# rodale_clean <- rodale %>% 
#   mutate_across(as.factor(c('plot', 'date')))


# Permanova ####
