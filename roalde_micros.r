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

# Need to add in the till, no-till, organic, and conventional treatment identifiers
# Till plots: 200, 500, 700, 800
# No-till plots: 100, 300, 400, 600

###
##
#
# Stuff I tried that worked but I did not use for final code 
?case_when
?str_sub
?str_detect
# going to add a blank column for trt then add case_when 
# test <- rodale
# test[, 'trt'] = NA
# 
# # testing out str_sub to find locations 
# str_sub(rodale$plot, start = 1 | 3, end = 1 | 3) == c(1,3)
# rm(test_2)
# # using str_sub_all
# test_2 <- test %>% 
#   mutate(trt = case_when(
#    str_sub_all(plot, start = 1, end = 1) == 1 ~ "NT",
#    
#   ))
# # did this work? 
# test_2 %>% 
#   select(plot, trt) %>% 
#   print(n = Inf)
#
##
###


# This is the route I am going with 
rodale_trt <- rodale %>%
  mutate(trt = c('NT', 'T')[1+str_detect(plot, "^[1 | 3 | 4 | 6]")])

# did this work? : yes
rodale_trt %>% 
  select(plot, trt) %>% 
  filter(trt == "NT") %>% 
  arrange(desc(plot)) %>% 
  print(n = Inf)


# sort by date and look at orb summary
rodale_trt %>%
  group_by(date, trt) %>% 
  summarize(
    mean_orb = mean(Orb),
    sd_orb = sd(Orb),
    var_orb = var(Orb)
  )


# create column for all arthropoda
rodale_totals <- rodale_trt %>% 
  mutate(total_arth = select(.,3:32) %>% # add across all rows from columns 3 through 32
           rowSums(na.rm = TRUE))
rodale_totals$total_arth

rodale_totals %>% 
  group_by(date, plot, trt) %>% 
  summarise(
    mean = mean(total_arth),
    sd = sd(total_arth),
    var = var(total_arth),
    median = median(total_arth),
    IQr = IQR(total_arth)
  ) 

# rodale_clean <- rodale %>% 
#   mutate_across(as.factor(c('plot', 'date')))


# Permanova ####
