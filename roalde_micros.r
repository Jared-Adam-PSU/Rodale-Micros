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

# I must add the final trts now
  # 11 = organic manure
  # 22 = organic legume
  # 31 = conventional without cc 
  # 33 = conventional with cc 
# can I use str_detect for the end of a string? 

test <- rodale_trt
test[,'new_trt'] = NA
test %>% 
  mutate(new_trt = case_when(grepl(133, trt)) ~ "CCC")
# I thought this would have worked 
test %>% 
  mutate(new_trt = 'CCC'[grep(33, plot)], 
         new_trt = 'OM'[grep(11, plot)],
         new_trt = 'OL'[grep(22, plot)],
         new_trt = 'CWW'[grep(31, plot)])
# this does not work 
if(grep(33, rodale_trt$plot)){
  mutate(new_trt = 'CCC')
}





###
##
#
# inspecting
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

arth_groups <- rodale_totals[,3:32]

dist <- vegdist(arth_groups, "bray")

permanova_trt <- adonis2(dist ~ trt, permutations = 999, method = "bray", data = rodale_totals)
permanova_trt

permanova_date <- adonis2(dist ~ date, permutations = 999, method = "bray", data = rodale_totals)
permanova_date

permanova_d.t <- adonis2(dist ~ trt*date, permutations = 999, method = "bray", data = rodale_totals)
permanova_d.t

# NMDS ####

# running two fits: k = 2 and 3
ord_2 <- metaMDS(arth_groups, k = 2)
ord_2$stress # 0.23
stressplot(ord_2)
ord_3 <- metaMDS(arth_groups, k = 3)
ord_3$stress # 0.17
stressplot(ord_3)


