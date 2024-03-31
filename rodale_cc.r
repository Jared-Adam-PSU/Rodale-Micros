# Jared Adam 
# 3/31/2024
# rodale cover crop data
# only have vetch data from trt = 22

# packages #####
library(tidyverse)
library(lme4)
library(emmeans)
library(lmtest)
library(MASS)

# data ####
cc <- cover_crop_data
cc

# wrangling ####
cc_clean <- cc %>% 
  rename(plot = `Plot ID`,
         trt = `Tillage trt`,
         rep = Rep) %>% 
  dplyr::select(plot, trt, rep, weed_bm_no_bag_kg, cc_bm_no_bag_kg) %>% 
  mutate_at(vars(1:3), as.factor) %>% 
  group_by(plot, trt) %>% 
  summarise(avg_weed_rep = mean(weed_bm_no_bag_kg),
            avg_cc_rep = mean(cc_bm_no_bag_kg)) %>% 
  mutate(avg_cc_rep_mg = avg_cc_rep * 0.04)

# stats ####
m1 <- aov(avg_cc_rep_mg ~ trt, cc_clean)
TukeyHSD(m1)

# $trt
# diff         lwr        upr     p adj
# Tilled-No-till 0.0011 -0.00320746 0.00540746 0.5550596