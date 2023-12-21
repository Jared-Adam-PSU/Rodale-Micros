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
  mutate(tillage = c('NT', 'T')[1+str_detect(plot, "^[1 | 3 | 4 | 6]")])

# did this work? : yes
rodale_trt %>% 
  select(plot, tillage) %>% 
  filter(tillage == "NT") %>% 
  arrange(desc(plot)) %>% 
  print(n = Inf)

# I must add the final trts now
  # 11 = organic manure
  # 22 = organic legume
  # 31 = conventional without cc 
  # 33 = conventional with cc 

unique(rodale$plot)
rodale_clean <- rodale_trt %>% 
  mutate(trt = case_when(plot %in% c(111,211,311,411,511,611,711,811) ~ 'OM',
                              plot %in% c(122,222,322,422,522,622,722,822) ~ 'OL',
                              plot %in% c(131,231,331,431,531,631,731,831) ~ 'CWW',
                              plot %in% c(133,233,333,433,533,633,733,833) ~ 'CCC'))
rodale_clean %>% 
  select(tillage, plot, trt) %>% 
  print(n = Inf)

###
##
#
# inspecting
# sort by date and look at orb summary
rodale_clean %>%
  group_by(date, tillage, trt) %>% 
  summarize(
    mean_orb = mean(Orb),
    sd_orb = sd(Orb),
    var_orb = var(Orb)
  )


# create column for all arthropoda
rodale_totals <- rodale_clean %>% 
  mutate(total_arth = select(.,3:32) %>% # add across all rows from columns 3 through 32
           rowSums(na.rm = TRUE))
rodale_totals$total_arth

rodale_totals %>% 
  group_by(date, plot, tillage) %>% 
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


# want to see by date, trt, tillage
permanova_trt <- adonis2(dist ~ trt, permutations = 999, method = "bray", data = rodale_totals)
permanova_trt

permanova_date <- adonis2(dist ~ date, permutations = 999, method = "bray", data = rodale_totals)
permanova_date

permanova_tillage <- adonis2(dist ~ tillage, permutations = 999, method = "bray", data = rodale_totals)
permanova_tillage

permanova_all <- adonis2(dist ~ date*trt*tillage, permutations = 999, method = "bray", data = rodale_totals)
permanova_all



# NMDS ####

# running two fits: k = 2 and 3
ord_2 <- metaMDS(arth_groups, k = 2)
ord_2$stress # 0.23
stressplot(ord_2)
ord_3 <- metaMDS(arth_groups, k = 3)
ord_3$stress # 0.17
stressplot(ord_3)



# Taxon Scores ####

# I need to assign values to each taxon group 
# First, aggregate appropriate columns
  # e.g., diplurans 
# Second, assign appropriate score in a new column 
# Third, compare these scores
  # How?

# 1: aggregate 

colnames(rodale_totals)

rodale_aggregate <- rodale_totals %>% 
  mutate(mites = Orb + Norb,
         diplura = Japy + Camp,
         collembola = Sym + Ento,
         larvae = CL + OL + neuroptera,
         hemiptera = hemip + Enich,
         adult = Adipt + lep + Siphon,
         symph_tot = Simphyla + Scolopendrellida) %>% 
  select(-Orb, -Norb, -Japy, -Camp, -Sym, -Ento, -CL, -OL, -hemip, -Enich,
         -Adipt, -lep, -Siphon, -Simphyla, -Scolopendrellida)
  

# 2: adding the scores
  # this may be a bear
  # individual if_else statements for each column?
    # if(x >= 1) {score = #} else {score = 0}?
    # OR if_else(x >= #, 20) # THIS ONE
# the only concern is with colembolans
# Podo get 20
# ento and sym get 10, in general 
# will combine ento and sym, but keep podo separate
# NEED to revisit coleoptera, for now giving them all 10 and carabids 1
# dip = 5
# chil = 10
# hymen = 1
# form = 5
# ALL larvae = 10
# TEST
test_score <- rodale_aggregate %>% 
  select(mites, plot)

?if_else
test_score %>% 
  mutate(score = if_else(mites >= 1, 20, 0))
#

# REAL
# the order of this is based on the table provided in Parisi et al
colnames(rodale_aggregate)
rodale_scores <- rodale_aggregate %>% 
  mutate(mite_score = if_else(mites >= 1, 20, 0),
         pro_score = if_else(Protura >= 1, 20,0),
         dip_score = if_else(diplura >= 1, 20, 0),
         hemip_score = if_else(hemiptera >= 1, 1, 0), #1 unless cicada larvae 
         thrips_score = if_else(Thrips >= 1, 1, 0),
         coleop_score = if_else(OAC >= 1, 10, 0),
         carabid_score = if_else(AC >= 1, 1, 0),
         hymen_score = if_else(hymen >= 1, 1, 0),
         formic_score = if_else(Formicid >= 1, 5, 0), 
         larvae_score = if_else(larvae >= 1, 10, 0),
         spider_score = if_else(Spider >= 1, 5, 0),
         pseudo_score = if_else(Pseu >= 1, 20, 0), 
         isop_score = if_else(Iso >= 1, 10, 0), 
         chil_score = if_else(Chil >= 1, 10, 0), 
         diplo_score = if_else(Dip >= 1, 5, 0), 
         symph_score = if_else(symph_tot >= 1, 20, 0), 
         eu_ed_col_score = if_else(collembola >= 1, 10, 0), 
         podo_score = if_else(Pod >= 1, 20, 0),
         adult_score = if_else(adult >= 1, 1, 0),
         pauropod_score = if_else(Pauropoda >= 1, 20, 0)) %>% 
   select(-mites, -Protura, -diplura, -hemiptera, -Thrips, -OAC, -AC, -hymen,
          -Formicid, -larvae, -Spider, -Pseu, -Iso, -Chil, -Dip, -symph_tot,
          -collembola, -Pod, -neuroptera, -adult, -Pauropoda, -Annelid)

colnames(rodale_scores)

# adding these columns into one total score column
# combining trt and tillage
rodale_final <- rodale_scores %>% 
  mutate(total_score = select(.,6:25) %>% 
         rowSums(na.rm = TRUE)) %>% 
  select(date, plot, trt, tillage, total_score) %>% 
  mutate(treatment = paste(trt, '-', tillage),
         treatment = as.factor(treatment))

###
##
#
# mean scores
mean_scores <- rodale_final %>%
  select(-tillage, -trt, -plot) %>% 
  arrange(date, treatment)  %>% 
  group_by(treatment, date) %>% 
  dplyr::summarize(avg = mean(total_score)) %>% #plyr has summarize
  print(n = Inf)

ggplot(mean_scores, aes(x = treatment, y = total_score, fill = date))+
  geom_boxplot()

# stats begin: this is done with mean scores df
# ANOVA 
# date is significant 
mean_model_1 <- aov(avg ~ treatment + date, data = mean_scores)
summary(mean_model_1)
hist(residuals(mean_model_1)) # look good
?TukeyHSD
TukeyHSD(mean_model_1)

#nothing sig here becuase date is excluded 
mean_model_2 <- aov(avg ~ treatment, data = mean_scores)
summary(mean_model_2)

glm_mean <- glm(avg ~ treatment + date, data = mean_scores)
summary(glm_mean)
hist(residuals(glm_mean))
#
##
###

# separating trt and tillage
rodale_tillage <- rodale_final %>% 
  select(-plot, -treatment) %>% 
  arrange(date, trt) %>% 
  group_by(date, trt, tillage) %>% 
  dplyr::summarize(avg = mean(total_score)) %>%
  print(n = Inf) 

# stats with tillage separated 
tillage_model <- aov(avg ~ tillage , data = rodale_tillage)
summary(tillage_model)
hist(residuals(tillage_model))
qqnorm(residuals(tillage_model))

glm_tillage <- glm(avg ~ tillage + date, data = rodale_tillage)
summary(glm_tillage)
hist(residuals(glm_tillage))
qqnorm(residuals(glm_tillage))

# Shannon index ####

# following along with a tutorial 
# https://www.flutterbys.com.au/stats/tut/tut13.2.html

colnames(rodale_totals)
rodale_shannon <- rodale_totals %>% 
  mutate(treatment = paste(trt, '-', tillage),
         treatment = as.factor(treatment)) %>% 
  select(-trt, -tillage)
colnames(rodale_shannon)
library(plyr)
# sum up the number of non-zero entries per row
# ignore the first two columns cuz they aint numbas 

?ddply
ddply(rodale_totals, ~trts, function(x){
  data.frame(richness = sum(x[3:33]>0))
})
ddply(rodale_totals,~Sites,function(x) {
  data.frame(RICHNESS=sum(x[3:33]>0))
})

apply(rodale_totals[,3:33]>0,1,sum)
