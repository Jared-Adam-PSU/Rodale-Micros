# Jared Adam
# Goal: 
  # I have two time points and all of my micros 
  # First: Diversity index? 
  # Second: Permanova and NMDS? 
  # Assign micro scores to these?

# Packages ####

library(tidyverse)
library(vegan)
library(lme4)
library(performance)
library(MASS)
library(lmtest)
library(emmeans)
library(multcomp)
library(flextable)

# Load in data ####
rodale <- as_tibble(Rodale_counts)

grain <- grain_yield

# and explore ####
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
#   dplyr::select(plot, trt) %>% 
#   print(n = Inf)
#
##
###


# This is the route I am going with 
rodale_trt <- rodale %>%
  mutate(tillage = c('T', 'NT')[1+str_detect(plot, "^[1 | 3 | 4 | 6]")])

# did this work? : yes
rodale_trt %>% 
  dplyr::select(plot, tillage) %>% 
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
  dplyr::select(tillage, plot, trt) %>% 
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
  mutate(total_arth = dplyr::select(.,3:32) %>% # add across all rows from columns 3 through 32
           rowSums(na.rm = TRUE)) 
rodale_totals$total_arth

rodale_totals %>% 
  summarise(total = sum(total_arth))
# 2556


rodale_totals %>% 
  group_by(date, plot, tillage) %>% 
  summarise(
    mean = mean(total_arth),
    sd = sd(total_arth),
    var = var(total_arth),
    median = median(total_arth),
    IQr = IQR(total_arth)
  ) 



# Abundance cleaning ####
abund_models <- rodale_totals %>% 
  dplyr::select(date, plot, tillage, trt, total_arth) %>% 
  mutate_at(vars(1:4), as.factor)

# for paper 
colnames(rodale_totals)
table <- rodale_totals %>%
  dplyr::select(-plot, -total_arth, - trt) %>% 
  mutate(Acari = Orb + Norb,
         Diplura = Japy + Camp,
         'Other larvae' = OL + neuroptera,
         Hemiptera = hemip + Enich,
         Simphyla = Simphyla + Scolopendrellida
         ) %>%
  rename('Hemi-Eudaphic Collembola' = Sym,
         'Epigeic Collembola' = Ento,
         'Eudpahic Collembola' = Pod,
         'Coleopter larave' = CL,
         Diptera = Adipt,
         Lepidoptera = lep,
         Siphonoptera = Siphon,
         Carabidae = AC,
         'Other Coleoptera' = OAC,
         Thysanoptera = Thrips,
         Hymenoptera = hymen,
         'Chilopod < 5mm' = Chil,
         'Diplopod < 5mm' = Dip,
         Pseudoscorpion = Pseu,
         Isopoda = Iso
  ) %>% 
  dplyr::select(-Orb, -Norb, -Japy, -Camp, -OL, -hemip, -Enich,
                  -Simphyla, -Scolopendrellida,
                 -neuroptera)

paper <- table %>% 
  pivot_longer(
    cols = where(is.numeric)
  ) %>% 
  group_by(date, tillage, name) %>% 
  summarise(total = sum(value)) %>% 
  mutate(date = case_when(date == '10/11/2023' ~ "11 October 2023",
                          date == '7/28/2023' ~ "28 July 2023")) %>% 
  print(n = Inf)

paper <- paper %>% 
  pivot_wider(names_from = name, 
              values_from = total, 
              values_fn = list(family = length))

fp <- flextable(paper) %>% 
  set_header_labels(values = list(
    date = 'Date',
    tillage = 'Tillage'
  ))

fp <- theme_zebra(fp)
autofit(fp) %>% 
  save_as_docx(path = 'rodaleabund.docx')



# Abundance stats ####
abund_models

a0 <- glm(total_arth ~ trt, 
                      data = abund_models)

a1 <- glm(total_arth ~ trt + tillage, 
          data = abund_models)

a2<- glm(total_arth ~ trt + tillage + date, 
          data = abund_models)

a3 <- glm(total_arth ~ tillage + date, 
          data = abund_models)
anova(a0,a1,a2,a3)

hist(residuals(a3))
summary(a3)
cld(emmeans(a3, ~tillage + date), Letters = letters)
# tillage date       emmean   SE df lower.CL upper.CL .group
# T       10/11/2023   17.8 6.67 60      4.4     31.1  a    
# NT      10/11/2023   34.3 6.82 60     20.6     47.9  ab   
# T       7/28/2023    46.7 6.67 60     33.3     60.0   bc  
# NT      7/28/2023    63.2 6.67 60     49.8     76.5    c  



# Abundance plots ####
abund_models %>% 
  group_by(date) %>% 
  summarise(mean = mean(total_arth),
            sd = sd(total_arth),
            n = n(),
            se = sd/sqrt(n))


abund_plot <- abund_models %>% 
  group_by(tillage, date) %>% 
  summarise(mean = mean(total_arth),
            sd = sd(total_arth),
            n = n(),
            se = sd/sqrt(n))

ggplot(abund_plot, aes(x = date, y = mean, fill = tillage))+
  geom_bar(stat = 'identity', position = 'dodge', alpha = 0.7,
           aes(x = factor(date, level = c('7/28/2023', '10/11/2023'))))+
  geom_errorbar(aes(x = date, ymin = mean-se, ymax = mean+se), 
                position = position_dodge(0.9),
                width = 0.4, linewidth = 1.3)+
  scale_fill_manual(values = c("#E7298A", "#7570B3"),
                    name = 'Tillage type', labels = c('No-Till', "Till"))+
  scale_x_discrete(labels = c('28 July 2023','11 October 2023'))+
  labs(title = 'Abundance by date and till',
       x = 'Sampling date', 
       y = 'Average abundance')+
  theme(legend.position = 'bottom',
        legend.key.size = unit(.5, 'cm'), 
        legend.text = element_text(size = 24),
        legend.title = element_text(size = 24),
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24),
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())+
  annotate('text', x = 0.775, y = 85, label = 'a', size = 10)+
  annotate('text', x = 1.225, y = 85, label = 'ab', size = 10)+
  annotate('text', x = 1.775, y = 85, label = 'bc', size = 10)+
  annotate('text', x = 2.225, y = 85, label = 'c', size = 10)




# yield x abundance ####
grain_clean <- grain %>% 
  group_by(plot) %>% 
  summarise(mean_yield = mean(grain_g)) %>% 
  mutate(trt = case_when(plot %in% c(122,222,322,422,522,622,722,822) ~ 'OL',
                         plot %in% c(131,231,331,431,531,631,731,831) ~ 'CWW',
                         plot %in% c(133,233,333,433,533,633,733,833) ~ 'CCC')) %>% 
  relocate(plot, trt) %>% 
  mutate_at(vars(1:2), as.factor) %>% 
  arrange(trt) %>% 
  print(n = Inf)

g1 <- aov(mean_yield ~ trt, data = grain_clean)
TukeyHSD(g1)
hist(residuals(g1))
# $trt
# diff        lwr       upr     p adj
# CWW-CCC  0.1266667 -0.2637572 0.5170906 0.6963964
# OL-CCC  -0.1100000 -0.5004239 0.2804239 0.7602369
# OL-CWW  -0.2366667 -0.6270906 0.1537572 0.2985669

am_reg <- abund_models %>% 
  filter(trt != 'OM') %>% 
  group_by(plot) %>% 
  summarise(mean_arth = mean(total_arth)) %>% 
  rename(tret = plot) %>% 
  print(n = Inf)

ga_reg <- grain %>% 
  mutate(trt = case_when(plot %in% c(122,222,322,422,522,622,722,822) ~ 'OL',
                         plot %in% c(131,231,331,431,531,631,731,831) ~ 'CWW',
                         plot %in% c(133,233,333,433,533,633,733,833) ~ 'CCC')) %>%  
  group_by(plot) %>% 
  summarise(mean_g = mean(grain_g)) %>% 
  print(n = Inf)

ab_reg <- cbind(am_reg, ga_reg) %>% 
  dplyr::select(-tret)

r1 <- glm(mean_g ~ mean_arth, data = ab_reg)
hist(residuals(r1))
summary(r1)

ggplot(ab_reg, aes(x = mean_g, y = mean_arth))+
  geom_point()



# Scores cleaning ####

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
         #collembola = Sym + Ento,
         larvae = CL + OL + neuroptera,
         hemiptera = hemip + Enich,
         adult = Adipt + lep + Siphon,
         symph_tot = Simphyla + Scolopendrellida,
         coleop = OAC + AC) %>% 
  dplyr::select(-Orb, -Norb, -Japy, -Camp, -CL, -OL, -hemip, -Enich,
         -Adipt, -lep, -Siphon, -Simphyla, -Scolopendrellida, -OAC, -AC)
  

# 2: adding the scores
  # this may be a bear
  # individual if_else statements for each column?
    # if(x >= 1) {score = #} else {score = 0}?
    # OR if_else(x >= #, 20) # THIS ONE

# old: 
# the only concern is with colembolans
# Podo get 20
# ento and sym get 10, in general
# will combine ento and sym, but keep podo separate 
# 

# new: 
# 1/18/2024 revision: Take out entomo and make them 6 points(typically hemi)
# Podo get 20
# sym get 10 
# ento get 6 
# coleop get 1 because they are all big? 4/12/2024

# old note
# NEED to revisit coleoptera, for now giving them all 10 and carabids 1
# dip = 5
# chil = 10
# hymen = 1
# form = 5
# ALL larvae = 10
# TEST
test_score <- rodale_aggregate %>% 
  dplyr::select(mites, plot)

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
         coleop_score = if_else(coleop >= 1, 1, 0),
         hymen_score = if_else(hymen >= 1, 1, 0),
         formic_score = if_else(Formicid >= 1, 5, 0), 
         larvae_score = if_else(larvae >= 1, 10, 0),
         spider_score = if_else(Spider >= 1, 5, 0),
         pseudo_score = if_else(Pseu >= 1, 20, 0), 
         isop_score = if_else(Iso >= 1, 10, 0), 
         chil_score = if_else(Chil >= 1, 10, 0), 
         diplo_score = if_else(Dip >= 1, 5, 0), 
         symph_score = if_else(symph_tot >= 1, 20, 0), 
         hemi_ed_score = if_else(Ento >= 1, 6, 0),
         hemi_eu_ed_col_score = if_else(Sym >= 1, 10, 0), 
         ed_col_score = if_else(Pod >= 1, 20, 0),
         adult_score = if_else(adult >= 1, 1, 0),
         pauropod_score = if_else(Pauropoda >= 1, 20, 0)) %>% 
   dplyr::select(-mites, -Protura, -diplura, -hemiptera, -Thrips, -coleop, -hymen,
          -Formicid, -larvae, -Spider, -Pseu, -Iso, -Chil, -Dip, -symph_tot,
          -Ento, -Sym, -Pod, -neuroptera, -adult, -Pauropoda, -Annelid)

colnames(rodale_scores)

unique(rodale_scores$plot)
# adding these columns into one total score column
# combining trt and tillage
rodale_final <- rodale_scores %>% 
  mutate(total_score = dplyr::select(.,6:25) %>% 
         rowSums(na.rm = TRUE)) %>% 
  dplyr::select(date, plot, trt, tillage, total_score) %>% 
  mutate(treatment = paste(trt, '-', tillage),
         treatment = as.factor(treatment)) %>% 
  mutate(block = case_when(plot %in% c(111,122,131,133) ~ 1,
                           plot %in% c(211,222,231,233) ~ 2, 
                           plot %in% c(311,322,331,333) ~ 3, 
                           plot %in% c(411,422,431,433) ~ 4,
                           plot %in% c(511,522,531,533) ~ 5,
                           plot %in% c(611,622,631,633) ~ 6, 
                           plot %in% c(711,722,731,733) ~ 7, 
                           plot %in% c(811,822,831,833) ~ 8))


score_table <- rodale_final %>% 
  group_by(date, tillage) %>% 
  summarise(avg = mean(total_score), 
            sd = sd(total_score),
            n = n(),
            se = sd/sqrt(n))

overall <- flextable(score_table)
overall <- theme_zebra(overall)
autofit(overall) %>% 
  save_as_docx(path = 'overall_scores.docx')


###
##
#
# mean scores
mean_scores <- rodale_final %>%
  dplyr::select(-plot) %>% 
  group_by(trt, tillage, date) %>% 
  summarise(avg = mean(total_score),
            sd = sd(total_score),
            n = n(), 
            se = sd/sqrt(n)) %>% 
  arrange(date, tillage) %>% 
  print(n = Inf)

ggplot(mean_scores, aes(x = treatment, y = avg, fill = date))+
  geom_boxplot()


mean_overall <- flextable(mean_scores)
mean_overall <- theme_zebra(mean_overall)
autofit(mean_overall) %>% 
  save_as_docx(path = 'mean_overall_scores.trt.till.docx')


# Score stats ####

# need to add a blocking factor (added above with rodale final)
rodale_models <- rodale_final %>% 
  dplyr::select(date, plot, total_score, treatment, block) %>% 
  mutate(date = as.factor(date), 
         plot = as.factor(plot), 
         block = as.factor(block))


glm1 <- glm(total_score ~ treatment + date, data = rodale_models)
summary(glm1)
hist(residuals(glm1))
cld(emmeans(glm1, ~treatment), Letters= letters) # no differences among treatment
# date       emmean   SE df lower.CL upper.CL .group
# 10/11/2023   52.1 4.22 54     43.6     60.5  a    
# 7/28/2023    75.5 4.14 54     67.2     83.8   b 


# look at overdispersion: variance > mean?
dispersion_stats <- rodale_models %>% 
  group_by(treatment) %>%
  summarise(
  mean = mean(total_score),
  variances = var(total_score),
  ratio = variances/mean)


# let's see which is better, poisson or nb? 
# run one of each where the only difference is the family 

poisson_model <- glmer(total_score ~ treatment + 
                       (1|block) + (1|date), 
                     data = rodale_models, 
                    family = poisson)

nb_model_trt <- glmer.nb(total_score ~ treatment + (1|date), 
                     data = rodale_models) 

lrtest(poisson_model,nb_model_trt)
# the negative binomial has the higher likelihood score, so we will use that

m0 <- glmer.nb(total_score ~  (1|date), 
               data = rodale_models) 

m1 <- glmer.nb(total_score ~ treatment + (1|date), 
                         data = rodale_models) 
anova(m0,m1)
# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# m0    3 606.04 612.47 -300.02   600.04                     
# m1   10 614.38 635.81 -297.19   594.38 5.6617  7     0.5798
summary(m1)
hist(residuals(m1))
r2_nakagawa(m1) 
rodale_residuals <- binned_residuals(m1)
plot(rodale_residuals)
cld(emmeans(m1, ~treatment), Letters = letters)
# treatment emmean    SE  df asymp.LCL asymp.UCL .group
# OL - NT     3.81 0.188 Inf      3.44      4.18  a    
# CCC - NT    4.05 0.186 Inf      3.68      4.41  a    
# OM - T      4.15 0.195 Inf      3.77      4.53  a    
# CWW - T     4.15 0.186 Inf      3.78      4.51  a    
# OM - NT     4.18 0.186 Inf      3.82      4.55  a    
# CWW - NT    4.22 0.185 Inf      3.85      4.58  a    
# OL - T      4.24 0.185 Inf      3.87      4.60  a    
# CCC - T     4.27 0.186 Inf      3.90      4.63  a 

#
##
###

# separating trt and tillage
rodale_tillage <- rodale_final %>% 
  dplyr::select(-plot, -treatment) %>% 
  arrange(date, trt) %>% 
  group_by(date, trt, tillage) %>%
  print(n = Inf) 

glm_tillage <- glm(total_score ~ tillage + date, data = rodale_tillage)
summary(glm_tillage)
hist(residuals(glm_tillage))
qqnorm(residuals(glm_tillage))
cld(emmeans(glm_tillage, ~ date), Letters = letters)
# date       emmean   SE df lower.CL upper.CL .group
# 10/11/2023   52.2 4.23 60     43.8     60.7  a    
# 7/28/2023    75.5 4.16 60     67.1     83.8   b 


# glmer for trt
# this seems like a bad model, based on the r2 and other performance analytics
rodale_tillage_glmer <- rodale_final %>% 
  dplyr::select(-treatment) %>% 
  relocate(date, trt, block) %>% 
  mutate_at(vars(1:5), as.factor)

t0 <- glmer.nb(total_score ~ 
                 (1|date/block), 
               data = rodale_tillage_glmer)

t1 <- glmer.nb(total_score ~ tillage + 
                 (1|date/block), 
               data = rodale_tillage_glmer)

t2 <- glmer.nb(total_score ~ tillage + trt + 
                 (1|date/block), 
                       data = rodale_tillage_glmer)

t3 <- glmer.nb(total_score ~ tillage*trt + 
                 (1|date/block), 
               data = rodale_tillage_glmer)
anova(t0,t1,t2,t3)
# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# t0    4 607.61 616.19 -299.81   599.61                     
# t1    5 608.77 619.49 -299.39   598.77 0.8392  1     0.3596
# t2    8 613.49 630.64 -298.75   597.49 1.2798  3     0.7339
# t3   11 615.56 639.13 -296.78   593.56 3.9397  3     0.2680

summary(m2)
r2_nakagawa(m2)
binned_residuals(m2)
cld(emmeans(m2, ~tillage*trt), Letters= letters)
# tillage trt emmean    SE  df asymp.LCL asymp.UCL .group
# NT      OL    3.78 0.195 Inf      3.40      4.16  a    
# NT      CCC   4.05 0.191 Inf      3.67      4.42  a    
# T       OM    4.10 0.202 Inf      3.70      4.49  a    
# T       CWW   4.13 0.191 Inf      3.76      4.51  a    
# NT      OM    4.18 0.190 Inf      3.80      4.55  a    
# T       OL    4.21 0.191 Inf      3.83      4.58  a    
# NT      CWW   4.21 0.190 Inf      3.84      4.59  a    
# T       CCC   4.24 0.191 Inf      3.86      4.61  a   



# Score plots ####
rodale_tillage %>%
  group_by(trt, tillage) %>% 
  summarise(mean = mean(total_score), 
            sd = sd(total_score), 
            n = n(),
            se = sd/sqrt(n)) %>% 
  mutate_at(vars(1:3), as.factor) %>% 
  ggplot(aes(x = trt, y = mean))+
  geom_bar(stat = 'identity', position = 'dodge') +
  facet_wrap(~tillage)

date_fig <- rodale_models %>%
  relocate(date, plot, treatment, block) %>% 
  group_by( date) %>% 
  summarise(mean = mean(total_score), 
            sd = sd(total_score), 
            n = n(),
            se = sd/sqrt(n)) 

ggplot(date_fig, aes(x = date, y = mean, fill = date))+
  geom_bar(stat = 'identity', position = 'dodge', alpha = 0.7,
           aes(x = factor(date, level = c('7/28/2023', '10/11/2023'))))+
  geom_errorbar(aes(x = date, ymin = mean-se, ymax = mean+se), 
                position = position_dodge(0.9),
                width = 0.4, linewidth = 1.3)+
  scale_fill_manual(values = c("#E7298A", "#7570B3"))+
  scale_x_discrete(labels = c('28 July 2023','11 October 2023'))+
  labs(title = 'Rodale scores by date', 
       y = 'Average QBS scores',
       x = 'Samplting date')+
  theme(legend.position = 'none',
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24),
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())+
  annotate('text', x = 1, y = 83, label = 'a', size = 10)+
  annotate('text', x = 2, y = 83, label ='b', size = 10)



# yield x score ####

gs_reg <- grain %>% 
  mutate(trt = case_when(plot %in% c(122,222,322,422,522,622,722,822) ~ 'OL',
                         plot %in% c(131,231,331,431,531,631,731,831) ~ 'CWW',
                         plot %in% c(133,233,333,433,533,633,733,833) ~ 'CCC')) %>%  
  group_by(plot) %>% 
  summarise(mean_g = mean(grain_g)) %>% 
  rename(ploot = plot) %>% 
  print(n = Inf)

sc_reg <- rodale_final %>% 
  filter(trt != 'OM') %>% 
  group_by(plot) %>% 
  summarise(mean_score = mean(total_score)) %>% 
  print(n = Inf)

scr_reg <- cbind(gs_reg, sc_reg) %>% 
  dplyr::select(-ploot)
s1 <- glm(mean_g ~ mean_score, data = scr_reg)
hist(residuals(s1))
summary(s1)

ggplot(scr_reg, aes(x = mean_g, y = mean_score))+
  geom_point()


# Permanova ####
arth_groups <- rodale_totals[,3:32]

dist <- vegdist(arth_groups, "bray")


# want to see by date, trt, tillage

permanova_all <- adonis2(dist ~ date + trt + tillage, permutations = 999, method = "bray", data = rodale_totals)
permanova_all
# Df SumOfSqs      R2      F Pr(>F)   
# date      1   0.6909 0.05133 3.3101  0.004 **
#   trt       3   0.6455 0.04796 1.0308  0.383   
# tillage   1   0.2257 0.01677 1.0812  0.330   
# Residual 57  11.8976 0.88394                 
# Total    62  13.4597 1.00000


# NMDS ####
# these are all 3d and no differences by trt, so not including them. 


# going to use taxon score df 

# rodale_nmds<- rodale_clean %>% 
# mutate(treatment = paste(trt, '-', tillage),
#        treatment = as.factor(treatment))
# running two fits: k = 2 and 3
colnames(rodale_nmds)

colnames(rodale_aggregate)
rodale_ord_df <- rodale_aggregate %>% 
  dplyr::select(-total_arth, -neuroptera) %>% 
  dplyr::rename('Eu-edaphic collembola' = Pod,
                'Hemi-eu-edaphic collembola' = Sym,
                'Hemi-edpahic collembola' = Ento,
                'Diplopoda' = Dip, 
                'Chilopoda' = Chil,
                'Araneae' = Spider,
                'Coleoptera' = coleop,
                'Hymenoptera' = hymen,
                'Acari' = mites,
                'Diplura' = diplura,
                'Larvae' = larvae,
                'Hemiptera' = hemiptera,
                'Holo Insects' = adult,
                'Thysanoptera' = Thrips,
                'Symphyla' = symph_tot,
                
                ) %>% 
  mutate(treatment = paste(trt, '-', tillage),
                 treatment = as.factor(treatment)) %>% 
  dplyr::select(-trt, -tillage)
colnames(rodale_ord_df)

new_arth_groups <- rodale_ord_df[,3:23]


ord_2 <- metaMDS(new_arth_groups, k = 2)
ord_2$stress # 0.24
stressplot(ord_2)

# going to use this one
ord_3 <- metaMDS(new_arth_groups, k = 3)
ord_3$stress # 0.167
stressplot(ord_3)


# need to get site scores for ordination
# I think I want display  = "sites"
?scores
scrs <- scores(ord_3, display = "sites")
# adding my scores from metaMDS to their associated trts 
scrs <- cbind(as.data.frame(scrs), trt = rodale_nmds$treatment)
scrs <- cbind(as.data.frame(scrs), date = rodale_nmds$date)

# i want to add functional group to this df 
# "species" = averaged site scores
# as_tibble here gets rid of the name and replaces the groups with numbers != what I want
functional_scores <- as.data.frame(scores(ord_3, "species"))
functional_scores$species <- rownames(functional_scores)

unique(rodale_nmds$treatment)
# going to chull the objects to get trts into their own shapes
CWW_NT <- scrs[scrs$trt == "CWW - NT",][chull(scrs[scrs$trt == "CWW - NT",c("NMDS1", "NMDS2", "NMDS3")]),]
CWW_T <- scrs[scrs$trt == "CWW - T",][chull(scrs[scrs$trt == "CWW - T",c("NMDS1", "NMDS2","NMDS3")]),]
OL_T <- scrs[scrs$trt == "OL - T",][chull(scrs[scrs$trt == "OL - T",c("NMDS1", "NMDS2","NMDS3")]),]
OL_NT <- scrs[scrs$trt == "OL - NT",][chull(scrs[scrs$trt == "OL - NT",c("NMDS1", "NMDS2","NMDS3")]),]
OM_T <- scrs[scrs$trt == "OM - T",][chull(scrs[scrs$trt == "OM - T",c("NMDS1", "NMDS2","NMDS3")]),]
OM_NT <- scrs[scrs$trt == "OM - NT",][chull(scrs[scrs$trt == "OM - NT",c("NMDS1", "NMDS2","NMDS3")]),]
CCC_NT <- scrs[scrs$trt == "CCC - NT",][chull(scrs[scrs$trt == "CCC - NT",c("NMDS1", "NMDS2","NMDS3")]),]
CCC_T <- scrs[scrs$trt == "CCC - T",][chull(scrs[scrs$trt == "CCC - T",c("NMDS1", "NMDS2","NMDS3")]),]


hull.data <- rbind(CWW_NT, CWW_T, CCC_NT,CCC_T,OL_NT,OL_T,OM_NT, OM_T)
as_tibble(hull.data)  #trt = factor
hull.data$trt <- as.factor(hull.data$trt)
library(ggrepel)
library(RColorBrewer)

display.brewer.all(colorblindFriendly = TRUE)


nmds_all <- ggplot()+
  geom_polygon(data = hull.data, (aes(x = NMDS1, y = NMDS2, group = trt, fill = trt)), alpha = 0.5)+
  scale_fill_brewer(palette = 'Dark2')+
  # scale_fill_manual(name = "Treatment", labels = c('CWW_NT','CWW_T','OL_T','OL_NT','OM_T','OM_NT','CCC_T'))+
  geom_segment(data = functional_scores, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2), 
                                             arrow = arrow(length = unit(0.25, "cm")),
               color = "grey10", lwd = 0.3)+
  geom_text_repel(data = functional_scores, aes(x = NMDS1, y = NMDS2, label = species), cex = 8, direction = "both",
                  segment.size = 0.25)+
  annotate("label", x = 0, y=.5, label ="Stress: X", size = 6)+
  coord_equal()+
  theme_bw()+
  labs(title = "Microarthropod abundance by treatment")+
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_blank(), # remove x-axis labels
        axis.title.y = element_blank(), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank(),
        plot.title = element_text(size = 22))+
  theme(
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    legend.key.size = unit(1.5, 'cm')
  )

#### NO TILL
nt_hull <- hull.data %>% 
  subset(trt %in% c('CWW - NT','CCC - NT', 'OL - NT', 'OM - NT'))

nmds_nt <- ggplot()+
  geom_polygon(data = nt_hull, (aes(x = NMDS1, y = NMDS2, group = trt, fill = trt)), alpha = 0.5)+
  scale_fill_brewer(palette = 'Dark2')+
  # scale_fill_manual(name = "Treatment", labels = c('CWW_NT','CWW_T','OL_T','OL_NT','OM_T','OM_NT','CCC_T'))+
  geom_segment(data = functional_scores, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2), 
               arrow = arrow(length = unit(0.25, "cm")),
               color = "grey10", lwd = 0.3)+
  geom_text_repel(data = functional_scores, aes(x = NMDS1, y = NMDS2, label = species), cex = 8, direction = "both",
                  segment.size = 0.25)+
  annotate("label", x = 0, y=.5, label ="Stress: X", size = 6)+
  coord_equal()+
  theme_bw()+
  labs(title = "Microarthropod abundance by No-till treatment")+
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_blank(), # remove x-axis labels
        axis.title.y = element_blank(), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank(),
        plot.title = element_text(size = 22))+
  theme(
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    legend.key.size = unit(1.5, 'cm')
)

#### TILL
till_hull <- hull.data %>% 
  subset(trt %in% c('CWW - T','CCC - T','OL - T','OM - T' ))
nmds_t <- ggplot()+
  geom_polygon(data = till_hull, (aes(x = NMDS1, y = NMDS2, group = trt, fill = trt)), alpha = 0.5)+
  scale_fill_brewer(palette = 'Dark2')+
  # scale_fill_manual(name = "Treatment", labels = c('CWW_NT','CWW_T','OL_T','OL_NT','OM_T','OM_NT','CCC_T'))+
  geom_segment(data = functional_scores, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2), 
               arrow = arrow(length = unit(0.25, "cm")),
               color = "grey10", lwd = 0.3)+
  geom_text_repel(data = functional_scores, aes(x = NMDS1, y = NMDS2, label = species), cex = 8, direction = "both",
                  segment.size = 0.25)+
  annotate("label", x = 0, y=.5, label ="Stress: X", size = 6)+
  coord_equal()+
  theme_bw()+
  labs(title = "Microarthropod abundance by Till treatment")+
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_blank(), # remove x-axis labels
        axis.title.y = element_blank(), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank(),
        plot.title = element_text(size = 22))+
  theme(
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    legend.key.size = unit(1.5, 'cm')
  )

#### Organic
organic_hull <- hull.data %>% 
  subset(trt %in% c('OL - T','OM - T','OL - NT', 'OM - NT'))
nmds_organic <- ggplot()+
  geom_polygon(data = organic_hull, (aes(x = NMDS1, y = NMDS2, group = trt, fill = trt)), alpha = 0.5)+
  scale_fill_brewer(palette = 'Dark2')+
  # scale_fill_manual(name = "Treatment", labels = c('CWW_NT','CWW_T','OL_T','OL_NT','OM_T','OM_NT','CCC_T'))+
  geom_segment(data = functional_scores, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2), 
               arrow = arrow(length = unit(0.25, "cm")),
               color = "grey10", lwd = 0.3)+
  geom_text_repel(data = functional_scores, aes(x = NMDS1, y = NMDS2, label = species), cex = 8, direction = "both",
                  segment.size = 0.25)+
  annotate("label", x = 0, y=.5, label ="Stress: X", size = 6)+
  coord_equal()+
  theme_bw()+
  labs(title = "Microarthropod abundance by Organic treatment")+
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_blank(), # remove x-axis labels
        axis.title.y = element_blank(), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank(),
        plot.title = element_text(size = 22))+
  theme(
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    legend.key.size = unit(1.5, 'cm')
  )

#### Conventional 
convetional_hull <- hull.data %>% 
  subset(trt %in% c('CWW - T','CCC - T','CWW - NT','CCC - NT'))
nmds_conv <- ggplot()+
  geom_polygon(data = convetional_hull, (aes(x = NMDS1, y = NMDS2, group = trt, fill = trt)), alpha = 0.5)+
  scale_fill_brewer(palette = 'Dark2')+
  # scale_fill_manual(name = "Treatment", labels = c('CWW_NT','CWW_T','OL_T','OL_NT','OM_T','OM_NT','CCC_T'))+
  geom_segment(data = functional_scores, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2), 
               arrow = arrow(length = unit(0.25, "cm")),
               color = "grey10", lwd = 0.3)+
  geom_text_repel(data = functional_scores, aes(x = NMDS1, y = NMDS2, label = species), cex = 8, direction = "both",
                  segment.size = 0.25)+
  annotate("label", x = 0, y=.5, label ="Stress: X", size = 6)+
  coord_equal()+
  theme_bw()+
  labs(title = "Microarthropod abundance by Conventional treatment")+
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_blank(), # remove x-axis labels
        axis.title.y = element_blank(), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank(),
        plot.title = element_text(size = 22))+
  theme(
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    legend.key.size = unit(1.5, 'cm')
  )

