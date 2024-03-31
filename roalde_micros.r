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
library(lmtest)
library(MASS)
library(multcomp)
library(ggrepel)
library(RColorBrewer)
library(plotly)

# Load in data and explore ####
rodale <- as_tibble(Rodale_counts)

# Need to add in the till, no-till, organic, and conventional treatment identifiers
# Till plots: 200, 500, 700, 800
# No-till plots: 100, 300, 400, 600

# This is the route I am going with 
rodale_trt <- rodale %>%
  mutate(tillage = c('NT', 'T')[1+str_detect(plot, "^[1 | 3 | 4 | 6]")])

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
#

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

###
##
#
# mean scores
mean_scores <- rodale_final %>%
  dplyr::select(-tillage, -trt, -plot) %>% 
  arrange(date, treatment)  %>% 
  group_by(treatment, date) %>% 
  summarise(avg = mean(total_score)) %>% #plyr has summarize
  print(n = Inf)

ggplot(mean_scores, aes(x = treatment, y = avg, fill = date))+
  geom_bar(stat = 'identity', position = 'dodge')

# stats #### 

# need to add a blocking factor (added above with rodale final)
rodale_glmer <- rodale_final %>% 
  dplyr::select(date, plot, total_score, treatment, block) %>% 
  mutate(date = as.factor(date), 
         plot = as.factor(plot), 
         block = as.factor(block))

# begin: this is done with mean scores df
# ANOVA 
# date is significant 
m1 <- aov(total_score ~ treatment + date, data = rodale_glmer)
summary(m1)
hist(residuals(m1)) # look good
TukeyHSD(m1)
# $date
# diff      lwr      upr     p adj
# 7/28/2023-10/11/2023 23.3256 11.48962 35.16159 0.0002275

#nothing sig here becuase date is excluded 
m2 <- aov(total_score ~ treatment, data = rodale_glmer)
summary(m2)

# glm to confirm no differences among treatments
m3 <- glm(total_score ~ treatment + date, data = rodale_glmer)
summary(m3)
hist(residuals(m3))
cld(emmeans(m3, ~treatment), Letters = letters)
# treatment emmean   SE df lower.CL upper.CL .group
# OL - NT     44.6 8.28 54     28.0     61.2  a    
# CCC - NT    57.2 8.28 54     40.6     73.9  a    
# CWW - T     63.1 8.28 54     46.5     79.7  a    
# OM - T      63.8 8.86 54     46.0     81.5  a    
# OM - NT     67.0 8.28 54     50.4     83.6  a    
# CWW - NT    68.2 8.28 54     51.6     84.9  a    
# OL - T      71.2 8.28 54     54.6     87.9  a    
# CCC - T     75.0 8.28 54     58.4     91.6  a    

# glmer
# look at overdispersion: variance > mean?
dispersion_stats <- rodale_glmer %>% 
  group_by(treatment) %>%
  summarise(
  mean = mean(total_score),
  variances = var(total_score),
  ratio = variances/mean)


#singular fit with blocking variable
# removed block and switch to fixed effects only 


glm1 <- glm.nb(total_score ~ date,
                         data = rodale_glmer) 
summary(glmer1)
hist(residuals(glm1))

cld(emmeans(glm1, ~date), Letters = letters)
# date       emmean     SE  df asymp.LCL asymp.UCL .group
# 10/11/2023   3.95 0.0799 Inf      3.80      4.11  a    
# 7/28/2023    4.32 0.0775 Inf      4.17      4.48   b   

#
##
###

# separating trt and tillage
rodale_tillage <- rodale_final %>% 
  dplyr::select(-treatment) %>% 
  arrange(date, trt, tillage, block) %>%
  mutate_at(vars(1:4), as.factor) %>% 
  mutate(block = as.factor(block)) %>% 
  print(n = Inf) 

# stats with tillage separated 
tillage_model <- aov(total_score ~ tillage , data = rodale_tillage)
summary(tillage_model)
hist(residuals(tillage_model))
qqnorm(residuals(tillage_model))

glm_tillage <- glm(total_score ~ tillage + date, data = rodale_tillage)
summary(glm_tillage)
hist(residuals(glm_tillage))
qqnorm(residuals(glm_tillage))


tillage_glmer <- glmer.nb(total_score ~ tillage +
                         (1|date), 
                       data = rodale_tillage)

summary(tillage_glmer)
r2_nakagawa(tillage_glmer)
tillage_residuals <- binned_residuals(tillage_glmer)
plot(tillage_residuals)

cld(emmeans(tillage_glmer, ~tillage), Letters = letters)
# tillage emmean    SE  df asymp.LCL asymp.UCL .group
# NT        4.08 0.139 Inf      3.80      4.35  a    
# T         4.20 0.140 Inf      3.93      4.47  a 


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

permanova_all <- adonis2(dist ~ date + trt + tillage, permutations = factorial(10), method = "bray", data = rodale_totals)
permanova_all
# Df SumOfSqs      R2      F   Pr(>F)   
# date      1   0.6909 0.05133 3.3101 0.003356 **
#   trt       3   0.6455 0.04796 1.0308 0.405608   
# tillage   1   0.2257 0.01677 1.0812 0.348599   
# Residual 57  11.8976 0.88394                   
# Total    62  13.4597 1.00000  


# NMDS ####

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

colnames(rodale_aggregate)
rodale_ord_df <- rodale_aggregate %>% 
  dplyr::select(-total_arth, -neuroptera) %>% 
  mutate(collembola = Sym +Ento) %>% 
  dplyr::rename('Eu-edaphic collembola' = Pod,
                'Hemi-edpahic collembola' = collembola,
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
                'Symphyla' = symph_tot
                ) %>% 
  mutate(treatment = paste(trt, '-', tillage),
                 treatment = as.factor(treatment)) %>% 
  dplyr::select(-trt, -tillage, - Sym, -Ento) %>% 
  relocate(date, plot, treatment) %>% 
  print(n = Inf)
colnames(rodale_ord_df)

new_arth_groups <- rodale_ord_df[,4:23]


ord_2 <- metaMDS(new_arth_groups, k = 2)
ord_2$stress # 0.23
stressplot(ord_2)

# going to use this one
ord_3 <- metaMDS(new_arth_groups, k = 3)
ord_3$stress # 0.163
stressplot(ord_3)


# need to get site scores for ordination
# I think I want display  = "sites"
?scores
scrs <- scores(ord_3, display = "sites")
# adding my scores from metaMDS to their associated trts 
scrs <- cbind(as.data.frame(scrs), trt = rodale_ord_df$treatment)
scrs <- cbind(as.data.frame(scrs), date = rodale_ord_df$date)

# i want to add functional group to this df 
# "species" = averaged site scores
# as_tibble here gets rid of the name and replaces the groups with numbers != what I want
functional_scores <- as.data.frame(scores(ord_3, "species"))
functional_scores$species <- rownames(functional_scores)

unique(rodale_ord_df$treatment)
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


display.brewer.all(colorblindFriendly = TRUE)

# by treatment 
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

# by date
# looks bad in 2d, lets make this one 3d 
plot <- plot_ly(hull.data, x = ~NMDS1, y = ~NMDS2, z = ~NMDS3, color = ~date, 
        colors = c("#1B9E77","#E7298A"))
plot <- plot %>% 
  add_markers()
plot

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

