#this is part of the levallois experimental code
#the size of the clasts was previously grouped manually by Y and A
#this code groups them by maximum dimension
#we then see if we observe the same correlations and patterns as we did before in different tests

setwd("/Users/aditimajoe/Desktop/IISER/levallois_experiment")
getwd()
install.packages("chisq.posthoc.test")

#load libraries

library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(devtools)
library(chisq.posthoc.test)

#import dataset as excel file
raw_data_csv <- read.csv("/Users/aditimajoe/Desktop/IISER/levallois_experiment/levallois_knapping_complete.csv")


#making a new dataset of everything except flake counts using select function

levallois_simple_data_1 <- select(raw_data_csv,
                                  -c(flake_number, flake_number.1, number_strikes,estimate,estimate_remarks,no_flake,
                                     flakes_opening_reduction, flake_serial_number, flakes_bifacial_shaping,
                                     flake_surface_shaping,
                                     flakes_preparation, other,
                                     X, remarks))

# make dataset that omits the NAs from simplified dataset and drop levels
levallois_predrop_levels<-na.omit(levallois_simple_data_1)

levallois_simple_data<-droplevels(levallois_predrop_levels)

# Making a new column for weight reduction

levallois_simple_data$weight_removed = (levallois_simple_data$core_weight_initial-levallois_simple_data$core_weight_final)/levallois_simple_data$core_weight_initial

str(levallois_simple_data$weight_removed)

#putting the different stages in order

levallois_simple_data$farthest_stage_reached <- 
  factor(levallois_simple_data$farthest_stage_reached, levels=c("none", "reduction", "bifacial", "surface_shaping", "platform_prep",
                                                                "plf_attempted", "plf_detached"))


#making a new variable for number of strikes per flake

levallois_simple_data$strike_per_flake <- levallois_simple_data$strikes_total/levallois_simple_data$flakes_total

#make a new column with difficulty difference
levallois_simple_data$difficulty_difference <- levallois_simple_data$difficulty_assessment_final - levallois_simple_data$difficulty_assessment_initial

str(levallois_simple_data$difficulty_difference)

#recoding difficulty as "as predicted" and "not as predicted"

levallois_simple_data$difficulty_binary <- ifelse(levallois_simple_data$difficulty_difference=="0",
                                                  "as predicted", "not as predicted")
levallois_simple_data$site <- 
  factor(levallois_simple_data$site, levels=c("GNBK", "SJPR", "KKR", "SJPR_O"))

#relationship between core_length and difficulty difference

ggplot(levallois_simple_data, aes(x=core_length, y=difficulty_difference)) +
  geom_point() + ggtitle("Difficulty Prediction by Core Size")+
  xlab("Core Length")+ylab("Difficulty Prediction")+
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  geom_text(aes(x = max(core_length), y = max(difficulty_difference), label = sprintf("y = %.2f + %.2fx", lm(difficulty_difference ~ core_length)$coefficients[1], lm(difficulty_difference ~ core_length)$coefficients[2])), 
            hjust = 1, vjust = 1, size = 5)+
  theme_classic()

cor(levallois_simple_data$core_length,levallois_simple_data$difficulty_difference)

#relationship between core_length and core_reduction

ggplot(levallois_simple_data, aes(x=core_length, y=weight_removed)) +
  geom_point() + ggtitle("Weight Removed by Core Size")+
  xlab("Core Length (cm)")+ylab("Relative Weight Removed (kg)")+
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  geom_text(aes(x = max(core_length), y = max(weight_removed), label = sprintf("y = %.2f + %.2fx", lm(weight_removed ~ core_length)$coefficients[1], lm(weight_removed ~ core_length)$coefficients[2])), 
            hjust = 1, vjust = 1, size = 5)+
  theme_classic()+
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title=element_text(size=30))

cor(levallois_simple_data$core_length,levallois_simple_data$weight_removed)


#core_length~plf-removed

aov.model <- aov(levallois_simple_data$core_length ~ levallois_simple_data$plf_detached)
summary(aov.model)

#core_length~farthest stage reached

ggplot(levallois_simple_data, aes(x=as.factor(farthest_stage_reached), y=core_length)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  ggtitle("Boxplot of Clast Size and Farthest Stage")+
  xlab("Farthest Stage") + ylab("Core Length (cm)")+
scale_x_discrete(labels=c("None", "Reduction","Bifacial", "Surface Shaping",
                            "Platform Preparation", "PLF Attempted", "PLF Detached"))+
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(angle = 25),
        axis.title.y = element_text(size = 18),
        plot.title=element_text(size=30))


aov.model <- aov(levallois_simple_data$core_length ~ levallois_simple_data$farthest_stage_reached)
summary(aov.model)

pairwise.t.test(levallois_simple_data$core_length, levallois_simple_data$farthest_stage_reached, p.adj='bonferroni')

#core_length~reason for abandonment

ggplot(levallois_simple_data, aes(x=as.factor(primary_reason_abandonment), y=core_length)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  ggtitle("Boxplot of Clast Size and Abandonment")+
  xlab("Reason for Abandonment") + ylab("Core Length (cm)")+
  scale_x_discrete(labels=c("Convexity Exhaustion", "Volume Exhaustion","Knapping Error", "PLF Detached",
                            "Raw Material Quality", "Morphology"))+
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(angle = 25),
        axis.title.y = element_text(size = 18),
        plot.title=element_text(size=30))

aov.model <- aov(levallois_simple_data$core_length ~ levallois_simple_data$primary_reason_abandonment)
summary(aov.model)

pairwise.t.test(levallois_simple_data$core_length, levallois_simple_data$primary_reason_abandonment, p.adj='bonferroni')


#make a new dataset with just the strikes per flake and take out the two with no flakes

simple_no_infinity <- subset(levallois_simple_data, 
                             select = c(site, core_weight_initial,core_length, core_width, core_thickness,
                                        core_number, weight_removed, core_size,strikes_total,flakes_total,
                                        raw_material_type,session_time_hours, cumulative_practice_hours,
                                        strike_per_flake))%>% 
  filter_all(all_vars(!is.infinite(.)))


#core_length~strikes per flake

plot(simple_no_infinity$strike_per_flake~simple_no_infinity$core_length)

# plot a regression line 
abline(lm(strike_per_flake~core_length,data=simple_no_infinity),col='red')

ggplot(simple_no_infinity, aes(x=core_length, y=strike_per_flake)) +
  geom_point() + ggtitle("Weight Removed by Core Size")+
  xlab("Core Length")+ylab("Strikes per Flake")+
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  geom_text(aes(x = max(core_length), y = max(strike_per_flake), label = sprintf("y = %.2f + %.2fx", lm(strike_per_flake ~ core_length)$coefficients[1], lm(strike_per_flake ~ core_length)$coefficients[2])), 
            hjust = 1, vjust = 1, size = 5)+
  theme_classic()

#fiding the correlation coefficients for weight removed and number of strikes
cor(simple_no_infinity$strike_per_flake,simple_no_infinity$core_length)


#-------------------------------------------------------------------------------

#making a new dataset of just individual flake counts using subset function

levallois_flake_count_data <- subset(raw_data_csv, 
                                     select = c(serial_number, flake_serial_number,
                                                core_size, core_size_max_length, core_length, raw_material_type,
                                                site,flake_number,number_strikes,estimate,estimate_remarks,
                                                flakes_opening_reduction, flakes_bifacial_shaping,
                                                flake_surface_shaping, 
                                                flakes_preparation, other,no_flake))

# Using dplyr package to change tally marks to flake type

df <- levallois_flake_count_data %>% 
  mutate(flakes_opening_reduction = ifelse(flakes_opening_reduction == '',
                                           flakes_opening_reduction,'reduction')) %>% 
  mutate(flakes_bifacial_shaping = ifelse(flakes_bifacial_shaping == '',
                                          flakes_bifacial_shaping,'bifacial_shaping')) %>% 
  mutate(flake_surface_shaping = ifelse(flake_surface_shaping == '',
                                        flake_surface_shaping,'surface_shaping')) %>% 
  mutate(flakes_preparation = ifelse(flakes_preparation == '',flakes_preparation,'platform_preparation')) %>% 
  mutate(no_flake = ifelse(no_flake == '',
                           no_flake,'no_flake')) %>% 
  mutate(other = ifelse(other == '',flakes_preparation,'other'))

# Replacing NA with blanks

df <- replace(df, is.na(df), "")

# Creating a new column that combines all the individual columns for type

df$flake_type <- paste(df$flakes_opening_reduction,df$flakes_bifacial_shaping, df$flake_surface_shaping,df$flakes_preparation, df$other, df$no_flake)

#make new dataset with no individual columns for types

flake_est_type <- select(df, -c(flakes_opening_reduction, flakes_bifacial_shaping,
                                flake_surface_shaping, flakes_preparation, no_flake, other))

flake_est_type$flake_type = as.factor(flake_est_type$flake_type)
flake_est_type$core_length = as.numeric(flake_est_type$core_length)

flake_est_type$flake_type = factor(flake_est_type$flake_type, levels = 
                                     c('     no_flake','reduction     ', ' bifacial_shaping    ',
                                       '  surface_shaping   ','   platform_preparation  ','    other '))

summary.data.frame(flake_est_type)
str(flake_est_type$core_length)
#reorder estimates

flake_est_type$estimate <- 
  factor(flake_est_type$estimate, levels=c("miss", "om", "ol", "p", "u"))

#core_length~frequency of flake types

aov.model <- aov(flake_est_type$core_length ~ flake_est_type$flake_type)
summary(aov.model)

#core_length~flake estimates

  aov.model <- aov(flake_est_type$core_length ~ flake_est_type$estimate)
  summary(aov.model)
  
#----------------------------------------------------------------------------------------
  
#checking correlation of variables with size when size is assigned by max length of population

  #basic structure of variables
  
  summary(raw_data_csv$core_length)
  
  str(raw_data_csv$core_length)
  
  table()
  
  #see distribution of max length in chart format
  
  boxplot(raw_data_csv$core_length)
  #this shows the two outliers to be >20cm, thus the range of the data is 5.7 to 19.3cm
  
  (19.3-5.7)/3 = 4.5
  
  #small clasts are < or = 10.2
  #medium clasts are < or = 14.7
  #large clasts are > 14.7
  
  #relationship between core_length and difficulty difference
  
  #correlation between max_length and core_weight_initial
  
  plot(data_size$core_weight_initial~data_size$core_length)
  
  # plot a regression line 
  abline(lm(core_weight_initial~core_length,data=data_size),col='red')
  
  #predictions of flakes by core size
  
  ggplot(flake_est_type, aes(x=core_size, fill=estimate)) + 
    geom_bar(aes(y=(..count..))) +
    ggtitle("Flake Estimates by Core Size")+xlab("Core Size") + ylab("Count")+
    scale_fill_manual(values=c("#f46d43","#fdae61","#fee08b","#d9ef8b","#66bd63"),
                      name="Outcome relative to knapper's estimate",
                      labels=c("Miss", "Overestimate by more than 50%",
                               "Overestimate by less than 50%", "As predicted", "Underestimate"))+
    stat_count(geom = "text", colour = "black", size = 3.5,
               aes(label = (..count..)),position=position_stack(vjust=0.5)) +
    theme_classic()
  
  #Chi square for prediction
  estimate_size_test <- chisq.test(table(flake_est_type$estimate, flake_est_type$core_size))
  
  estimate_site_test <- chisq.test(table(flake_est_type$estimate, flake_est_type$site))
  
  estimate_material_test <- chisq.test(table(flake_est_type$estimate, flake_est_type$raw_material_type))
  fisher.test(table(flake_est_type$estimate, flake_est_type$raw_material_type))
  
  #make a new column with difficulty difference
  levallois_simple_data$difficulty_difference <- levallois_simple_data$difficulty_assessment_final - levallois_simple_data$difficulty_assessment_initial
  
  str(levallois_simple_data$difficulty_difference)
  
  #recoding difficulty as "as predicted" and "not as predicted"
  
  levallois_simple_data$difficulty_binary <- ifelse(levallois_simple_data$difficulty_difference=="0",
                                                    "as predicted", "not as predicted")
  levallois_simple_data$site <- 
    factor(levallois_simple_data$site, levels=c("GNBK", "SJPR", "KKR", "SJPR_O"))
  
  #relationship between core_length and difficulty difference
  
  ggplot(levallois_simple_data, aes (x=difficulty_binary, fill = core_size_max_length))+
    geom_bar(aes(y = (..count..))) + ggtitle("Difference in Knapper's Difficulty Assessment Score before and after knapping")+
    xlab("Difficulty Assessment Scores") + ylab("Counts")+
    scale_fill_manual(values= c("#e7e1ef","#c994c7", "#756bb1"))+
    stat_count(geom = "text", colour = "black", size = 5.5,
               aes(label = ..count..),position=position_stack(vjust=0.5)) +
    theme_light()
  
  #chisquare of difficulty assessment not possible. Fisher's test:
  
  chisq.test(table(levallois_simple_data$difficulty_difference, levallois_simple_data$core_size_max_length))
  
  fisher.test(table(levallois_simple_data$difficulty_difference, levallois_simple_data$core_size_max_length))
  
  #relationship between core_length and core_reduction
  
  ggplot(levallois_simple_data, aes(x=core_length, y=weight_removed)) +
    geom_point() + ggtitle("Weight Removed by Core Size")+
    xlab("Core Length")+ylab("Core weight (kg)")+
    geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
    geom_text(aes(x = max(core_length), y = max(weight_removed), label = sprintf("y = %.2f + %.2fx", lm(weight_removed ~ core_length)$coefficients[1], lm(weight_removed ~ core_length)$coefficients[2])), 
              hjust = 1, vjust = 1, size = 5)+
    theme_classic()
  
  aov.model <- aov(levallois_simple_data$core_size_max_length ~ levallois_simple_data$core_reduction)
  summary(aov.model)
  
  #core_length~plf-removed
  
  chisq.test(table(levallois_simple_data$plf_detached, levallois_simple_data$core_size_max_length))
  
  fisher.test(table(levallois_simple_data$plf_detached, levallois_simple_data$core_size_max_length))
  
  #core_size~farthest stage reached
  

  chisq.test(table(levallois_simple_data$farthest_stage_reached, levallois_simple_data$core_size_max_length))
  
  fisher.test(table(levallois_simple_data$farthest_stage_reached, levallois_simple_data$core_size_max_length))
  
  #pothoc test for farthest stage
  
  chisq.posthoc.test(levallois_simple_data$farthest_stage_reached, levallois_simple_data$core_size_max_length, method = "bonferroni")
  
  chisq.results <- chisq.test(M)

  
  pairwise.chisq.test(plf_detached, raw_material_type, p.adjust.method = "bonferroni")
  
  #core_size~reason for abandonment
  chisq.test(table(levallois_simple_data$primary_reason_abandonment, levallois_simple_data$core_size_max_length))
  
  fisher.test(table(levallois_simple_data$primary_reason_abandonment, levallois_simple_data$core_size_max_length))
  
  #core_size~frequency of flake types
  
  ggplot(flake_est_type, aes(x=core_size_max_length, fill=flake_type)) + 
    geom_bar(aes(y=(..count..))) +
    ggtitle("Frequency of Flake Types")+xlab("Clast Size") + ylab("Count")+
    scale_fill_manual(values=c('#ffffcc','#c7e9b4','#7fcdbb','#41b6c4','#2c7fb8','#253494'),
                      name = "Flake Type", labels = c("No Flake", "Reduction", "Bifacial Shaping",
                                                      "Surface Shaping", "Platform Preperation",
                                                      "Other"))+
    stat_count(geom = "text", colour = "black", size = 4.0,
               aes(label = (..count..)),position=position_stack(vjust=0.5)) +
    theme_classic()
  
  table(flake_est_type$flake_type)
  
  chisq.test(table(flake_est_type$flake_type, flake_est_type$core_size_max_length))
  
  chisq.posthoc.test(flake_est_type$flake_type, flake_est_type$core_size_max_length, method = "bonferroni")
  
  #core_length~flake estimates

  ggplot(flake_est_type, aes(x=core_size_max_length, fill=estimate)) + 
    geom_bar(aes(y=(..count..))) +
    ggtitle("Flake Estimates by Core Size")+xlab("Core Size") + ylab("Count")+
    scale_fill_manual(values=c("#f46d43","#fdae61","#fee08b","#d9ef8b","#66bd63"),
                      name="Outcome relative to knapper's estimate",
                      labels=c("Miss", "Overestimate by more than 50%",
                               "Overestimate by less than 50%", "As predicted", "Underestimate"))+
    stat_count(geom = "text", colour = "black", size = 3.5,
               aes(label = (..count..)),position=position_stack(vjust=0.5)) +
    theme_classic()

    chisq.test(table(flake_est_type$estimate, flake_est_type$core_size_max_length))
  
  fisher.test(table(flake_est_type$estimate, flake_est_type$core_size_max_length))
  
  table(flake_est_type$estimate, flake_est_type$core_size_max_length)
  
