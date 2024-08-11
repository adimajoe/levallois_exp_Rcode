#this is part of the levallois experimental code
#this code is to look at expert data and compare expert and novice data

setwd("/Users/aditimajoe/Desktop/IISER/levallois_experiment")
getwd()

#load libraries

library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(devtools)
library(chisq.posthoc.test)
library(reporttools)


#import dataset as excel file
novice_raw_data_csv <- read.csv("/Users/aditimajoe/Desktop/IISER/levallois_experiment/levallois_knapping_complete.csv")
expert_raw_data_csv <- read.csv("/Users/aditimajoe/Desktop/IISER/levallois_experiment/levallois_expert_knapper.csv")
raw_core_shape <- read.csv("/Users/aditimajoe/Desktop/IISER/levallois_experiment/levallois_core_shape.csv")

summary(expert_raw_data_csv)

#find and remove extra variables from novice dataset- core_final_faciality_direction,core_size,cumulative_practice_hours, time_cumulative_session_end

setdiff(novice_raw_data_csv, expert_raw_data_csv)

novice_data <- select(novice_raw_data_csv,
                      -c(time_cumulative_session_end, cumulative_practice_hours, 
                         core_size, core_final_faciality_direction, X))

#add a column called 'knapper_level' to both datasets with "novice" or "expert"
  
novice_data$knapper_level <- "novice"
expert_raw_data_csv$knapper_level <- "expert"

#join two datasets rbind

combined_raw_data<-rbind(novice_data, expert_raw_data_csv)


combined_no_flake<-select(combined_raw_data,
                          -c(remarks, secondary_reason_abandonment, other, flakes_preparation,
                             flake_surface_shaping, flakes_bifacial_shaping, flakes_opening_reduction,
                             no_flake, estimate_remarks, flake_number.1, entry_strategy))

expert_no_flake<-select(expert_raw_data_csv,
                        -c(remarks, secondary_reason_abandonment, other, flakes_preparation,
                           flake_surface_shaping, flakes_bifacial_shaping, flakes_opening_reduction,
                           no_flake, estimate_remarks, flake_number.1, entry_strategy))

# make dataset that omits the NAs from simplified dataset and drop levels
combined_predrop_levels<-na.omit(combined_no_flake)
expert_data_core<-na.omit(expert_no_flake)

combined_data<-droplevels(combined_predrop_levels)

novice_data_core<-filter(combined_data, knapper_level=="novice")

# Making a new column for weight reduction

combined_data$weight_removed <- (combined_data$core_weight_initial-combined_data$core_weight_final)/combined_data$core_weight_initial

summary(combined_data$weight_removed)

#putting the different stages and sites in order

combined_data$farthest_stage_reached <- 
  factor(combined_data$farthest_stage_reached, levels=c("none", "reduction", "bifacial", "surface_shaping", "platform_prep",
                                                                "plf_attempted", "plf_detached"))
combined_data$site <- 
  factor(combined_data$site, levels=c("GNBK", "SJPR", "KKR", "SJPR_O"))


#make a new column with difficulty difference
combined_data$difficulty_difference <- combined_data$difficulty_assessment_final - combined_data$difficulty_assessment_initial

str(combined_data$difficulty_difference)

#recoding difficulty as "as predicted" and "not as predicted"

combined_data$difficulty_binary <- ifelse(combined_data$difficulty_difference=="0",
                                                  "as predicted", "not as predicted")

#raw_material_texture~type

table(combined_data$raw_material_texture, combined_data$raw_material_type)
prop.table(table(combined_data$raw_material_texture, combined_data$raw_material_type), margin = 2)

ggplot(combined_data, aes(x=raw_material_type, fill=raw_material_texture)) + 
  geom_bar(aes(y=(..count..))) +
  ggtitle("Raw Material Textures")+xlab("Raw Material") + ylab("Count")+
  scale_fill_manual(values=c('#b2182b','#fee08b','#a6d96a'),
                    name = " ", labels = c("Brecciated", "Coarse",
                                           "Smooth"))+
  stat_count(geom = "text", colour = "black", size = 4.0,
             aes(label = (..count..)),position=position_stack(vjust=0.5)) +
  theme_classic()+
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title=element_text(size=30))

fisher.test(table(combined_data$raw_material_texture, combined_data$raw_material_type))

#is there a significant difference between quartzite and sandstone?

combined_no_crypto<-filter(combined_data, raw_material_type!="cryptocrystalline")
combined_no_crypto$raw_material_type <- factor(combined_no_crypto$raw_material_type)
combined_no_crypto$raw_material_texture <- factor(combined_no_crypto$raw_material_texture)

fisher.test(table(combined_no_crypto$raw_material_texture, combined_no_crypto$raw_material_type))

pairwise.fisher.test(combined_no_crypto$raw_material_type, 
                     combined_no_crypto$raw_material_texture,
                     p.adjust.method = "bonferroni")

#compare crypto and sandstone
combined_no_quartzite<-filter(combined_data, raw_material_type!="quartzite")
combined_no_quartzite$raw_material_type <- factor(combined_no_quartzite$raw_material_type)

fisher.test(table(combined_no_quartzite$raw_material_texture, combined_no_quartzite$raw_material_type))
pairwise.fisher.test(combined_no_quartzite$raw_material_type, 
                     combined_no_quartzite$raw_material_texture,
                     p.adjust.method = "bonferroni")

#compare crypto and quartzite
combined_no_sandstone<-filter(combined_data, raw_material_type!="sandstone")
combined_no_sandstone$raw_material_type <- factor(combined_no_sandstone$raw_material_type)

fisher.test(table(combined_no_sandstone$raw_material_texture, combined_no_sandstone$raw_material_type))

pairwise.fisher.test(combined_no_sandstone$raw_material_type, 
                     combined_no_sandstone$raw_material_texture,
                     p.adjust.method = "bonferroni")

#raw_material_colour~type
table(combined_data$raw_material_colour, combined_data$raw_material_type)
prop.table(table(combined_data$raw_material_colour, combined_data$raw_material_type), margin = 2)

ggplot(combined_data, aes(x=raw_material_type, fill=raw_material_colour)) + 
  geom_bar(aes(y=(..count..))) +
  ggtitle("Raw Material Colours")+xlab("Raw Material") + ylab("Count")+
  scale_fill_manual(values=c('#8c2d04','#fec44f','#fff7bc'),
                    name = " ", labels = c("Dark Brown", "Light Brown",
                                           "Other"))+
  stat_count(geom = "text", colour = "black", size = 4.0,
             aes(label = (..count..)),position=position_stack(vjust=0.5)) +
  theme_classic()+
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title=element_text(size=30))

#significance testing for colour~raw material

fisher.test(table(combined_data$raw_material_colour, combined_data$raw_material_type))


#is there a significant difference between quartzite and sandstone?

combined_no_crypto<-filter(combined_data, raw_material_type!="cryptocrystalline")
combined_no_crypto$raw_material_type <- factor(combined_no_crypto$raw_material_type)
combined_no_crypto$raw_material_colour <- factor(combined_no_crypto$raw_material_colour)

fisher.test(table(combined_no_crypto$raw_material_colour, combined_no_crypto$raw_material_type))

#compare crypto and sandstone
combined_no_quartzite<-filter(combined_data, raw_material_type!="quartzite")
combined_no_quartzite$raw_material_type <- factor(combined_no_quartzite$raw_material_type)

fisher.test(table(combined_no_quartzite$raw_material_colour, combined_no_quartzite$raw_material_type))
pairwise.fisher.test(combined_no_quartzite$raw_material_type, 
                     combined_no_quartzite$raw_material_colour,
                     p.adjust.method = "bonferroni")

#compare crypto and quartzite
combined_no_sandstone<-filter(combined_data, raw_material_type!="sandstone")
combined_no_sandstone$raw_material_type <- factor(combined_no_sandstone$raw_material_type)

fisher.test(table(combined_no_sandstone$raw_material_colour, combined_no_sandstone$raw_material_type))

fisher.multcomp(table(combined_no_sandstone$raw_material_colour, combined_no_sandstone$raw_material_type), method = "bonferroni")

pairwise_fisher_test(table(combined_no_sandstone$raw_material_type, combined_no_sandstone$raw_material_colour))

pairwise.fisher.test(combined_no_sandstone$raw_material_type, 
                     combined_no_sandstone$raw_material_colour,
                     p.adjust.method = "bonferroni")

#weight reduced overall
require(mosaic)
require(tigerstats)

favstats(weight_removed~knapper_level, data=combined_data)

# weight reduced by site

ggplot(combined_data, aes(x=as.factor(site), y=weight_removed, fill = knapper_level)) + 
  geom_boxplot(alpha=0.2) + 
  ggtitle("Distribution of Core Reduction by Site")+
  xlab("Site") + ylab("Relative weight reduced (kg)")+
  theme(legend.text = element_text(size = 14),
    legend.title = element_text(size = 18),
    axis.text = element_text(size = 18),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    plot.title=element_text(size=30))

#weight reduced by size

ggplot(combined_data, aes(x=core_length, y=weight_removed, color=knapper_level, shape = knapper_level)) +
  geom_point(size=6) + ggtitle("Core Size vs Weight Removed")+
  xlab("Core Length (cm)")+ylab("Relative weight removed (kg)")+
  geom_smooth(aes(), fill = "white",
                   method = "lm", formula = y ~ x,
                   size = 3, se=FALSE, fullrange=TRUE) +
  theme_classic()+
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title=element_text(size=30))


# weight reduced by raw material
ggplot(combined_data, aes(x=as.factor(raw_material_type), y=weight_removed, fill = knapper_level)) + 
  geom_boxplot(alpha=0.2) + 
  ggtitle("Boxplot of Core Reduction by Raw Material")+
  xlab("Raw Material") + ylab("Relative weight reduced (kg)")+
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title=element_text(size=30))

table(combined_data$raw_material_type,combined_data$knapper_level)

#favstats for table above
novice_data$weight_removed <- (novice_data$core_weight_initial-novice_data$core_weight_final)/novice_data$core_weight_initial
expert_data_core$weight_removed <- (expert_data_core$core_weight_initial-expert_data_core$core_weight_final)/expert_data_core$core_weight_initial


favstats(weight_removed~raw_material_type, data=expert_data_core)

#PLF removed

table(combined_data$plf_stage_reached,combined_data$knapper_level)
chisq.test(table(combined_data$plf_stage_reached,combined_data$knapper_level))
fisher.test(table(combined_data$plf_stage_reached,combined_data$knapper_level))

#PLF by raw material 

table(novice_data$plf_stage_reached,novice_data$raw_material_type)
fisher.test(table(novice_data$plf_stage_reached,novice_data$raw_material_type))
chisq.posthoc.test(table(novice_data$plf_stage_reached,novice_data$raw_material_type))

#PLF by session length side-by-side boxplot
ggplot(combined_data, aes(x=as.factor(plf_stage_reached), y=session_time_hours, fill = knapper_level)) + 
  geom_boxplot(alpha=0.2) + 
  ggtitle("PLF Stage Reached by Session Length")+
  xlab("PLF Stage Reached") + ylab("Session Length (hrs)")+
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title=element_text(size=30))

#farthest stage reached by site

expert_data_core$farthest_stage_reached <- 
  factor(expert_data_core$farthest_stage_reached, levels=c("none", "reduction", "bifacial", "surface_shaping", "platform_prep",
                                                        "plf_attempted", "plf_detached"))

ggplot(expert_data_core, aes (x=site, fill = farthest_stage_reached))+
  geom_bar(aes(y = (..count..)))+ 
  scale_fill_manual(name = "Farthest Stage Reached",
                    labels = c("Reduction","Bifacial", "PLF Attempted", "PLF Detached"),
                    values= c("#f1eef6","#d0d1e6", "#a6bddb", "#74a9cf", "#3690c0","#0570b0","#034e7b"))+
  ggtitle("Farthest Stage Reached by Site")+xlab("Site") + ylab("Count")+
  stat_count(geom = "text", colour = "black", size = 3.5,
             aes(label = ..count..),position=position_stack(vjust=0.5)) +
  theme_classic()+
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title=element_text(size=30))

#farthest stage reached by size

ggplot(combined_data, aes(x=as.factor(farthest_stage_reached), y=core_length, fill = knapper_level)) + 
  geom_boxplot(alpha=0.2) + 
  ggtitle("Farthest Stage Reached by Core Size")+
  xlab("Farthest Stage") + ylab("Core Length (cm)")+
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 18, angle = 25),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title=element_text(size=30))

#farthest stage reached by raw material

ggplot(expert_data_core, aes (x=raw_material_type, fill = farthest_stage_reached))+
  geom_bar(aes(y = (..count..)))+ 
  scale_fill_manual(name = "Farthest Stage Reached",
                    labels = c("Reduction","Bifacial", "PLF Attempted", "PLF Detached"),
                    values= c("#f1eef6","#d0d1e6", "#a6bddb", "#74a9cf", "#3690c0","#0570b0","#034e7b"))+
  ggtitle("Farthest Stage Reached by Raw Material")+xlab("Raw Material Type") + ylab("Count")+
  stat_count(geom = "text", colour = "black", size = 3.5,
             aes(label = ..count..),position=position_stack(vjust=0.5)) +
  theme_classic()+
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title=element_text(size=30))

#farthest stage reached by session length

ggplot(combined_data, aes(x=as.factor(farthest_stage_reached), y=session_time_hours, fill = knapper_level)) + 
  geom_boxplot(alpha=0.2) + 
  ggtitle("Farthest Stage Reached by Session Length")+
  xlab("Farthest Stage") + ylab("Session Length (hrs)")+
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 18, angle = 25),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title=element_text(size=30))

#reason for abandonment - table?

table(combined_data$primary_reason_abandonment, combined_data$knapper_level)
fisher.test(table(combined_data$primary_reason_abandonment, combined_data$knapper_level))
#session length and reason for abandonment

ggplot(combined_data, aes(x=as.factor(primary_reason_abandonment), y=session_time_hours, fill = knapper_level)) + 
  geom_boxplot(alpha=0.2) + 
  ggtitle("Reason for Abandonment by Session Length")+
  xlab("Reason for Abandonment") + ylab("Session Length (hrs)")+
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 18, angle = 25),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title=element_text(size=30))

#Difficulty Estimate
table(combined_data$difficulty_binary, combined_data$knapper_level)

##-------------FLAKE DATA------------------------------------------------------------------

# Flake estimates

#Flake estimates by stage


#making a new variable for number of strikes per flake

combined_data$strike_per_flake <- combined_data$strikes_total/combined_data$flakes_total
novice_data$strike_per_flake<- novice_data$strikes_total/novice_data$flakes_total
expert_data_core$strike_per_flake<-expert_data_core$strikes_total/expert_data_core$flakes_total

#make a new dataset with just the strikes per flake and take out the two with no flakes

combined_strike_per_flake <- subset(combined_data, 
                          select = c(site, core_weight_initial,core_length, core_width, core_thickness,
                                     core_number,strikes_total,flakes_total,
                                     raw_material_type,session_time_hours,
                                     strike_per_flake, knapper_level))%>% 
  filter_all(all_vars(!is.infinite(.)))

novice_strike_per_flake <- subset(novice_data, 
                                    select = c(site, core_weight_initial,core_length, core_width, core_thickness,
                                               core_number,strikes_total,flakes_total,
                                               raw_material_type,session_time_hours,
                                               strike_per_flake, knapper_level))%>% 
  filter_all(all_vars(!is.infinite(.)))

#strikes per flake~site
favstats(expert_data_core$strike_per_flake)
favstats(strike_per_flake~site, data=novice_strike_per_flake)
favstats(strike_per_flake~site, data=expert_data_core)

ggplot(combined_strike_per_flake, aes(x=as.factor(site), y=strike_per_flake, fill = knapper_level)) + 
  geom_boxplot(alpha=0.2) + 
  ggtitle("Strikes per Flake by Site")+
  xlab("Site") + ylab("Number of Strikes per flake")+
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 18, angle = 25),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title=element_text(size=30))

#strikes per flake~raw material
favstats(strike_per_flake~raw_material_type, data=novice_strike_per_flake)
favstats(strike_per_flake~raw_material_type, data=expert_data_core)

ggplot(combined_strike_per_flake, aes(x=as.factor(raw_material_type), y=strike_per_flake, fill = knapper_level)) + 
  geom_boxplot(alpha=0.2) + 
  ggtitle("Strikes per Flake by Raw Material")+
  xlab("Raw Material Type") + ylab("Number of Strikes per flake")+
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 18, angle = 25),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title=element_text(size=30))

#strikes per flake~max length

ggplot(combined_strike_per_flake, aes(x=core_length, y=strike_per_flake, color=knapper_level, shape = knapper_level)) +
  geom_point(size=6) + ggtitle("Core Size vs Strikes per Flake")+
  xlab("Core Length (cm)")+ylab("Number of Strikes per flake")+
  geom_smooth(aes(), fill = "white",
              method = "lm", formula = y ~ x,
              size = 3, se=FALSE, fullrange=TRUE) +
  theme_classic()+
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title=element_text(size=30))
#-------------------------------------------------------------------------------

#making a new dataset of just individual flake counts using subset function

combined_flake_count_data <- subset(combined_raw_data, 
                                     select = c(serial_number, core_number, flake_serial_number,
                                                core_length, raw_material_type,
                                                site,flake_number,number_strikes,estimate,estimate_remarks,
                                                flakes_opening_reduction, flakes_bifacial_shaping,
                                                flake_surface_shaping, 
                                                flakes_preparation, other,no_flake, knapper_level))

# Using dplyr package to change tally marks to flake type

df <- combined_flake_count_data %>% 
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

flake_est_type$flake_type = factor(flake_est_type$flake_type, levels = 
                                     c('     no_flake','reduction     ', ' bifacial_shaping    ',
                                       '  surface_shaping   ','   platform_preparation  ','    other '))
#Estimate order
flake_est_type$estimate <- 
  factor(flake_est_type$estimate, levels=c("miss", "u", "p", "ol", "om"))

#table of estimate by knapper

table(flake_est_type$knapper_level, flake_est_type$estimate)
table(flake_est_type$knapper_level, flake_est_type$flake_type)

#Chi square for prediction

estimate_knapper_test <- chisq.test(table(flake_est_type$estimate, flake_est_type$knapper_level))

type_knapper_test<-chisq.test(table(flake_est_type$flake_type, flake_est_type$knapper_level))

#posthoc test

chisq.posthoc.test(table(flake_est_type$knapper_level, flake_est_type$flake_type), method = "bonferroni")

chisq.posthoc.test(table(flake_est_type$knapper_level, flake_est_type$estimate), method = "bonferroni")

#bar graph of estimates by knapper

ggplot(flake_est_type, aes(x=knapper_level, fill=estimate)) + 
  geom_bar(aes(y=(..count..))) +
  ggtitle("Flake Estimates by Knapper")+xlab("Knapper") + ylab("Count")+
  scale_fill_manual(values=c("#f46d43","#fdae61","#fee08b","#66bd63","darkgreen"),
                    labels= c("Missestimate","Underestimate", "As Predicted",
                              "Overestimate < 50%", "Overestimate > 50%"),
                    name="Flake Outcome")+
  stat_count(geom = "text", colour = "black", size = 3.5,
             aes(label = (..count..)),position=position_stack(vjust=0.5)) +
  theme_classic()+
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title=element_text(size=30))

#bar graph of flake types by knapper

ggplot(flake_est_type, aes(x=knapper_level, fill=flake_type)) + 
  geom_bar(aes(y=(..count..))) +
  ggtitle("Frequency of Flake Types")+xlab("Knapper") + ylab("Count")+
  scale_fill_manual(values=c('#ffffcc','#c7e9b4','#7fcdbb','#41b6c4','#2c7fb8','#253494'),
                    name = "Flake Type", labels = c("No Flake", "Reduction", "Bifacial Shaping",
                                                    "Surface Shaping", "Platform Preperation",
                                                    "Other"))+
  stat_count(geom = "text", colour = "black", size = 4.0,
             aes(label = (..count..)),position=position_stack(vjust=0.5)) +
  theme_classic()+
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title=element_text(size=30))

#separate datasets for novice and expert

novice_flake_est<-subset(flake_est_type, knapper_level=="novice")
expert_flake_est<-subset(flake_est_type, knapper_level=="expert")

#flake type~estimate

table(expert_flake_est$flake_type, expert_flake_est$estimate)
table(novice_flake_est$flake_type, novice_flake_est$estimate)

library(rstatix)

v<-fisher.test(table(novice_flake_est$flake_type, novice_flake_est$estimate),simulate.p.value=TRUE)

chisq.posthoc.test(table(novice_flake_est$flake_type, novice_flake_est$estimate), method = "bonferroni")

fisher.multcomp(table(novice_flake_est$flake_type, novice_flake_est$estimate), method = "bonferroni")

w<-fisher.test(table(expert_flake_est$flake_type, expert_flake_est$estimate),simulate.p.value=TRUE)

chisq.posthoc.test(table(expert_flake_est$flake_type, expert_flake_est$estimate), method = "bonferroni")

#-----------looking at shape data------------------------------

str(raw_core_shape)

raw_core_shape$serial_number<-as.factor(raw_core_shape$serial_number)
combined_data$serial_number<-as.factor(combined_data$serial_number)

#select only the variables from both datasets that are required for analysis

combined_data_selected <- select(combined_data, c(serial_number, site, core_number, knapper_level,
                                                        raw_material_type, core_length, core_weight_initial, 
                                                  core_weight_final, strikes_total, flakes_total, plf_stage_reached,
                                                  farthest_stage_reached,session_time_hours, plf_detached,
                                                        weight_removed, primary_reason_abandonment))

shape_data_selected <- select(raw_core_shape, c(core_number,
                                                elongation_ratio, platyness_ratio,
                                                cobble_shape))

#merge the raw data csv with core shape dataset

combined_shape_data <- inner_join(combined_data_selected, shape_data_selected, by = c("core_number"))

#basic descriptive shape data

table<-table(combined_shape_data$knapper_level,combined_shape_data$cobble_shape)
prop.table(table, margin=1)

fisher.test(table(combined_shape_data$knapper_level,combined_shape_data$cobble_shape))

#Different datasets for novice and expert

novice_shape_data<-filter(combined_shape_data, knapper_level=="novice")
expert_shape_data<-filter(combined_shape_data, knapper_level=="expert")

#shape~reason for abandonment

table<-table(expert_shape_data$primary_reason_abandonment, expert_shape_data$cobble_shape)
prop.table(table, margin = 1)

table<-table(novice_shape_data$primary_reason_abandonment, novice_shape_data$cobble_shape)
prop.table(table, margin = 2)

fisher.test(table(novice_shape_data$primary_reason_abandonment, novice_shape_data$cobble_shape))

#what if we look at only discs and spheres?

shape_data_disc_sphere <-filter(novice_shape_data, cobble_shape=="D"| cobble_shape=="S")

shape_data_disc_sphere$cobble_shape<-factor(shape_data_disc_sphere$cobble_shape)

table(shape_data_disc_sphere$cobble_shape, shape_data_disc_sphere$primary_reason_abandonment)

fisher.test(table(shape_data_disc_sphere$cobble_shape, shape_data_disc_sphere$primary_reason_abandonment))

#shape~plf stage reached
table(novice_shape_data$cobble_shape, novice_shape_data$plf_stage_reached)

fisher.test(table(novice_shape_data$cobble_shape, novice_shape_data$plf_stage_reached))

ggplot(novice_shape_data, aes (x=cobble_shape, fill = plf_stage_reached))+
  geom_bar(aes(y = (..count..)))+
  scale_fill_manual(name = "PLF Stage Reached",
                    labels = c("No","Yes"),
                    values= c("#ffeda0","#c7e9c0"))+
  ggtitle("PLF Stage Reached by Cobble Shape")+xlab("Cobble Shape") + ylab("Count")+
  stat_count(geom = "text", colour = "black", size = 3.5,
             aes(label = ..count..),position=position_stack(vjust=0.5)) +
  theme_classic()+
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title=element_text(size=30))

#site~plf stage reached
table(novice_shape_data$site, novice_shape_data$plf_stage_reached)

fisher.test(table(novice_shape_data$site, novice_shape_data$plf_stage_reached))

ggplot(novice_shape_data, aes (x=site, fill = plf_stage_reached))+
  geom_bar(aes(y = (..count..)))+
  scale_fill_manual(name = "PLF Stage Reached",
                    labels = c("No","Yes"),
                    values= c("#ffeda0","#c7e9c0"))+
  ggtitle("PLF Stage Reached by Site")+xlab("Site") + ylab("Count")+
  stat_count(geom = "text", colour = "black", size = 3.5,
             aes(label = ..count..),position=position_stack(vjust=0.5)) +
  theme_classic()+
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title=element_text(size=30))

#shape~weight_removed

favstats(weight_removed~cobble_shape, data=novice_shape_data)

shape_weight<-aov(novice_shape_data$core_length ~ novice_shape_data$cobble_shape)
summary(shape_weight)

ggplot(novice_shape_data, aes(x=as.factor(cobble_shape), y=weight_removed)) + 
  geom_boxplot(alpha=0.2, fill= "yellow") + 
  ggtitle("Amount of Core Reduction by Cobble Shape")+
  xlab("Cobble Shape") + ylab("Relative weight reduced (kg)")+
  theme_classic()+
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title=element_text(size=30))

#shape~farthest stage reached
table(novice_shape_data$cobble_shape, novice_shape_data$farthest_stage_reached)

fisher.test(table(novice_shape_data$cobble_shape, novice_shape_data$farthest_stage_reached),simulate.p.value=TRUE)

#size~plf stage reached
favstats(core_length~plf_stage_reached, data=novice_shape_data)

size_plfreached<-aov(novice_shape_data$core_length ~ novice_shape_data$plf_stage_reached)
summary(size_plfreached)

ggplot(novice_shape_data, aes(x=as.factor(plf_stage_reached), y=core_length, fill = plf_stage_reached)) + 
  geom_boxplot(alpha=0.2) + 
  ggtitle("PLF Stage Reached by Size")+
  xlab("PLF Stage Reached") + ylab("Core Length (cm)")+
  theme_classic()+
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title=element_text(size=30))

#graphs

# Proportions bar charts
ggplot(novice_shape_data, aes(x = cobble_shape)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill= "darkgreen") +
  ggtitle("Cobble Shapes for Novice Knapper")+
  xlab("Cobble Shape") +
  scale_y_continuous(labels = scales::percent, name = "Percentage of Clasts") +
  scale_x_discrete(labels = c("Blade", "Disc", "Rod", "Sphere"))+
  theme_light()+
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title=element_text(size=30))

ggplot(expert_shape_data, aes(x = cobble_shape)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill= "lightblue") +
  ggtitle("Cobble Shapes for Expert Knapper")+
  xlab("Cobble Shape") +
  scale_y_continuous(labels = scales::percent, name = "Percentage of Clasts") +
  scale_x_discrete(labels = c("Blade", "Disc", "Rod", "Sphere"))+
  theme_light()+
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title=element_text(size=30))

combined_type_shape_data <- inner_join(flake_est_type, shape_data_selected, by = c("core_number"))

combined_type_shape_noflake<-filter(combined_type_shape_data, flake_type!="     no_flake")

combined_type_shape_noflake$flake_type

combined_type_shape_noflake$flake_type <- factor(combined_type_shape_noflake$flake_type)

table(combined_type_shape_noflake$flake_type, combined_type_shape_noflake$cobble_shape)

chisq.test(table(combined_type_shape_noflake$flake_type, combined_type_shape_noflake$cobble_shape), simulate.p.value=TRUE)
chisq.posthoc.test(table(combined_type_shape_noflake$flake_type, combined_type_shape_noflake$cobble_shape), simulate.p.value=TRUE)

ggplot(combined_type_shape_noflake, aes(x=cobble_shape, fill=flake_type)) + 
  geom_bar(aes(y=(..count..))) +
  ggtitle("Frequency of Flake Types")+xlab("Cobble Shape") + ylab("Count")+
  scale_fill_manual(values=c('#c7e9b4','#7fcdbb','#41b6c4','#2c7fb8','#253494'),
                    name = "Flake Type", labels = c("Reduction", "Bifacial Shaping",
                                                    "Surface Shaping", "Platform Preperation",
                                                    "Other"))+
  stat_count(geom = "text", colour = "black", size = 4.0,
             aes(label = (..count..)),position=position_stack(vjust=0.5)) +
  theme_classic()+
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title=element_text(size=30))

#shape~number of flakes at different stages

novice_shape_flake <- read.csv("/Users/aditimajoe/Desktop/IISER/levallois_experiment/novice_shape_flakes.csv")
mat<-as.matrix(novice_shape_flake)
tab<-as.table(mat)

novice_shape_flake_table <- as.table(rbind(c(118, 134, 97, 9, 31), c(132,245,76, 18, 16), c(127,128,43, 5, 7), 
                    c(105,123,89, 9, 28)))
dimnames(novice_shape_flake_table) <- list(Shape=c("B","D","R","S"),Flakes=c("reduction_total","bifacial_total","surface_shaping_total",
                                               "platform_prep_total","other_total"))
novice_shape_flake_table

chisq.test(novice_shape_flake_table)
chisq.test(novice_shape_flake_table)$stdres

alpha=0.05

alpha_adj=alpha/(nrow(novice_shape_flake_table)*ncol(novice_shape_flake_table))

qnorm(alpha_adj/2)

chisq.posthoc.test(novice_shape_flake_table)

#expert

expert_shape_flake <- read.csv("/Users/aditimajoe/Desktop/IISER/levallois_experiment/expert_shape_flake.csv")
mat_e<-as.matrix(expert_shape_flake)

expert_shape_flake_table <- as.table(rbind(c(3, 51, 5, 6, 2), c(24,99, 38, 26, 11), c(12,32,16, 16, 8), 
                                           c(14, 0, 0, 0, 0)))
dimnames(expert_shape_flake_table) <- list(Shape=c("B","D","R","S"),Flakes=c("reduction_total","bifacial_total","surface_shaping_total",
                                                                             "platform_prep_total","other_total"))
expert_shape_flake_table

chisq.test(expert_shape_flake_table, simulate.p.value = TRUE)

chisq.test(expert_shape_flake_table, simulate.p.value = TRUE)$stdres

alpha=0.05

alpha_adj=alpha/(nrow(novice_shape_flake_table)*ncol(novice_shape_flake_table))

qnorm(alpha_adj/2)

chisq.posthoc.test(expert_shape_flake_table)

#final core form data

str(combined_data$subsumed_core_final_form)
table(combined_data$knapper_level, combined_data$subsumed_core_final_form)
prop.table(table(combined_data$knapper_level, combined_data$subsumed_core_final_form), margin =1)

fisher.test(table(combined_data$knapper_level, combined_data$subsumed_core_final_form))

#final form~plf stage reached
table(combined_data$plf_stage_reached, combined_data$subsumed_core_final_form)


y=table(novice_data_core$subsumed_core_final_form, novice_data_core$plf_stage_reached)
prop.table(y,margin = 1)
fisher.test(y)
pairwise_fisher_test(y, p.adjust.method = "bonferroni")

chisq.test(y, simulate.p.value = T)

chisq.test(y, simulate.p.value = TRUE)$stdres

alpha=0.05

alpha_adj=alpha/(nrow(y)*ncol(y))

qnorm(alpha_adj/2)

chisq.posthoc.test(y, simulate.p.value = T)

z=table(expert_data_core$subsumed_core_final_form, expert_data_core$plf_stage_reached)
prop.table(z, margin = 1)
expert_data_core$plf_stage_reached<-factor(expert_data_core$plf_stage_reached)
expert_data_core$subsumed_core_final_form<-factor(expert_data_core$subsumed_core_final_form)

chisq.test(z, simulate.p.value = TRUE)$stdres

alpha=0.05

alpha_adj=alpha/(nrow(z)*ncol(z))

qnorm(alpha_adj/2)

fisher.test(table(expert_data_core$subsumed_core_final_form, expert_data_core$plf_stage_reached))
pairwise_fisher_test(z, p.adjust.method = "bonferroni")

