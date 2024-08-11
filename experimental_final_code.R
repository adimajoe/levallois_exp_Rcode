setwd("/Users/aditimajoe/Desktop/IISER/levallois_experiment")
getwd()
#load libraries

library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(devtools)
library(chisq.posthoc.test)


#raw_data<-read_excel("/Users/aditimajoe/Desktop/IISER/levallois_experiment/levallois_knapping_complete.xlsx",sheet=1,col_names= TRUE,col_types=NULL,na="",skip= 0)

raw_data_csv <- read.csv("/Users/aditimajoe/Desktop/IISER/levallois_experiment/levallois_knapping_complete.csv")

raw_core_shape <- read.csv("/Users/aditimajoe/Desktop/IISER/levallois_experiment/levallois_core_shape.csv")

#basic structure of variables

summary.data.frame(raw_data_csv)

str(raw_data_csv$core_weight_final)

summary(raw_data_csv)

table()

#the variables in the excel file are not read correctly. For example all factors are classified as variables.
#It would be very inefficient to do 'as.factor' on them all
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

levallois_simple_data$weight_removed <- (levallois_simple_data$core_weight_initial-levallois_simple_data$core_weight_final)/levallois_simple_data$core_weight_initial

summary(levallois_simple_data$weight_removed)

#putting the different stages in order

levallois_simple_data$farthest_stage_reached <- 
  factor(levallois_simple_data$farthest_stage_reached, levels=c("none", "reduction", "bifacial", "surface_shaping", "platform_prep",
                                                                "plf_attempted", "plf_detached"))


#making a new variable for number of strikes per flake

levallois_simple_data$strike_per_flake <- levallois_simple_data$strikes_total/levallois_simple_data$flakes_total

#make a new dataset with just the strikes per flake and take out the two with no flakes

simple_no_infinity <- subset(levallois_simple_data, 
                             select = c(site, core_weight_initial,core_length, core_width, core_thickness,
                                        core_number, core_size,strikes_total,flakes_total,
                                        raw_material_type,session_time_hours, cumulative_practice_hours,
                                        strike_per_flake))%>% 
  filter_all(all_vars(!is.infinite(.)))

#make a new column with core volume

simple_no_infinity$core_volume <- simple_no_infinity$core_length*simple_no_infinity$core_width*simple_no_infinity$core_thickness

#make a new column with difficulty difference
levallois_simple_data$difficulty_difference <- levallois_simple_data$difficulty_assessment_final - levallois_simple_data$difficulty_assessment_initial

str(levallois_simple_data$difficulty_difference)

#recoding difficulty as "as predicted" and "not as predicted"

levallois_simple_data$difficulty_binary <- ifelse(levallois_simple_data$difficulty_difference=="0",
                                                  "as predicted", "not as predicted")
levallois_simple_data$site <- 
  factor(levallois_simple_data$site, levels=c("GNBK", "SJPR", "KKR", "SJPR_O"))

#chi square tests and post-hoc tests
if(!require('rstatix')) {
  install.packages('rstatix')
  library('rstatix')}

table(levallois_simple_data$primary_reason_abandonment, levallois_simple_data$raw_material_type)
n=table(levallois_simple_data$raw_material_type, levallois_simple_data$plf_detached)

test_rsults = chisq.test(n)
test_rsults = fisher.test(n, detailed = TRUE)

test_rsults$stdres

pairwise_fisher_test(n)
chisq.posthoc.test(n,
                   method = "bonferroni")

# reason for abandonment ~ site

n=table(levallois_simple_data$primary_reason_abandonment, levallois_simple_data$site)

test_rsults = chisq.test(n, simulate.p.value = TRUE)
test_rsults = fisher.test(n, simulate.p.value = TRUE)


test_rsults$stdres

pairwise_fisher_test(n)
chisq.posthoc.test(n,
                   method = "bonferroni")
# Boxplots to show time taken

ggplot(levallois_simple_data, aes(x=as.factor(site), y=session_time_hours)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  ggtitle("Time taken per session by site")+
  xlab("Site") + ylab("Time in hours")

#scatterplot for core weight vs time taken and number of strikes per flake and time taken

ggplot(levallois_simple_data, aes(x=session_time_hours, y=weight_removed)) +
  geom_point() + ggtitle("Time taken vs Weight Removed")+
  xlab("Time in hours")+ylab("Core weight (kg)")+
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  geom_text(aes(x = max(session_time_hours), y = max(weight_removed), label = sprintf("y = %.2f + %.2fx", lm(weight_removed ~ session_time_hours)$coefficients[1], lm(weight_removed ~ session_time_hours)$coefficients[2])), 
            hjust = 1, vjust = 1, size = 5)+
  theme_classic()

ggplot(simple_no_infinity, aes(x=session_time_hours, y=strikes_total)) +
  geom_point() + ggtitle("Time taken vs Total Strikes")+
  xlab("Time in hours")+ylab("Number of strikes")+
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  geom_text(aes(x = max(session_time_hours), y = max(strikes_total), label = sprintf("y = %.2f + %.2fx", lm(strikes_total ~ session_time_hours)$coefficients[1], lm(strikes_total ~ session_time_hours)$coefficients[2])), 
            hjust = 1, vjust = 1, size = 5)+
  theme_classic()

# descriptive statistics for sessionlength by abandonment. substitute required stat into "name="

table(levallois_simple_data$primary_reason_abandonment)

levallois_simple_data %>%
  group_by(primary_reason_abandonment) %>%
  summarise_at(vars(session_time_hours), list(name = sd))

#IQR
o <- levallois_simple_data %>% 
  group_by(primary_reason_abandonment) %>%
  summarise(IQR(session_time_hours))
o <- data.frame(o)

#fiding the correlation coefficients for weight removed and number of strikes
cor(simple_no_infinity$strikes_total,simple_no_infinity$session_time_hours)

#anova for time taken per session

aov.model <- aov(levallois_simple_data$session_time_hours ~ levallois_simple_data$raw_material_type)
summary(aov.model)

pairwise.t.test(levallois_simple_data$session_time_hours, levallois_simple_data$raw_material_type, p.adj='bonferroni')

aov.model <- aov(levallois_simple_data$session_time_hours ~ levallois_simple_data$site)
summary(aov.model)

pairwise.t.test(levallois_simple_data$session_time_hours, levallois_simple_data$site, p.adj='bonferroni')


aov.model <- aov(levallois_simple_data$session_time_hours ~ levallois_simple_data$core_size)
summary(aov.model)

pairwise.t.test(levallois_simple_data$session_time_hours, levallois_simple_data$core_size, p.adj='bonferroni')


aov.model <- aov(levallois_simple_data$session_time_hours ~ levallois_simple_data$plf_detached)
summary(aov.model)

pairwise.t.test(levallois_simple_data$session_time_hours, levallois_simple_data$plf_detached, p.adj='bonferroni')


aov.model <- aov(levallois_simple_data$session_time_hours ~ levallois_simple_data$farthest_stage_reached)
summary(aov.model)

pairwise.t.test(levallois_simple_data$session_time_hours, levallois_simple_data$farthest_stage_reached, p.adj='bonferroni')


aov.model <- aov(levallois_simple_data$session_time_hours ~ levallois_simple_data$primary_reason_abandonment)
summary(aov.model)

pairwise.t.test(levallois_simple_data$session_time_hours, levallois_simple_data$primary_reason_abandonment, p.adj='bonferroni')


aov.model <- aov(levallois_simple_data$session_time_hours ~ levallois_simple_data$difficulty_binary)
summary(aov.model)

# bloxplot for session times by plf removed, farthest stage, and reason for abandonment

ggplot(levallois_simple_data, aes(x=as.factor(plf_stage_reached), y=session_time_hours)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  ggtitle("Boxplot of Session Length vs PLF Stage Reached")+
  xlab("PLF Stage Reached") + ylab("Session length (hrs)")+
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title=element_text(size=30))

aov.model <- aov(levallois_simple_data$session_time_hours ~ levallois_simple_data$plf_stage_reached)
summary(aov.model)

ggplot(levallois_simple_data, aes(x=as.factor(farthest_stage_reached), y=session_time_hours)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  ggtitle("Boxplot of Session Length vs Farthest Stage Reached")+
  xlab("Farthest Stage Reached") + ylab("Session Length (hrs)")+
  scale_x_discrete(labels=c("None", "Reduction","Bifacial", "Surface Shaping",
                            "Platform Preparation", "PLF Attempted", "PLF Detached"))+
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(angle = 25),
        axis.title.y = element_text(size = 18),
        plot.title=element_text(size=30))

ggplot(levallois_simple_data, aes(x=as.factor(primary_reason_abandonment), y=session_time_hours)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  ggtitle("Boxplot of session length")+
  xlab("Reason for Abandonment") + ylab("Session length in hours")

ggplot(levallois_simple_data, aes(x=as.factor(site), y=session_time_hours)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  ggtitle("Boxplot of session length")+
  xlab("Site") + ylab("Session length in hours")

# descriptive statistics for weight_reduced by core_size. substitute required stat into "name="

levallois_simple_data %>%
  group_by(core_size) %>%
  summarise_at(vars(weight_removed), list(name = max))

favstats(weight_removed~raw_material_type, data=levallois_simple_data)

#IQR
a <- levallois_simple_data %>% 
  group_by(raw_material_type) %>%
  summarise(IQR(weight_removed))
a<- data.frame(a)


#anova for cumulative practice hours

aov.model <- aov(levallois_simple_data$cumulative_practice_hours ~ levallois_simple_data$core_size)
summary(aov.model)

aov.model <- aov(levallois_simple_data$cumulative_practice_hours ~ levallois_simple_data$plf_stage_reached)
summary(aov.model)

aov.model <- aov(levallois_simple_data$cumulative_practice_hours ~ levallois_simple_data$farthest_stage_reached)
summary(aov.model)

pairwise.t.test(levallois_simple_data$cumulative_practice_hours, levallois_simple_data$farthest_stage_reached, p.adj='bonferroni')


aov.model <- aov(levallois_simple_data$cumulative_practice_hours ~ levallois_simple_data$primary_reason_abandonment)
summary(aov.model)

aov.model <- aov(levallois_simple_data$cumulative_practice_hours ~ levallois_simple_data$difficulty_binary)
summary(aov.model)

#kruskal wallis test, in case ANOVA is inappropriate

kruskal.test(cumulative_practice_hours ~ farthest_stage_reached, data = levallois_simple_data)

kruskal.test(session_time_hours ~ primary_reason_abandonment, data = levallois_simple_data)


#fiding the correlation coefficients for weight removed and number of strikes

cor(levallois_simple_data$weight_removed,levallois_simple_data$cumulative_practice_hours)

cor(simple_no_infinity$strikes_total,simple_no_infinity$cumulative_practice_hours)

#fiding the correlation coefficient for practice time and time per session

cor(levallois_simple_data$session_time_hours,levallois_simple_data$cumulative_practice_hours)

# bloxplot for practice hours by farthest stage reached

aov.model <- aov(levallois_simple_data$cumulative_practice_hours ~ levallois_simple_data$farthest_stage_reached)
summary(aov.model)

ggplot(levallois_simple_data, aes(x=as.factor(farthest_stage_reached), y=cumulative_practice_hours)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  ggtitle("Boxplot of Practice Hours vs Farthest Stage Reached")+
  xlab("Farthest Stage Reached") + ylab("Practice Hours (hrs)")+
  scale_x_discrete(labels=c("None", "Reduction","Bifacial", "Surface Shaping",
                            "Platform Preparation", "PLF Attempted", "PLF Detached"))+
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(angle = 25),
        axis.title.y = element_text(size = 18),
        plot.title=element_text(size=30))

#summary stats

levallois_simple_data %>%
  group_by(farthest_stage_reached) %>%
  summarise_at(vars(cumulative_practice_hours), list(name = sd))


# descriptive statistics for weight_reduced by site. substitute required stat into "name="

levallois_simple_data %>%
  group_by(site) %>%
  summarise_at(vars(weight_removed), list(name = sd))
?summarise_at
#IQR
a <- levallois_simple_data %>% 
  group_by(site) %>%
  summarise(IQR(weight_removed))
a <- data.frame(a)

# Boxplots to show weight reduction

ggplot(levallois_simple_data, aes(x=as.factor(site), y=weight_removed)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  ggtitle("Distribution of Core Reduction by Site")+
  xlab("Site") + ylab("Relative weight reduced (kg)")+
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title=element_text(size=30))

# descriptive statistics for weight_reduced by site. substitute required stat into "name="

levallois_simple_data %>%
  group_by(site) %>%
  summarise_at(vars(weight_removed), list(name = sd))
?summarise_at
#IQR
a <- levallois_simple_data %>% 
  group_by(site) %>%
  summarise(IQR(weight_removed))
a <- data.frame(a)

# Boxplots to show weight reduction by size

ggplot(levallois_simple_data, aes(x=as.factor(core_size), y=weight_removed)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  ggtitle("Distribution of Core Reduction by Core Size")+
  xlab("Size") + ylab("Total weight reduced (kg)")

# descriptive statistics for weight_reduced by core_size. substitute required stat into "name="

levallois_simple_data %>%
  group_by(core_size) %>%
  summarise_at(vars(weight_removed), list(name = max))

#IQR
b <- levallois_simple_data %>% 
  group_by(core_size) %>%
  summarise(IQR(weight_removed))
b <- data.frame(b)

#weight reduction by raw material type

ggplot(levallois_simple_data, aes(x=as.factor(raw_material_type), y=weight_removed)) + 
  geom_boxplot(fill="yellow", alpha=0.2) + 
  ggtitle("Core Reduction by Raw Material Type")+
  xlab("Raw Material") + ylab("Relative Weight Removed (kg)")+
  theme_classic()+
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title=element_text(size=30))

# descriptive statistics for weight_reduced by core_size. substitute required stat into "name="

levallois_simple_data %>%
  group_by(raw_material_type) %>%
  summarise_at(vars(weight_removed), list(name = median))

#IQR
q <- levallois_simple_data %>% 
  group_by(raw_material_type) %>%
  summarise(IQR(weight_removed))
q <- data.frame(q)

#ANOVA for weight difference by site and by core size and by raw material

aov.model <- aov(levallois_simple_data$weight_removed ~ levallois_simple_data$core_size)
summary(aov.model)

aov.model <- aov(levallois_simple_data$weight_removed ~ levallois_simple_data$site)
summary(aov.model)

aov.model <- aov(levallois_simple_data$weight_removed ~ levallois_simple_data$raw_material_type)
summary(aov.model)

pairwise.t.test(levallois_simple_data$weight_removed, levallois_simple_data$raw_material_type, p.adj='bonferroni')

#plf detchment data

table(levallois_simple_data$plf_stage_reached)

prop.table(table(levallois_simple_data$plf_stage_reached))

aov.model <- aov(levallois_simple_data$core_length ~ levallois_simple_data$plf_stage_reached)
summary(aov.model)

#bar graph for plf detached

ggplot(levallois_simple_data, aes (x=plf_detached))+
  geom_bar(aes(y = (..count..)), fill="#BFE12E")+
  ggtitle("Levallois Flakes Detached")+xlab("PLF Detached") + ylab("Count")+
  stat_count(geom = "text", colour = "black", size = 3.5,
             aes(label = ..count..),position=position_stack(vjust=0.5)) +
  theme_classic()

#bar graph for plf detached by site

table(levallois_simple_data$plf_stage_reached, levallois_simple_data$site)
fisher.test(table(levallois_simple_data$plf_stage_reached, levallois_simple_data$site))

ggplot(levallois_simple_data, aes (x=plf_detached, fill = site))+
  geom_bar(aes(y = (..count..)))+ 
  scale_fill_manual(values= c("#fbb4ae","#b3cde3", "#ccebc5", "#decbe4"))+
  ggtitle("Levallois Flakes Detached by Site")+xlab("PLF Detached") + ylab("Count")+
  stat_count(geom = "text", colour = "black", size = 3.5,
             aes(label = ..count..),position=position_stack(vjust=0.5)) +
  theme_classic()

ggplot(levallois_simple_data, aes (x=site, fill = plf_detached))+
  geom_bar(aes(y = (..count..)))+ 
  scale_fill_manual(values= c("#8dd3c7","#ffffb3", "#fc8d62"))+
  ggtitle("Levallois Flakes Detached by Site")+xlab("Site") + ylab("Count")+
  stat_count(geom = "text", colour = "black", size = 3.5,
             aes(label = ..count..),position=position_stack(vjust=0.5)) +
  theme_classic()

#bar graph for plf detached by size

ggplot(levallois_simple_data, aes (x=core_size, fill = plf_detached))+
  geom_bar(aes(y = (..count..)))+ 
  scale_fill_manual(values= c("#8dd3c7","#ffffb3", "#fc8d62"))+
  ggtitle("Levallois Flakes Detached by Size")+xlab("Core Size") + ylab("Count")+
  stat_count(geom = "text", colour = "black", size = 3.5,
             aes(label = ..count..),position=position_stack(vjust=0.5)) +
  theme_classic()

ggplot(levallois_simple_data, aes (x=plf_detached, fill = core_size))+
  geom_bar(aes(y = (..count..)))+ 
  scale_fill_manual(values= c("#e7e1ef","#c994c7", "#756bb1"))+
  ggtitle("Levallois Flakes Detached by Size")+xlab("PLF Detached") + ylab("Count")+
  stat_count(geom = "text", colour = "black", size = 3.5,
             aes(label = ..count..),position=position_stack(vjust=0.5)) +
  theme_classic()

#bar graph for plf detached by raw material

fisher.test(table(levallois_simple_data$raw_material_type, levallois_simple_data$plf_stage_reached))

x=table(levallois_simple_data$raw_material_type, levallois_simple_data$plf_stage_reached)
pairwise_fisher_test(x, p.adjust.method="bonferroni", detailed = TRUE)


ggplot(levallois_simple_data, aes (x=raw_material_type, fill = plf_stage_reached))+
  geom_bar(aes(y = (..count..)))+ 
  scale_fill_manual(values= c("#fcbba1", "#c7e9c0"),
                    labels=c("No", "Yes"),
                    name="PLF Stage Reached")+
  ggtitle("PLF Stage Reached by Raw Material Type")+xlab("Raw Material") + ylab("Count")+
  stat_count(geom = "text", colour = "black", size = 3.5,
             aes(label = ..count..),position=position_stack(vjust=0.5)) +
  theme_classic()+
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title=element_text(size=30))

#Chi squared for PLF detached not appropriate due to cell count < 5

chisq.test(table(levallois_simple_data$plf_detached, levallois_simple_data$core_size))

fisher.test(table(levallois_simple_data$plf_detached, levallois_simple_data$core_size))
fisher.test(table(levallois_simple_data$plf_detached, levallois_simple_data$site))
fisher.test(table(levallois_simple_data$plf_detached, levallois_simple_data$raw_material_type))

table(levallois_simple_data$raw_material_type, levallois_simple_data$plf_detached)

#pothoc test for raw maerial

chisq.posthoc.test(levallois_simple_data$raw_material_type, levallois_simple_data$plf_detached, method = "bonferroni")

chisq.results <- chisq.test(M)

install.packages("chisq.posthoc.test")
library(chisq.posthoc.test)

pairwise.chisq.test(plf_detached, raw_material_type, p.adjust.method = "bonferroni")

#farthest stage reached data

table(levallois_simple_data$site,levallois_simple_data$farthest_stage_reached)

#Stacked bar graph with site as x axis and stage as fill

fisher.test(table(levallois_simple_data$site,levallois_simple_data$farthest_stage_reached))
aov.model <- aov(levallois_simple_data$core_length ~ levallois_simple_data$farthest_stage_reached)
summary(aov.model)

ggplot(levallois_simple_data, aes (x=site, fill = farthest_stage_reached))+
  geom_bar(aes(y = (..count..)))+ 
  scale_fill_manual(values= c("#f1eef6","#d0d1e6", "#a6bddb", "#74a9cf", "#3690c0","#0570b0","#034e7b"),
                    labels=c("None", "Reduction","Bifacial", "Surface Shaping",
                             "Platform Preparation", "PLF Attempted", "PLF Detached"),
                    name="Farthest Stage Reached")+
  ggtitle("Farthest Stage Reached by Site")+xlab("Site") + ylab("Count")+
  stat_count(geom = "text", colour = "black", size = 3.5,
             aes(label = ..count..),position=position_stack(vjust=0.5)) +
  theme_classic()+
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title=element_text(size=30))

#Stacked bar graph with stage as x axis and site as fill

ggplot(levallois_simple_data, aes (x=farthest_stage_reached, fill = site))+
  geom_bar(aes(y = (..count..)))+ 
  scale_fill_manual(values= c("#fbb4ae","#b3cde3", "#ccebc5", "#decbe4"))+
  ggtitle("Farthest Stage Reached by Site")+xlab("Farthest Stage Reached") + ylab("Count")+
  stat_count(geom = "text", colour = "black", size = 3.5,
             aes(label = ..count..),position=position_stack(vjust=0.5)) +
  theme_classic()

#Stacked bar graph with size as x axis and stage as fill

ggplot(levallois_simple_data, aes (x=core_size, fill = farthest_stage_reached))+
  geom_bar(aes(y = (..count..)))+ 
  scale_fill_manual(values= c("#f1eef6","#d0d1e6", "#a6bddb", "#74a9cf", "#3690c0","#0570b0","#034e7b"))+
  ggtitle("Farthest Stage Reached by Core Size")+xlab("Size") + ylab("Count")+
  stat_count(geom = "text", colour = "black", size = 3.5,
             aes(label = ..count..),position=position_stack(vjust=0.5)) +
  theme_classic()

ggplot(levallois_simple_data, aes (x=farthest_stage_reached, fill = core_size))+
  geom_bar(aes(y = (..count..)))+ 
  scale_fill_manual(values= c("#e7e1ef","#c994c7", "#756bb1"))+
  ggtitle("Farthest Stage Reached by Core Size")+xlab("Farthest Stage Reached") + ylab("Count")+
  stat_count(geom = "text", colour = "black", size = 3.5,
             aes(label = ..count..),position=position_stack(vjust=0.5)) +
  theme_classic()

#Stacked bar graph with raw material as x axis and stage as fill

ggplot(levallois_simple_data, aes (x=raw_material_type, fill = farthest_stage_reached))+
  geom_bar(aes(y = (..count..)))+ 
  scale_fill_manual(values= c("#f1eef6","#d0d1e6", "#a6bddb", "#74a9cf", "#3690c0","#0570b0","#034e7b"))+
  ggtitle("Farthest Stage Reached by Raw Material Type")+xlab("Raw Material") + ylab("Count")+
  stat_count(geom = "text", colour = "black", size = 3.5,
             aes(label = ..count..),position=position_stack(vjust=0.5)) +
  theme_classic()

#Stacked bar graph with raw material as x axis and stage as fill

table(levallois_simple_data$raw_material_type, levallois_simple_data$farthest_stage_reached)
fisher.test(table(levallois_simple_data$farthest_stage_reached, levallois_simple_data$raw_material_type))

ggplot(levallois_simple_data, aes (x=raw_material_type, fill = farthest_stage_reached))+
  geom_bar(aes(y = (..count..)))+ 
  scale_fill_manual(values= c("#f1eef6","#d0d1e6", "#a6bddb", "#74a9cf", "#3690c0","#0570b0","#034e7b"),
                    labels=c("None", "Reduction","Bifacial", "Surface Shaping",
                             "Platform Preparation", "PLF Attempted", "PLF Detached"),
                             name="Farthest Stage Reached")+
  ggtitle("Farthest Stage Reached by Raw Material Type")+xlab("Raw Material") + ylab("Count")+
  stat_count(geom = "text", colour = "black", size = 3.5,
             aes(label = ..count..),position=position_stack(vjust=0.5)) +
  theme_classic()+
  scale_x_discrete(labels=c("Crptocrystalline", "Quartzite", "Sandstone"))+
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title=element_text(size=30))

#chi-squared for farthest stage reached-- not appripriate

chisq.test(table(levallois_simple_data$farthest_stage_reached, levallois_simple_data$raw_material_type))

table_stage_material<-table(levallois_simple_data$raw_material_type, levallois_simple_data$farthest_stage_reached)

length(table_stage_material)

#Fisher's exact test for farthest stage reached

fisher.test(table(levallois_simple_data$farthest_stage_reached, levallois_simple_data$core_size))

fisher.test(table(levallois_simple_data$farthest_stage_reached, levallois_simple_data$site))

fisher.test(table(levallois_simple_data$farthest_stage_reached, levallois_simple_data$raw_material_type))

#graph of abandonment reasons

ggplot(levallois_simple_data, aes(x=farthest_stage_reached)) + 
  geom_bar(aes(y=(..count..)), fill= "orange") +
  ggtitle("Farthest Stage Reached")+xlab("Stage") + ylab("Count")+
  scale_x_discrete(labels = c("None", "Reduction", "Bifacial",
                              "Surface Shaping", "Platform Preparation","PLF Attempted",
                              "PLF Detached"))+
  stat_count(geom = "text", colour = "black", size = 3.5,
             aes(label = (..count..)),position=position_stack(vjust=0.5)) +
  theme_classic()+
  theme(axis.text.x = element_text(angle=15, hjust = 1))

#stacked bar graph should primary reason for abandonment

table(levallois_simple_data$primary_reason_abandonment)

ggplot(levallois_simple_data, aes(x=site, fill= primary_reason_abandonment)) +
  geom_bar(aes(y=(..count..))) +
  scale_fill_manual(values=c("#b35806","#f1a340","#fee0b6","#d8daeb","#998ec3","#542788"),
                    labels=c("Convexity Exhaustion", "Volume Exhaustion","Knapping Error", "PLF Detached",
                             "Raw Material Quality", "Morphology"),
                    name="Reason for Abandonment") +
  ggtitle("Reasons for Abandonment by Site")+ xlab("Site") + ylab("Count")+
  stat_count(geom = "text", colour = "black", size = 3.5,
             aes(label = ..count..),position=position_stack(vjust=0.5)) +
  theme_classic()+
  scale_x_discrete(labels=c("GNBK", "SJPR","KKR", "SJPR_O"))+
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title=element_text(size=30))


ggplot(levallois_simple_data, aes(x=primary_reason_abandonment, fill= core_size)) +
  geom_bar(aes(y=(..count..))) +
  scale_fill_manual(values=c("#e7e1ef","#c994c7", "#756bb1")) +
  ggtitle("Reasons for Abandonment by Core Size")+ xlab("Reason for Abandonment") + ylab("Count")+
  stat_count(geom = "text", colour = "black", size = 3.5,
             aes(label = ..count..),position=position_stack(vjust=0.5)) +
  theme_classic()

ggplot(levallois_simple_data, aes(x=raw_material_type, fill= primary_reason_abandonment)) +
  geom_bar(aes(y=(..count..))) +
  scale_fill_manual(values=c("#b35806","#f1a340","#fee0b6","#d8daeb","#998ec3","#542788"),
                    labels=c("Convexity Exhaustion", "Volume Exhaustion","Knapping Error", "PLF Detached",
                            "Raw Material Quality", "Morphology"),
                    name="Reason for Abandonment") +
  ggtitle("Reasons for Abandonment by Raw Material Type")+ xlab("Raw Material") + ylab("Count")+
  stat_count(geom = "text", colour = "black", size = 3.5,
             aes(label = ..count..),position=position_stack(vjust=0.5)) +
  theme_classic()+
  scale_x_discrete(labels=c("Cryptocrystalline", "Quartzite","Sandstone"))+
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title=element_text(size=30))

table(levallois_simple_data$raw_material_type,levallois_simple_data$primary_reason_abandonment)
table(levallois_simple_data$raw_material_type)


#graph of abandonment reasons

ggplot(levallois_simple_data, aes(x=primary_reason_abandonment)) + 
  geom_bar(aes(y=(..count..)), fill= "darkgreen") +
  ggtitle("Reason for Abandonment")+xlab("Reasons") + ylab("Count")+
  scale_x_discrete(labels = c("Convexity Exhaustion", "Volume Exhaustion", "Knapper Error",
                              "PLF Detached", "Raw Material Quality",
                              "Shape"))+
  stat_count(geom = "text", colour = "white", size = 3.5,
             aes(label = (..count..)),position=position_stack(vjust=0.5)) +
  theme_classic()+
  theme(axis.text.x = element_text(angle=15, hjust = 1))

# chi-square for abandonment--shows error because of cell counts being less than 5 most likely

abandonment_site_test<-chisq.test(table(levallois_simple_data$primary_reason_abandonment, levallois_simple_data$site))

abandonment_size_test<-chisq.test(table(levallois_simple_data$primary_reason_abandonment, levallois_simple_data$core_size))

fisher.test(table(levallois_simple_data$primary_reason_abandonment, levallois_simple_data$raw_material_type))

#since chi-square is not the appropriate test, using Fisher's exact test

t=table(levallois_simple_data$site,levallois_simple_data$primary_reason_abandonment)
v=table(levallois_simple_data$primary_reason_abandonment,levallois_simple_data$core_size)
w=table(levallois_simple_data$primary_reason_abandonment,levallois_simple_data$raw_material_type)

fisher.test(levallois_simple_data$site,levallois_simple_data$primary_reason_abandonment,  simulate.p.value=TRUE)

pairwise_fisher_test(t)

# Summarise central tendency (size and site)

simple_no_infinity %>%
  group_by(core_size) %>%
  summarise_at(vars(strike_per_flake), list(name = mean))

# to get IQR of strikes per flake by (size or site)

d <- simple_no_infinity %>% 
  group_by(core_size) %>%
  summarise(IQR(strike_per_flake))
d <- data.frame(d)

# Bloxplot graph for strikes per flake by site

ggplot(simple_no_infinity, aes (x=site, y= strike_per_flake))+
  geom_boxplot(fill="slateblue", alpha=0.2) +
  ggtitle("Average attempts per removal")+xlab("Site") + ylab("Strikes per flake")+
  theme_classic()

# Bloxplot graph for strikes per flake by size

ggplot(simple_no_infinity, aes (x=core_size, y= strike_per_flake))+
  geom_boxplot(fill="slateblue", alpha=0.2) +
  ggtitle("Average attempts per removal")+xlab("Core Size") + ylab("Strikes per flake")+
  theme_classic()

summary(simple_no_infinity$strike_per_flake)

#scatterplot for core weight vs strikes per flake

ggplot(simple_no_infinity, aes(x=core_weight_initial, y=strike_per_flake)) +
  geom_point() + ggtitle("Initial Core Weight vs Number of Strikes per Flake")+
  xlab("Core weight (kg)")+ylab("Number of strikes per flake")+
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  geom_text(aes(x = max(core_weight_initial), y = max(strike_per_flake), label = sprintf("y = %.2f + %.2fx", lm(strike_per_flake ~ core_weight_initial)$coefficients[1], lm(strike_per_flake ~ core_weight_initial)$coefficients[2])), 
            hjust = 1, vjust = 1, size = 5)+
  theme_classic()


#If you want to display just the slope, use:
# +annotate("text",x=2,y=20,label=(paste0("slope==",coef(lm(simple_no_infinity$strike_per_flake~simple_no_infinity$core_weight_initial))[2])),parse=TRUE)

#checking intercept and slope separately

coef(lm(simple_no_infinity$strike_per_flake~simple_no_infinity$core_length))

#fiding the correlation coefficients
cor(simple_no_infinity$strike_per_flake,simple_no_infinity$core_volume)

#scatterplot for core length vs strikes per flake

ggplot(simple_no_infinity, aes(x=core_length, y=strike_per_flake)) +
  geom_point() + ggtitle("Initial Core Length vs Number of Strikes per Flake")+
  xlab("Core length (cm)")+ylab("Number of strikes per flake")+
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  geom_text(aes(x = max(core_length), y = max(strike_per_flake), label = sprintf("y = %.2f + %.2fx", lm(strike_per_flake ~ core_length)$coefficients[1], lm(strike_per_flake ~ core_length)$coefficients[2])), 
            hjust = 1, vjust = 1, size = 5)+
  theme_classic()

#scatterplot for core volume vs strikes per flake

ggplot(simple_no_infinity, aes(x=core_volume, y=strike_per_flake)) +
  geom_point() + ggtitle("Initial Core Volume vs Number of Strikes per Flake")+
  xlab("Core volume (cm3)")+ylab("Number of strikes per flake")+
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  geom_text(aes(x = max(core_volume), y = max(strike_per_flake), label = sprintf("y = %.2f + %.2fx", lm(strike_per_flake ~ core_volume)$coefficients[1], lm(strike_per_flake ~ core_volume)$coefficients[2])), 
            hjust = 1, vjust = 1, size = 5)+
  theme_classic()

#ANOVA for strikes per flake by site and by core size

aov.model <- aov(simple_no_infinity$strike_per_flake ~ simple_no_infinity$core_size)
summary(aov.model)

aov.model <- aov(simple_no_infinity$strike_per_flake ~ simple_no_infinity$site)
summary(aov.model)

aov.model <- aov(simple_no_infinity$strike_per_flake ~ simple_no_infinity$raw_material_type)
summary(aov.model)

# did the knapper's predictions improve over time, or by site? 
#(difficulty difference and difficulty binary)

#over time

ggplot(levallois_simple_data, aes(x=serial_number, y=difficulty_difference)) +
  geom_point() + ggtitle("Difficulty Assessment Score over Time") + 
  xlab("Time") + ylab("Difficulty Assessment Score") + 
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  geom_text(aes(x = max(serial_number), y = max(difficulty_difference), label = sprintf("y = %.2f + %.2fx", lm(difficulty_difference ~ serial_number)$coefficients[1], lm(difficulty_difference ~ serial_number)$coefficients[2])), 
            hjust = 1, vjust = 1, size = 5)+
  theme_classic()

#fiding the correlation coefficients

cor(levallois_simple_data$serial_number,levallois_simple_data$difficulty_difference)

#by site

ggplot(levallois_simple_data, aes (x=difficulty_binary, fill = site))+
  geom_bar(aes(y = (..count..))) + ggtitle("Difference in Knapper's Difficulty Assessment Score before and after knapping")+
  xlab("Difficulty Assessment Scores") + ylab("Counts")+
  scale_fill_manual(values= c("#fbb4ae","#b3cde3", "#ccebc5", "#decbe4"))+
  stat_count(geom = "text", colour = "black", size = 5.5,
             aes(label = ..count..),position=position_stack(vjust=0.5)) +
  theme_light()

#by size

ggplot(levallois_simple_data, aes (x=difficulty_binary, fill = core_size))+
  geom_bar(aes(y = (..count..))) + ggtitle("Difference in Knapper's Difficulty Assessment Score before and after knapping")+
  xlab("Difficulty Assessment Scores") + ylab("Counts")+
  scale_fill_manual(values= c("#e7e1ef","#c994c7", "#756bb1"))+
  stat_count(geom = "text", colour = "black", size = 5.5,
             aes(label = ..count..),position=position_stack(vjust=0.5)) +
  theme_light()

#chisquare of difficulty assessment not possible. Fisher's test:

chisq.test(table(levallois_simple_data$difficulty_binary, levallois_simple_data$site))

fisher.test(table(levallois_simple_data$difficulty_binary, levallois_simple_data$raw_material_type))

fisher.test(table(levallois_simple_data$difficulty_binary, levallois_simple_data$core_size))

#final product

str(levallois_simple_data$core_final_form)
summary(levallois_simple_data$core_final_form)
table(levallois_simple_data$core_final_form)


#-------------------------------------------------------------------------------

#making a new dataset of just individual flake counts using subset function

levallois_flake_count_data <- subset(raw_data_csv, 
                                     select = c(serial_number, flake_serial_number,
                                                core_size, core_length, raw_material_type,
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

flake_est_type$flake_type = factor(flake_est_type$flake_type, levels = 
                                        c('     no_flake','reduction     ', ' bifacial_shaping    ',
                                          '  surface_shaping   ','   platform_preparation  ','    other '))

summary.data.frame(flake_est_type)

# Bar graph for number of bifacial, surface shaping, platform prep flakes across groups

ggplot(flake_est_type, aes(x=site, fill=flake_type)) + 
  geom_bar(aes(y=(..count..))) +
  ggtitle("Frequency of Flake Types")+xlab("Site") + ylab("Count")+
  scale_fill_manual(values=c('#ffffcc','#c7e9b4','#7fcdbb','#41b6c4','#2c7fb8','#253494'),
                    name = "Flake Type", labels = c("No Flake", "Reduction", "Bifacial Shaping",
                                                    "Surface Shaping", "Platform Preperation",
                                                    "Other"))+
  stat_count(geom = "text", colour = "black", size = 4.0,
             aes(label = (..count..)),position=position_stack(vjust=0.5)) +
  theme_classic()

table(flake_est_type$flake_type)

# Bar graph for number of bifacial, surface shaping, platform prep flakes across size groups

ggplot(flake_est_type, aes(x=core_size, fill=flake_type)) + 
  geom_bar(aes(y=(..count..))) +
  ggtitle("Frequency of Flake Types")+xlab("Core Size") + ylab("Count")+
  scale_fill_manual(values=c('#ffffcc','#c7e9b4','#7fcdbb','#41b6c4','#2c7fb8','#253494'),
                    name = "Flake Type", labels = c("No Flake", "Reduction", "Bifacial Shaping",
                                                    "Surface Shaping", "Platform Preperation",
                                                    "Other"))+
  stat_count(geom = "text", colour = "black", size = 4.0,
             aes(label = (..count..)),position=position_stack(vjust=0.5)) +
  theme_classic()

#export flake est type as csv

write.csv(flake_est_type, file="/Users/aditimajoe/Desktop/IISER/levallois_experiment/flake_estimate_raw_materials.csv")

estimate_raw_materials<-read.csv("/Users/aditimajoe/Desktop/IISER/levallois_experiment/estimate_raw_materials.csv")

fisher.test(table(estimate_raw_materials$flake_type, estimate_raw_materials$raw_material_type, simulate.p.value = TRUE, B = 1e6))
chisq.test(table(estimate_raw_materials$flake_type, estimate_raw_materials$raw_material_type, simulate.p.value = TRUE, B = 1e6))

table(estimate_raw_materials$flake_type, estimate_raw_materials$raw_material_type)

# Bar graph for number of bifacial, surface shaping, platform prep flakes across raw materials

ggplot(estimate_raw_materials, aes(x=raw_material_type, fill=flake_type)) + 
  geom_bar(aes(y=(..count..))) +
  ggtitle("Frequency of Flake Types")+xlab("Raw Material") + ylab("Count")+
  scale_fill_manual(values=c('#ffffcc','#c7e9b4','#7fcdbb','#41b6c4','#2c7fb8','#253494'),
                    name = "Flake Type", labels = c("No Flake", "Reduction", "Bifacial Shaping",
                                                    "Surface Shaping", "Platform Preperation",
                                                    "Other"))+
  stat_count(geom = "text", colour = "black", size = 4.0,
             aes(label = (..count..)),position=position_stack(vjust=0.5)) +
  theme_classic()

#making a two way table to show flake type by site
y=table(flake_est_type$flake_type, flake_est_type$site)

#creating a dataframe of the table above
u=as.data.frame.matrix(y)

#graph of flake types

ggplot(flake_est_type, aes(x=flake_type)) + 
  geom_bar(aes(y=(..count..)), fill= "slateblue") +
  ggtitle("Frequency of Flake Types")+xlab("Flake Types") + ylab("Count")+
  scale_x_discrete(labels = c("No Flake", "Reduction", "Bifacial Shaping",
                                                    "Surface Shaping", "Platform Preperation",
                                                    "Other"))+
  stat_count(geom = "text", colour = "white", size = 3.5,
             aes(label = (..count..)),position=position_stack(vjust=0.5)) +
  theme_classic()+
  theme(axis.text.x = element_text(angle=15, hjust = 1))

#Chi square for flake types

flake_type_site_test<-chisq.test(table(flake_est_type$flake_type, flake_est_type$site))

chisq.test(table(flake_est_type$flake_type, flake_est_type$raw_material_type))
fisher.test(table(flake_est_type$flake_type, flake_est_type$raw_material_type))


aov.model <- aov(flake_est_type$flake_type, flake_est_type$core_length)
summary(aov.model)

#reorder estimates

flake_est_type$estimate <- 
  factor(flake_est_type$estimate, levels=c("miss", "om", "ol", "p", "u"))

#predictions of flakes

ggplot(flake_est_type, aes(x=estimate)) + 
  geom_bar(aes(y=(..count..)), fill= "darkblue") +
  ggtitle("Frequency of flake Estimates")+xlab("Knapper's Estimate") + ylab("Count")+
  scale_x_discrete(labels=c("Miss", "Overestimate by more than 50%",
                             "Overestimate by less than 50%", "As predicted", "Underestimate"))+
  stat_count(geom = "text", colour = "white", size = 3.5,
             aes(label = (..count..)),position=position_stack(vjust=0.5)) +
  theme_classic()+
  theme(axis.text.x = element_text(angle=15, hjust = 1))

#predictions of flakes by site

ggplot(flake_est_type, aes(x=site, fill=estimate)) + 
  geom_bar(aes(y=(..count..))) +
  ggtitle("Flake Estimates by Site")+xlab("Site") + ylab("Count")+
  scale_fill_manual(values=c("#f46d43","#fdae61","#fee08b","#d9ef8b","#66bd63"),
                    name="Outcome relative to knapper's estimate",
                    labels=c("Miss", "Overestimate by more than 50%",
                             "Overestimate by less than 50%", "As predicted", "Underestimate"))+
  stat_count(geom = "text", colour = "black", size = 3.5,
             aes(label = (..count..)),position=position_stack(vjust=0.5)) +
  theme_classic()

#summaries
prop.table(table(flake_est_type$estimate))
table(flake_est_type$estimate, flake_est_type$core_size)

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

#looking at shape data

str(raw_core_shape)

raw_core_shape$serial_number<-as.factor(raw_core_shape$serial_number)
levallois_simple_data$serial_number<-as.factor(levallois_simple_data$serial_number)

#select only the variables from both datasets that are required for analysis

simple_data_selected <- select(levallois_simple_data, c(serial_number, site, core_number,
                                                        raw_material_type, core_size, core_length,
                                                        core_weight_initial,
                                                        strikes_total, farthest_stage_reached,
                                                        session_time_hours, plf_detached,
                                                        weight_removed, primary_reason_abandonment))

shape_data_selected <- select(raw_core_shape, c(core_number, elongation_ratio, platyness_ratio,
                                                cobble_shape))

#merge the raw data csv with core shape dataset

merged_shape_variables <- inner_join(simple_data_selected, shape_data_selected, by = c("core_number"))

#basic descriptive shape data

table<-table(merged_shape_variables$cobble_shape)

prop.table(table)

# re-order levels in decreasing order

reorder_shape <- function(x) {
  factor(x, levels = names(sort(table(x), decreasing = TRUE)))
}


# Proportions bar charts
ggplot(merged_shape_variables, aes(x = cobble_shape)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill= "darkgreen") +
  xlab("Cobble Shape") +
  scale_y_continuous(labels = scales::percent, name = "Proportion") +
  scale_x_discrete(labels = c("Blade", "Disc", "Rod", "Sphere"))+
  theme_light()+
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 15))

# stat_count(geom = "text", colour = "black", size = 3.5, aes(label = ..count..),position=position_stack(vjust=0.5)) +
  
#table for PLF success~cobble shape

plf_cobble_shape<-table(merged_shape_variables$cobble_shape,merged_shape_variables$plf_detached)

margin.table(plf_cobble_shape, 1)
prop.table(plf_cobble_shape, margin=1)

# Proportions bar charts
ggplot(merged_shape_variables, aes(x = plf_detached, fill= cobble_shape)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("PLF Detached") +
  scale_y_continuous(labels = scales::percent, name = "Proportion") +
  scale_fill_manual(values= c("#66c2a5","#fdae61", "#8da0cb", "#b2df8a"))+
  scale_x_discrete(labels = c("attempted", "no", "yes"))+
  theme_light()+
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 15))

#hypothesis testing

fisher.test(table(merged_shape_variables$cobble_shape,merged_shape_variables$plf_detached))

#table for farthest stage~cobble shape

stage_cobble_shape<-table(merged_shape_variables$cobble_shape,merged_shape_variables$farthest_stage_reached)

margin.table(stage_cobble_shape, 1)
prop.table(stage_cobble_shape, margin=1)

# Proportions bar charts
ggplot(merged_shape_variables, aes(x = cobble_shape, fill= farthest_stage_reached), stat="identity") +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("Cobble Shape") +
  scale_y_continuous(labels = scales::percent, name = "Proportion") + 
  scale_fill_manual(values= c("#f1eef6","#d0d1e6", "#a6bddb", "#74a9cf", "#3690c0","#0570b0","#034e7b"))+
  scale_x_discrete(labels = c("Blade", "Disc", "Rod","Sphere"))+
  theme_light()+
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 15))
  

#hypothesis testing

fisher.test(table(merged_shape_variables$cobble_shape,merged_shape_variables$farthest_stage_reached),simulate.p.value=TRUE)

#by site

site_cobble_shape <-table(merged_shape_variables$site, merged_shape_variables$cobble_shape)

margin.table(site_cobble_shape, 1)
prop.table(site_cobble_shape, margin = 1)

ggplot(merged_shape_variables, aes(x=site, fill= cobble_shape)) +
  geom_bar(aes(y=(..count..)/sum(..count..))) +
  scale_fill_manual(values=c("#66c2a5","#fdae61", "#8da0cb", "#b2df8a")) +
  xlab("Site") + ylab("Count")+
  scale_y_continuous(labels = scales::percent, name = "Proportion") +
  theme_light()+
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 15))

#hypothesis testing

fisher.test(table(merged_shape_variables$cobble_shape,merged_shape_variables$site))

#abandonment ~ shape
abandon_cobble_shape <-table(merged_shape_variables$cobble_shape, merged_shape_variables$primary_reason_abandonment)

margin.table(abandon_cobble_shape, 1)
prop.table(abandon_cobble_shape, margin = 1)

ggplot(merged_shape_variables, aes(x=primary_reason_abandonment, fill= cobble_shape)) +
  geom_bar(aes(y=(..count..)/sum(..count..))) +
  scale_fill_manual(values=c("#66c2a5","#fdae61", "#8da0cb", "#b2df8a"))+
  xlab("Reason") + ylab("Count")+
  scale_y_continuous(labels = scales::percent, name = "Proportion") +
  theme_light()+
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 15))

#hypothesis testing

fisher.test(table(merged_shape_variables$cobble_shape,merged_shape_variables$primary_reason_abandonment))

#core reduction~core shape

merged_shape_variables %>%
  group_by(cobble_shape) %>%
  summarise_at(vars(weight_removed), list(name = median))

#IQR
r <- merged_shape_variables %>% 
  group_by(cobble_shape) %>%
  summarise(IQR(weight_removed))
r <- data.frame(r)

ggplot(merged_shape_variables, aes(x=cobble_shape, y=weight_removed))+
  geom_boxplot(fill="slateblue", alpha=0.2)+
  xlab("Cobble Shape") + ylab("Relative Weight Removed")

#hypothesis test

aov.model <- aov(merged_shape_variables$weight_removed~merged_shape_variables$cobble_shape)
summary(aov.model)

kruskal.test(merged_shape_variables$weight_removed~merged_shape_variables$cobble_shape)

#session_time~cobble shape

merged_shape_variables %>%
  group_by(cobble_shape) %>%
  summarise_at(vars(session_time_hours), list(name = sd))

#IQR
s <- merged_shape_variables %>% 
  group_by(cobble_shape) %>%
  summarise(IQR(session_time_hours))
s <- data.frame(s)

ggplot(merged_shape_variables, aes(x=cobble_shape, y=session_time_hours))+
  geom_boxplot(fill="slateblue", alpha=0.2)+
  xlab("Cobble Shape") + ylab("Average Length of Knapping Session")

#hypothesis test

aov.model <- aov(merged_shape_variables$session_time_hours~merged_shape_variables$cobble_shape)
summary(aov.model)

kruskal.test(merged_shape_variables$session_time_hours~merged_shape_variables$cobble_shape)

#two-way ANOVA. shape+size~session_time

twoANOVA_size <- aov(session_time_hours~cobble_shape * factor(core_size), data = merged_shape_variables)
summary(twoANOVA_size)

TukeyHSD(twoANOVA_size)

#two-way ANOVA. shape+raw_material~session_time


twoANOVA_material <- aov(session_time_hours~cobble_shape * factor(raw_material_type), data = merged_shape_variables)
summary(twoANOVA_material)

TukeyHSD(twoANOVA_material)

interaction.plot(merged_shape_variables$cobble_shape,
                 merged_shape_variables$raw_material_type, 
                 merged_shape_variables$session_time_hours)

interaction.plot(merged_shape_variables$cobble_shape,
                 merged_shape_variables$core_size, 
                 merged_shape_variables$session_time_hours)

ggplot(merged_shape_variables, aes(x= cobble_shape, y= session_time_hours, colour = raw_material_type))+
  geom_point(aes(shape=raw_material_type), position = position_dodge(width = 5))+
  geom_line(aes(linetype=raw_material_type), position = position_dodge(width = 5))+
  labs(x="Cobble Shape", y = "Session time (hours)")+
  theme_bw()
  
#do practice hours show any difference in variables when subsetted by size?

levallois_small_cores <- filter(levallois_simple_data, core_size=="small")
levallois_medium_cores <- filter(levallois_simple_data, core_size=="medium")
levallois_large_cores <- filter(levallois_simple_data, core_size=="large")


aov.model <- aov(levallois_large_cores$cumulative_practice_hours~levallois_large_cores$farthest_stage_reached)
summary(aov.model)

aov.model <- aov(levallois_medium_cores$cumulative_practice_hours~levallois_medium_cores$difficulty_binary)

summary(aov.model)

#two-way ANCOVA. shape+max length~relative weight removed


res.aov <- merged_shape_variables %>% anova_test(weight_removed ~ core_length + cobble_shape)
get_anova_table(res.aov)


ggplot(merged_shape_variables, aes(x= core_length, y= weight_removed, colour = cobble_shape))+
  geom_point(aes(shape=cobble_shape), position = position_dodge(width = 5), size= 3)+
  geom_smooth(aes(), fill = "white",
              method = "lm", formula = y ~ x,
              size = 3, se=FALSE, fullrange=TRUE) +
  labs(x="Core Length (cm)", y = "Relative Weight Removed (kg)")+
  theme_classic()+
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title=element_text(size=30))

?element_text
