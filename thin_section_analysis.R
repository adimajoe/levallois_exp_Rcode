setwd("/Users/aditimajoe/Desktop/IISER/levallois_experiment")
getwd()

library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)

sandstone_thin_raw<-read.csv("/Users/aditimajoe/Desktop/IISER/levallois_experiment/sandstone_thin_section.csv")

str(sandstone_analysis)

sandstone_analysis <- sandstone_thin_raw %>% select(-c(Presence.of.Quartz..Volcanic.rock.,
                                                       other_features, Notes))

ggplot(sandstone_analysis, aes(x=rock_type, fill=site))+
  geom_bar(aes(y=..count..))+
  ggtitle("Frequency of types of Sandstone")+
  stat_count(geom = "text", colour = "black", size = 3.5,
             aes(label = ..count..),position=position_stack(vjust=0.5)) +
  theme_classic()

ggplot(sandstone_analysis, aes(x=other_minerals, fill=rock_type))+
  geom_bar(aes(y=..count..))+
  ggtitle("other minerals")+
  stat_count(geom = "text", colour = "black", size = 3.5,
             aes(label = ..count..),position=position_stack(vjust=0.5)) +
  theme_classic()


ggplot(sandstone_analysis, aes(x=rock_type, fill=cementation))+
  geom_bar(aes(y=..count..))+
  ggtitle("Frequency of types of cementation on different rock types")+
  stat_count(geom = "text", colour = "black", size = 3.5,
             aes(label = ..count..),position=position_stack(vjust=0.5)) +
  theme_classic()

ggplot(sandstone_analysis, aes(x=grain_sorting, y=grain_size_texture, label=rock_type, colour=rock_type))+
  geom_point() +geom_text(hjust=0, vjust=0) + 
  theme_classic()


ggplot(sandstone_analysis, aes(x=grain_rounding, y=grain_size_texture, label=rock_type, colour=rock_type))+
  geom_point() +geom_text(hjust=0, vjust=0) + 
  theme_classic()


ggplot(sandstone_analysis, aes(x=grain_rounding, y=grain_sorting, label=rock_type, colour=rock_type))+
  geom_point() +geom_text(hjust=0, vjust=0) + 
  theme_classic()

#join the sorting and rounding and texture into one category

sandstone_analysis$combined_text_round_sort <- paste(sandstone_analysis$grain_size_texture,sandstone_analysis$grain_rounding, sandstone_analysis$grain_sorting)

sandstone_analysis$combined_text_round_sort <- is.factor(sandstone_analysis$combined_text_round_sort)

str(sandstone_analysis$combined_text_round_sort)

ggplot(sandstone_analysis, aes(x=site, fill=combined_text_round_sort))+
  geom_bar(aes(y=..count..))+
  stat_count(geom = "text", colour = "black", size = 3.5,
             aes(label = ..count..),position=position_stack(vjust=0.5)) +
  theme_classic()

#sum of texture, rounding and sorting

sandstone_analysis$sum_text_round_sort<- sandstone_analysis$grain_size_texture+sandstone_analysis$grain_rounding+
  sandstone_analysis$grain_sorting

str(sandstone_analysis$sum_text_round_sort)

sandstone_analysis$sum_text_round_sort <- as.factor(sandstone_analysis$sum_text_round_sort)

ggplot(sandstone_analysis, aes(x=sum_text_round_sort, fill=rock_type))+
  geom_bar(aes(y=..count..))+
  stat_count(geom = "text", colour = "black", size = 3.5,
             aes(label = ..count..),position=position_stack(vjust=0.5)) +
  theme_classic()


ggplot(sandstone_analysis, aes(x=grain_sorting, fill=rock_type))+
  geom_bar(aes(y=..count..))+
  stat_count(geom = "text", colour = "black", size = 3.5,
             aes(label = ..count..),position=position_stack(vjust=0.5)) +
  theme_classic()


#sum texxture and rounding

sandstone_analysis$sum_text_round<- sandstone_analysis$grain_size_texture+sandstone_analysis$grain_rounding

sandstone_analysis$sum_text_sort<- sandstone_analysis$grain_size_texture+sandstone_analysis$grain_sorting

sandstone_analysis$sum_round_sort<- sandstone_analysis$grain_rounding+sandstone_analysis$grain_sorting


ggplot(sandstone_analysis, aes(x=sum_round_sort, y=grain_size_texture, label=rock_type, colour=rock_type))+
  geom_point() +geom_text(hjust=0, vjust=0) + 
  theme_classic()
