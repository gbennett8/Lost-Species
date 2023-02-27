# This is code to create overall and class-specific density maps of terrestrial 
# vertebrate revenants by country. Data collected by Tom E. Martin and Gareth Bennett
# Code developed by Gareth Bennett 

#Packages 
library(ggplot2)
library(tidyverse)
library(sf)               
library(readxl)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(ggthemes)
theme_set(theme_bw())


#Define countries. Exclude Antarctica 
world <- ne_countries(scale = "medium", returnclass = "sf")%>%
  filter(name!='Antarctica')


#Define Mega Diverse Countries
Mega<-c('United States','Mexico', 'Colombia', 'Ecuador', 'Peru', 'Venezuela', 'Brazil', 
        'Dem. Rep. Congo', 'South Africa', 'Madagascar', 'India', 'Malaysia', 'Indonesia', 
        'Philippines', 'Papua New Guinea','China','Australia')


#read input
rev<- read_csv("Rev Locations.csv")


##Overall (Fig.1)##

#join rev count & Megadiverse with map data. 
worldt <- left_join(world, rev$Total, by="name_long")%>%
  mutate(`Species Richness` = case_when(
    name %in% Mega ~'Mega Diverse',
    TRUE~'Normal') %>% 
      factor(levels = c('Normal','Mega Diverse')))

#name legend
colnames(worldt)[which(names(worldt) == "Total")] <- "Revenants"

#plot map
T <-ggplot(data = worldt) +
  geom_sf(aes(fill = Revenants, colour= `Species Richness`),
          lwd = ifelse(worldt$name %in% Mega, 0.7, 0.2))+
  theme_map()+
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(fill = 'aliceblue'))+
  scale_fill_gradient(low = "beige", high = "Red",limits=c(0,80))+
  scale_color_manual(values = c( "#666666","black"))+
  ggtitle('Terrestrial Vertebrates')+
  theme(legend.position = 'right')+
  guides(colour=FALSE)

T
##MAMMALS (Fig.2)##

#join rev count & Megadiverse with map data. 
worldm<- left_join(world, rev$Mammals, by="name_long" )%>%
  mutate(`Species Richness` = case_when(
    name %in% Mega ~'Mega Diverse',
    TRUE~'Normal') %>% 
      factor(levels = c('Normal','Mega Diverse')))

#name legend
colnames(worldm)[which(names(worldm) == "Mammals")] <- "Revenants"

#plot map
M<-ggplot(data = worldm) +
  geom_sf(aes(fill = Revenants, colour=`Species Richness`),
          lwd = ifelse(worldm$name %in% Mega, 0.7, 0.5))+
  theme_map()+
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(fill = 'aliceblue'))+
  scale_fill_gradient(low = "beige", high = "Red",limits=c(0,30))+
  scale_color_manual(values = c( "#666666","black"))+
  ggtitle('Mammals')+
  theme(legend.position = 'right')+
  guides(colour=FALSE)
M
##BIRDS (Fig.2)##


#join rev count & Megadiverse with map data. 
worldb<- left_join(world, rev$Birds, by="name_long")%>%
  mutate(`Species Richness` = case_when(
    name %in% Mega ~'Mega Diverse',
    TRUE~'Normal') %>% 
      factor(levels = c('Normal','Mega Diverse')))

#name legend
colnames(worldb)[which(names(worldb) == "Birds")] <- "Revenants"

#plot map
B<-ggplot(data = worldb) +
  geom_sf(aes(fill = Revenants, colour=`Species Richness`),
          lwd = ifelse(worldb$name %in% Mega, 0.7, 0.2))+
  theme_map()+
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(fill = 'aliceblue'))+
  scale_fill_gradient(low = "beige", high = "Red",limits=c(0,30))+
  scale_color_manual(values = c( "#666666","black"))+
  ggtitle('Birds')+
  theme(legend.position = 'right')+
  guides(colour=FALSE)
B
##REPTILES (Fig.2)##

#join rev count & Megadiverse with map data. 
worldr<- left_join(world, rev$Reptiles, by="name_long")%>%
  mutate(`Species Richness`= case_when(
    name %in% Mega ~'Mega Diverse',
    TRUE~'Normal') %>% 
      factor(levels = c('Normal','Mega Diverse')))

#name legend
colnames(worldr)[which(names(worldr) == "Reptiles")] <- "Revenants"

#plot map
R<-ggplot(data = worldr) +
  geom_sf(aes(fill = Revenants, colour= `Species Richness`),
          lwd = ifelse(worldr$name %in% Mega, 0.7, 0.2))+
  theme_map()+
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(fill = 'aliceblue'))+
  scale_fill_gradient(low = "beige", high = "Red",limits=c(0,30))+
  scale_color_manual(values = c( "#666666","black"))+
  ggtitle('Reptiles')+
  theme(legend.position = 'right')+
  guides(colour=FALSE)
R
##AMPHIBIANS (Fig.2)##

#join rev count & Megadiverse with map data. 
worlda <- left_join(world, rev$Amphibians, by="name_long")%>%
  mutate(`Species Richness` = case_when(
    name %in% Mega ~'Mega Diverse',
    TRUE~'Normal') %>% 
      factor(levels = c('Normal','Mega Diverse')))

#name legend
colnames(worlda)[which(names(worlda) == "Amphibians")] <- "Revenants"

#plot map
A<-ggplot(data = worlda) +
  geom_sf(aes(fill = Revenants, colour= `Species Richness`),
          lwd = ifelse(worlda$name %in% Mega, 0.7, 0.2))+
  theme_map()+
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(fill = 'aliceblue'))+
  scale_fill_gradient(low = "beige", high = "Red",limits=c(0,30))+
  scale_color_manual(values = c( "#666666","black"))+
  ggtitle('Amphibians')+
  theme(legend.position = 'right')+
  guides(colour=FALSE)


z


#Run
T #Overall Map
M #Mammal Map
B #Bird Map
R #Reptile Map
A #Amphibian Map


#Save#
ggsave("map_weba.png", width = 10, height = 8, dpi = 300,units='in',device='png')


