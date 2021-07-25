
# Read data for the analysis
GB <- read.csv(file = "https://raw.githubusercontent.com/cheolminlee/Capstone_project/main/Ground_Beetle_Data.csv",header = TRUE)
EN <- read.csv(file = "https://raw.githubusercontent.com/cheolminlee/Capstone_project/main/Community_Index.csv",header = TRUE)

# Install R packages needed for the analysis 
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(vegan)) install.packages("vegan", repos = "http://cran.us.r-project.org")
if(!require(permute)) install.packages("permute", repos = "http://cran.us.r-project.org")
if(!require(lattice)) install.packages("lattice", repos = "http://cran.us.r-project.org")

library(dplyr)
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(corrplot)
library(vegan)
library(permute)
library(lattice)

###################################
#Species richness of ground beetles
###################################

# Show total species richness of ground beetles collected in nine urban green areas
n_distinct(GB$ï..Species)

# Show species richness of ground beetles according to body size
Number_Body = GB %>%
  group_by(Body.size)%>%
  summarise((Number_abundace=n_distinct(ï..Species)))
Number_Body

# Show species richness of ground beetles according to habitat type
Number_Habitat_Type = GB %>%
  group_by(Habitat.type)%>%
  summarise((Number_abundace=n_distinct(ï..Species)))
Number_Habitat_Type

############################
#Abundance of ground beetles
############################

# Show total abundance of ground beetles
sum(GB$Abundance)

# Show abundance of ground beetles according to body size
Abundace_Body = GB %>%
  group_by(Body.size)%>%
  summarise((Abundance_Body=sum(Abundance)))
Abundace_Body

# Abundance of ground beetles according to habitat type 
Abundace_Habitat_Type = GB %>%
  group_by(Habitat.type)%>%
  summarise((Abundance_Habitat_Type=sum(Abundance)))
Abundace_Habitat_Type

#####################################################
# Make graphs according to the data of ground beetles
#####################################################


# Reoder nine study sites according to area
GB$Acronym <- factor(GB$Acronym, levels =c ("SU","CH","TA","IZ","NI","KU","UM","KO","KM"))

# Make graph for species richness of ground beetles in nine urban green area
plot1<- ggplot(GB, aes(Acronym)) + geom_bar(fill = "cadetblue2") +
  labs(y = "Species richness", x = "Study sites")

# Make graph for abundance of ground beetles in nine urban green area
plot2<- ggplot(data=GB, aes(x=Acronym, y=Abundance))+geom_bar(stat="identity", fill = "71b7begreen")+
  labs(y = "Aundance", x = "Study sites")

# Make Figure 1 with Plot1 and Plot2
grid.arrange(plot1, plot2, nrow=1, ncol=2, bottom="Figure 1. Species richness and abundance of ground beetles in nine urban green areas.")


# Make graph for species richness of ground beetles according to body size
Plot3<- ggplot(GB, aes(Acronym, fill=Body.size))+geom_bar(stat = "count")+
  theme(legend.position ="top")+
  scale_fill_discrete(name = "Body size")+
  labs(y = "Species richness", x = "Study sites")

# Make graph for species richness of ground beetles according to habitat type
Plot4<- ggplot(GB, aes(Acronym, fill=Habitat.type))+geom_bar(stat = "count")+
  theme(legend.position ="top")+
  scale_fill_discrete(name = "Habitat type")+
  labs(y = "Species richness", x = "Study sites")

# Make graph for abundance of ground beetles according to body size
Plot5<- ggplot(GB, aes(x=Acronym, y=Abundance, fill=Body.size))+geom_bar(stat="identity")+
  theme(legend.position ="none")+
  labs(y = "Abundace", x = "Study sites")

# Make graph for abundance of ground beetles according to habitat type
Plot6<- ggplot(GB, aes(x=Acronym, y=Abundance, fill=Habitat.type))+geom_bar(stat="identity")+
  theme(legend.position ="none")+      
  labs(y = "Abundace", x = "Study sites")

# Make Figure 2.
grid.arrange(Plot3, Plot4, Plot5, Plot6, nrow=2, ncol=2, bottom="Figure 2. Species richness and abundance of ground beetles according to body size and habitat type in nine urban green areas.")

######################################################################################
# Make bar graphs for species richness of body size and habitat type according to area
######################################################################################

# Reoder according to area
EN$Area <- factor(EN$Area, levels =c ("Small area","Medium area","Large area"))

# Make bar graph for species richness of ground beetles according to area
EN1 <- group_by(EN,Area) %>% summarise(mean=mean(Speciesrichness),sd=sd(Speciesrichness))
Plot7 <- ggplot(EN1,aes(x=Area,y=mean))+
  geom_bar(stat="identity",position="dodge", fill = "#99CC00") +
  labs(y = "Species richness", x = "Study sites")+
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),width=0.25,
                size=1,position= position_dodge(0.9),alpha=0.3)

# Make bar graph for species richness of small-sized species according to area
EN2 <- group_by(EN,Area) %>% summarise(mean=mean(Small_species),sd=sd(Small_species))
Plot8 <- ggplot(EN2,aes(x=Area,y=mean))+
  geom_bar(stat="identity",position="dodge", fill = "#FFCC33") +
  labs(y = "Small-sized species", x = "Study sites")+
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),width=0.25,
                size=1,position= position_dodge(0.9),alpha=0.3)

# Make bar graph for species richness of medium-sized species according to area
EN3 <- group_by(EN,Area) %>% summarise(mean=mean(Medium_species),sd=sd(Medium_species))
Plot9 <- ggplot(EN3,aes(x=Area,y=mean))+
  geom_bar(stat="identity",position="dodge", fill = "#FF6633") +
  labs(y = "Medium-sized species", x = "Study sites")+
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),width=0.25,
                size=1,position= position_dodge(0.9),alpha=0.3)

# Make bar graph for species richness of large-sized species according to area
EN4 <- group_by(EN,Area) %>% summarise(mean=mean(Large_species),sd=sd(Large_species))
Plot10 <- ggplot(EN4,aes(x=Area,y=mean))+
  geom_bar(stat="identity",position="dodge", fill = "#CC0033") +
  labs(y = "Large-sized species", x = "Study sites")+
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),width=0.25,
                size=1,position= position_dodge(0.9),alpha=0.3)

# Make bar graph for species richness of forest species according to area
EN5 <- group_by(EN,Area) %>% summarise(mean=mean(Forest_species),sd=sd(Forest_species))
Plot11 <- ggplot(EN5,aes(x=Area,y=mean))+
  geom_bar(stat="identity",position="dodge", fill = "#3399FF") +
  labs(y = "Forest species", x = "Study sites")+
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),width=0.25,
                size=1,position= position_dodge(0.9),alpha=0.3)

# Make bar graph for species richness of open land species according to area
EN6 <- group_by(EN,Area) %>% summarise(mean=mean(Openland_species),sd=sd(Openland_species))
Plot12 <- ggplot(EN6,aes(x=Area,y=mean))+
  geom_bar(stat="identity",position="dodge", fill = "#CC00CC") +
  labs(y = "Open land species", x = "Study sites")+
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),width=0.25,
                size=1,position= position_dodge(0.9),alpha=0.3)

# Make Figure 3 with Plot7, Plot8, Plot9, Plot10, Plot11, and Plot12
grid.arrange(Plot7, Plot8, Plot9, Plot10, Plot11, Plot12, nrow=4, ncol=2, bottom="Figure 3. Species richness of ground beetles, body size, and habitat type according to area. The error bars indicate one SD.")


###############################################################################
# Make bar graphs for abundance of body size and habitat type according to area
###############################################################################

# Make bar graphs for density of ground beetles according to area
EN7 <- group_by(EN,Area) %>% summarise(mean=mean(Density),sd=sd(Density))
Plot13 <- ggplot(EN7,aes(x=Area,y=mean))+
  geom_bar(stat="identity",position="dodge", fill = "#99CC00") +
  labs(y = "Density (Abundance per 20 traps)", x = "Study sites")+
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),width=0.25,
                size=1,position= position_dodge(0.9),alpha=0.3)

# Make bar graphs for density of small-sized species according to area
EN8 <- group_by(EN,Area) %>% summarise(mean=mean(Small_density),sd=sd(Small_density))
Plot14 <- ggplot(EN8,aes(x=Area,y=mean))+
  geom_bar(stat="identity",position="dodge", fill = "#FFCC33") +
  labs(y = "Small-sized species", x = "Study sites")+
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),width=0.25,
                size=1,position= position_dodge(0.9),alpha=0.3)

# Make bar graphs for density of medium-sized species according to area
EN9 <- group_by(EN,Area) %>% summarise(mean=mean(Medium_density),sd=sd(Medium_density))
Plot15 <- ggplot(EN9,aes(x=Area,y=mean))+
  geom_bar(stat="identity",position="dodge", fill = "#FF6633") +
  labs(y = "Medium-sized species", x = "Study sites")+
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),width=0.25,
                size=1,position= position_dodge(0.9),alpha=0.3)

# Make bar graphs for density of large-sized species according to area
EN10 <- group_by(EN,Area) %>% summarise(mean=mean(Large_density),sd=sd(Large_density))
Plot16 <- ggplot(EN10,aes(x=Area,y=mean))+
  geom_bar(stat="identity",position="dodge", fill = "#CC0033") +
  labs(y = "Large-sized species", x = "Study sites")+
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),width=0.25,
                size=1,position= position_dodge(0.9),alpha=0.3)

# Make bar graphs for density of forest species according to area
EN11 <- group_by(EN,Area) %>% summarise(mean=mean(Forest_density),sd=sd(Forest_density))
Plot17 <- ggplot(EN11,aes(x=Area,y=mean))+
  geom_bar(stat="identity",position="dodge", fill = "#3399FF") +
  labs(y = "Forest species", x = "Study sites")+
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),width=0.25,
                size=1,position= position_dodge(0.9),alpha=0.3)

# Make bar graphs for density of open land species according to area
EN12 <- group_by(EN,Area) %>% summarise(mean=mean(Openland_density),sd=sd(Openland_density))
Plot18 <- ggplot(EN12,aes(x=Area,y=mean))+
  geom_bar(stat="identity",position="dodge", fill = "#CC00CC") +
  labs(y = "Open land species", x = "Study sites")+
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),width=0.25,
                size=1,position= position_dodge(0.9),alpha=0.3)

# Make Figure 4 with Plot13, Plot14, Plot15, Plot16, Plot17, and Plot18
grid.arrange(Plot13, Plot14, Plot15, Plot16, Plot17, Plot18, nrow=4, ncol=2, bottom="Figure 4. Density (Abundance per 20 traps) of ground beetles, body size, and habitat type according to area. The error bars indicate one SD.")

# Do One-Way ANOVA test for community index according to area
ANOVA1<- aov(EN$Speciesrichness~EN$Area)
ANOVA2<- aov(EN$Small_species~EN$Area)
ANOVA3<- aov(EN$Medium_species~EN$Area)
ANOVA4<- aov(EN$Large_species~EN$Area)
ANOVA5<- aov(EN$Forest_species~EN$Area)
ANOVA6<- aov(EN$Openland_species~EN$Area)
ANOVA7<- aov(EN$Density~EN$Area)
ANOVA8<- aov(EN$Small_density~EN$Area)
ANOVA9<- aov(EN$Medium_density~EN$Area)
ANOVA10<- aov(EN$Large_density~EN$Area)
ANOVA11<- aov(EN$Forest_density~EN$Area)
ANOVA12<- aov(EN$Openland_density~EN$Area)

# show the result of One-way ANOVA test for community index according to area
summary(ANOVA1)
summary(ANOVA2)
summary(ANOVA3)
summary(ANOVA4)
summary(ANOVA5)
summary(ANOVA6)
summary(ANOVA7)
summary(ANOVA8)
summary(ANOVA9)
summary(ANOVA10)
summary(ANOVA11)
summary(ANOVA12)

# Test pearson correlation analysis between ground beetles index and land use
# Percentage of eight land use categories (paddy, field, park and grren space, forest, urban area, road, open space, river and pond) with the range of 500 m from the edge of nine urban green areas
EN1 <- select(EN, -1:-2,-25:-59) 
EN2<- cor(EN1)
testRes = cor.mtest(EN2, conf.level = 0.95)
corrplot(EN2, p.mat = testRes$p, method = 'color', diag = FALSE, type = 'upper',tl.col = 'black',
        sig.level = c(0.001, 0.01, 0.05), pch.cex = 0.9, 
        insig = 'label_sig', pch.col = 'grey20', order = 'AOE')

# Figure 5. Pearson's correlation coefficient between ground beetles index and land use category of nine urban green areas.* p < 0.05, ** p < 0.01, *** p < 0.001.


# Make data set for Non-metirc Multidimensional scaling
groundbeetle <- select(EN, -1:-24)
groundbeetle.envi <- select(EN, -1,-3:-14,-25:-59)
groundbeetle.envi_2 <- groundbeetle.envi[,2:11]

# Test Non-metirc Multidimensional scaling
ord <- metaMDS(groundbeetle)
plot(ord, type ="t")

# Fit an environmental variables onto an ourdination
gb <- envfit(ord,groundbeetle.envi_2)
plot(gb)

# Show groups in ordinaiton diagrams
ordihull(ord,groups=groundbeetle.envi$Area,draw="polygon",label=T)
# Figure 6. Non???metric multidimensional scaling (NMDS) ordination of ground beetle communities in nine urban green areas.

# Test permutational multivariate analysis of variance using distance matrices
adonis(groundbeetle ~ Area, groundbeetle.envi)



