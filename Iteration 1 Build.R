getwd()


setwd("C:/Users/HP/Documents/Iteration 1 Datasets/Datasets")

library(dplyr)
library(shiny)
library(lubridate)
library(httr)
library(tidyverse)
library(stringr)
library(plotly)
library(reshape2)
library(viridis)
library(hrbrthemes)
library(readxl)

#Read the 'out of schoool' statistics Global dataset.
Out_of_school <- read.csv("3- number-of-out-of-school-children.csv")

names(Out_of_school) <- c("Country", "Country_code","Year", "Count_of_Males","Count_of_Females")


Out_of_school_Aus <- Out_of_school %>% 
                      filter(Country == "Australia")

#Filter out Years Greater than or equal to 2015 to consider most recent data.
Out_of_school_Aus <- Out_of_school_Aus %>% 
  filter(Year >= 2015)

#Rename the columns of the dataset.
names(Out_of_school_Aus) <- c("Country", "Country_code","Year", "Count_of_Males","Count_of_Females")

#Remove Unwanted columns 'Country' and 'Country_code'.
Out_of_school_Aus <- Out_of_school_Aus[, -c(1, 2)]

#Change the shape of dataset to long form.
Out_of_school_Aus_melt <- melt(Out_of_school_Aus, id.vars='Year')
#head(Out_of_school_Aus_melt)

#Plot To show the number of Out of School Children in Australia.
ggplot(data = Out_of_school_Aus_melt, aes(x = Year, y = value, fill=variable)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.75)  +
  scale_fill_viridis(discrete = T) +
  ylim(0,100000) +
  #geom_label(label= Out_of_school_Aus_melt$value, show_guide = F,color = "black" ) +
  # geom_text( fontface = "bold", vjust = 1.5,
  #          position = position_dodge(.9), size = 4) +
  labs(x = "\n Year", y = "Count", title = "\n Number of 'Out of School' children in Australia. \n") +
  labs(fill = " ") +
 # geom_text(show.legend = FALSE) +
  theme(plot.title = element_text(hjust = 0.5, size = 16), 
        axis.title.x = element_text(face="bold", colour="black", size = 12),
        axis.title.y = element_text(face="bold", colour="black", size = 12),
        legend.title = element_text(face="bold", size = 10))



#Filter out the developed "5 Eyes" countries (excluding USA as it's an outlier) for comparison.
Out_of_school_5Eyes <- Out_of_school %>% 
  filter(Country == "Australia" |Country == "Canada"|Country == "New Zealand"|Country == "United Kingdom")

#Filter out Years Greater than or equal to 2015 to consider most recent data.
Out_of_school_5Eyes <- Out_of_school_5Eyes %>% 
  filter(Year >= 2015)

#Create new column to get combined count of both the genders.
Out_of_school_5Eyes$Count_Both_Genders <- Out_of_school_5Eyes$Count_of_Males + Out_of_school_5Eyes$Count_of_Females

#Plot Stacked bar graph to compare 'out of school' children of different countries.
ggplot(Out_of_school_5Eyes, aes(y=Count_Both_Genders, x=Country, fill=as.factor(Year))) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  #geom_label(label= Out_of_school_5Eyes$Count_Both_Genders,nudge_x = 0.25, nudge_y = 0.25, 
  #           check_overlap = T, size = 2.8)+
  ggtitle("Comparison of 'Out of school' children of developed countries from year 2015-2018.") +
  theme(plot.title = element_text(size = 18)) +
  labs(fill = " ") +
  xlab("Country") + ylab("Count") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

#Sum the values in all the years for different countries using groupby.
Out_of_school_5Eyes_summ <- Out_of_school_5Eyes %>%
  group_by(Country) %>%
  summarize(total_count = sum(Count_Both_Genders))

#Calculate percentages for pie chart.
Out_of_school_5Eyes_summ$percent <- paste0(round(100 * Out_of_school_5Eyes_summ$total_count / sum(Out_of_school_5Eyes_summ$total_count), 1), "%")

# Pie Chart to show the split of 'Out of School Children of developed countries'.
# Plot the piechart using ggplot.
piecchart <- ggplot(Out_of_school_5Eyes_summ, aes(x="", y=total_count, fill=Country)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y") + #theme_ipsum() +
  ggtitle ("Out of School children split between developed countries(2015-2018).") +
  theme(plot.title = element_text(size = 18)) +
  scale_fill_brewer(palette="Set1") 

piecchart + geom_text(aes(label = percent), position = position_stack(vjust = 0.5))



#### TO show Children's connectedness level with their schools."#############
#Input the dataset.
Children_connect <- read_excel("C:/Users/HP/Documents/Iteration 1 Datasets/Datasets/VCAMS_Indicator_10_6-ChildrenConnectedWithTheirSchool.xlsx", sheet ="10.6 Years 7-9" )

#Filter out data only for Victoria as a whole.
Children_connect <- Children_connect %>% 
  filter(LGA == "Victoria")

#Add percentage column.
Children_connect$percent = Children_connect$Indicator*100

#Plot To show the percentage of connectednes of school for different years.
ggplot(data = Children_connect, aes(x = Year, y = percent)) +
  geom_bar(stat = "identity", alpha = 0.75, fill="darkorange")  +
  ylim(0,100) +
  theme(plot.title = element_text(size = 18)) +
  #scale_fill_viridis(discrete = F) +
  geom_label(label= Children_connect$percent, show_guide = F,color = "black", fill="white") +
  # geom_text( fontface = "bold", vjust = 1.5,
  #          position = position_dodge(.9), size = 4) +
  labs(x = "\n Year", y = "Percent", title = "\n Children connectedness with their school percentage in Victoria(2006-13). \n") +
  scale_x_discrete(limits = unique(Children_connect$Year))
  


Retention_Rate <- read_excel("C:/Users/HP/Documents/Iteration 1 Datasets/Datasets/Graph 5. Year 7_8 to 12 full-time apparent retention rates by sex, Australia, 2013 to 2023.xlsx", sheet = 1)

#Input the Retention Rate Datasets :-
Retention_Rate <- read_excel("C:/Users/HP/Documents/Iteration 1 Datasets/Datasets/Graph5.xlsx")


names(Retention_Rate) <- c("Year", "Male_percentage","Female_Percentage", "Total_percent")

Retention_Rate$Year <- as.numeric(Retention_Rate$Year)

Retention_Rate <- Retention_Rate %>% 
  filter( Year >= 2015)


#Plot To show the retention rates of school children(Year7_8) in Australia among different years.
ggplot(data = Retention_Rate, aes(x = Year, y = Total_percent)) +
  geom_bar(stat = "identity", alpha = 0.75, fill = "blue")  +
  ylim(0,100) +
  theme(plot.title = element_text(size = 18)) +
  #scale_fill_viridis(discrete = F) +
  geom_label(label= Retention_Rate$Total_percent, show_guide = F,color = "black", fill="white") +
  # geom_text( fontface = "bold", vjust = 1.5,
  #          position = position_dodge(.9), size = 4) +
  labs(x = "\n Year", y = "Percent", title = "\n Retention Rate of Children in Australia for different years. \n") +
  scale_x_discrete(limits = unique(Retention_Rate$Year))


