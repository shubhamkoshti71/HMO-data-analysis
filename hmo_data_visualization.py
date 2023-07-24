
''' DATA VISUALIZATION
	In this section, we will attempt to look at some graphs and charts to get a better idea of the variables and how they are spread out. We will also look at some histograms, some bar charts, scatterplots, and even a map of the different states and regions to see how the cost of each individual's insurance policy varies.'''
    
    
#In this section, we will try to visualize the patterns between categorical attributes with the cost attribute. Also, analyse the pattern of cost attribute

#Here we're using gridExtra package for arranging multiple graphs in one pane
install.packages("gridExtra")

library(gridExtra)

ggplot(HMO_data_new) + geom_histogram(aes(x=cost), col='black', fill='cyan', binwidth = 1000) + ggtitle("Cost attribute analysis") + scale_x_continuous(breaks = seq(0, 50000, by = 5000))
#We can observe that most of the patients in the dataset are incurring costs in the range of $0-10000, with the graph showing a right-skewed pattern


#Plotting box-whisker plots to analyse the effect of each categorical attribute on the 'Cost' attribute
g1 <- ggplot(HMO_data_new) + geom_boxplot(aes(x=smoker, y=cost), col="black", fill = 'cyan') + ggtitle("Smoker vs Cost")
g1 <- g1 +  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

g2 <- ggplot(HMO_data_new) + geom_boxplot(aes(x=education_level, y=cost), col="black", fill = 'green', ) + ggtitle("Education Level vs Cost")
g2 <- g2 +  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

g3 <- ggplot(HMO_data_new) + geom_boxplot(aes(x=yearly_physical, y=cost), col="black", fill = 'orange') + ggtitle("Yearly Physical vs Cost")
g3 <- g3 +  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
g4 <- ggplot(HMO_data_new) + geom_boxplot(aes(x=exercise, y=cost), col="black", fill = 'purple') + ggtitle("Exercise vs Cost")
g4 <- g4 +  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

g5 <- ggplot(HMO_data_new) + geom_boxplot(aes(x=married, y=cost), col="black", fill = 'pink') + ggtitle("Marraige Status vs Cost")
g5 <- g5 +  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

g6 <- ggplot(HMO_data_new) + geom_boxplot(aes(x=gender, y=cost), col="black", fill = 'yellow') + ggtitle("Gender vs Cost")
g6 <- g6 +  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


grid.arrange(g1, g2, nrow=1)
grid.arrange(g3, g4, nrow=1)
grid.arrange(g5, g6, nrow=1)

#We can observe that for each attribute, there is a category for which the overall cost incurred by people is higher as compared to the other categories.
#Although, these graphs don't provide enough information about which of the attribute is most closely related with cost


#Map plot to analyse how various regions affect the cost expenditure of patients in the dataset

#We will plot a map of all the US states and have the color represent the cost of expenditure for each state.

#For this we will use the ggplot2 library

install.packages('ggplot2')
install.packages('dplyr')
install.packages('maps')
install.packages('mapproj')

library(ggplot2)
library(dplyr)
library(maps)
library(mapproj)

states <- map_data("state")
states$state_name <- tolower(states$region)
HMO_data_new$location <- tolower(HMO_data_new$location)
HMO_data_with_states <- merge(HMO_data_new, states, all.y=TRUE, by.x="location", by.y="region")

HMO_data_with_states <- HMO_data_with_states %>% arrange(order)

ggplot(HMO_data_with_states) + geom_polygon(color="black",
                 aes(x=long,y=lat,group=group,fill=cost)) +
        ggtitle('Cost of insurance per state') +
coord_map()

#As we can see, the dataset is limited to some of the northeastern states (7), and it shows the average
#cost per state of insurance. It is evident that Maryland has a higher average than other states.


#Creating a bar graph representing the count of people from different states.

countPlot <- ggplot(data = HMO_data_new) + aes(x=location) + geom_bar(fill='purple')
countPlot

#Creating a bar graph representing the count of people from urban or rural location types.

locationTypePlot <- ggplot(data = HMO_data_new) + aes(x=location_type) + geom_bar(fill='violet')
locationTypePlot

#Creating a bar graph representing the distribution based on gender.

genderPlot <- ggplot(data = HMO_data_new) + aes(x=gender) + geom_bar(fill='lightgreen')
genderPlot

#For the next visualization, we will create a new column in our dataframe based on the existing 'age' column.
#This column will divide our dataset into 4 age categories: '18-25', '26-40', '41-55', and '55+'.

HMO_data_new <- HMO_data_new %>% mutate(age_group =
                     case_when(age <= 25 ~ "18-25", 
                               age <= 40 ~ "26-40",
                               age <= 55 ~ "41-55",
                               age >55 ~ "55+")
)

#We will now look at how our dataset is divided based on the age groups.

ageGroupPlot <- ggplot(data = HMO_data_new) + aes(x=age_group) + geom_bar(fill='orange')
ageGroupPlot

''' Now, we will try to identify variables that have a significant impact on the cost variable. Firstly, we will visualize the relationship of multiple variables with cost variable using scatterplots. Then, we'll run regression models to study the dependancy of cost variables with multiple variables '''

#We can observte that there is a slight linear relationship between age and cost variables. Although, the age variable alone wouldn't be sufficient to predict the 
#cost values accurately
ggplot(HMO_data_new, aes(x=age, y=cost)) + geom_point() + geom_smooth(method = "lm", se = FALSE) 

#We can observte that there is a slight linear relationship between bmi and cost variables. Although, the bmi variable alone wouldn't be sufficient to predict the 
#cost values accurately
ggplot(HMO_data_new, aes(x=bmi, y=cost)) + geom_point() + geom_smooth(method = "lm", se = FALSE)

