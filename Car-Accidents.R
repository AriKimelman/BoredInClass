
# Search "Question" for questions I would like to figure out.  


## Dataset Retrieval 
# https://smoosavi.org/datasets/us_accidents

#import dataset
d = read.csv("Car-Accidents.csv")

#Package loading
library(lattice)
library(ggplot2)
library(dplyr)

##--------- Potential Research questions----------------- ##

# is there a tendency to have more car accidents at warmer temperatures? 
# plot car accidents frequency based on weather conditions 
# more collisions during the day or night? (while accounting for the fewer num of cars at night)
# increase/decrease of car accidents if there are speed bumps near by 
# stop signs near by increase or decrease collisions? 

-----------------------------------------------------------
  
##--------Final Research Question----------------##  

# original question (can't answer): "whether weather results in more deaths or collisions"  
# new question (can answer): Does weather impact the 'length of the time' of the accident?"
-----------------------------------------------------------

##---------- Data Cleaning-------------##

head(d)

#shorten dataset keeping 500 rows due to R not being able to view dataset (too large)

## d1 <- d[-c(5000:nrow(d)), ]##

#change to random selection of rows due to an error 

d1 <- d[sample(nrow(d), 5000), ]

#preview dataset

head(d1)

#view format of start time
d1$Start_Time

##-------------calculating total time of collision-----------------##

difftime(d1$Start_Time,d1$End_Time, units="mins")

# create column representing difference in times and attach to d1 dataset
d1['difftime'] = difftime(d1$Start_Time,d1$End_Time)
d1$difftime = as.numeric(d1$difftime)

## check form 

d1$difftime
class(d1$difftime)

## change to absolute form

d1$difftime = abs(d1$difftime)

---------------#check if length of accidents are correlated to Severity(traffic stoppage) #--------------------

SeverityMeans<- d1 %>%
  group_by(Severity) %>%
  summarize(time_means = mean(difftime))

barplot(SeverityMeans)

# attempting to fix error...

SeverityMeans <-  SeverityMeans$time_means                     # Extract values
names(SeverityMeans) <- SeverityMeans$Severity              # Assign names to values
SeverityMeans

# re-run 

barplot(SeverityMeans)
# Question: How to put the Severity on the x-axis of bar plot 

##---------Weather and Length of Time Relationship---------------##

glimpse(d1)

##plotting side by side boxplots based on whether condition to show time

#Question: Is there a fast way to descriptive statistics of numeric column based on categories and automatically plotting them?
# Store the graph
box_plot <- ggplot(d1, aes(x =Weather_Condition, y = difftime))
# Add the geometric object box plot
box_plot +
  geom_boxplot()
boxplot(d1$difftime ~d1$Weather_Condition, col = c("skyblue", "pink"))

#attempt to fix outlier issues in plot

box_plot_crop<-ggplot(data=d1, aes(y = difftime)) 
box_plot_crop+ geom_boxplot(outlier.shape = NA) 
# Question: How to make boxplot correctly 




------ # USELESS CODE - DO NOT RUN  #---------------------

### Realized code below is useless

d1$difftime

## figured out that the 'mins' only shows in dataframe from more useless code below 

#edit column to exclude 'mins' and calculate in its absolute form 

d1$difftime = substr(d1$difftime, 1, nchar(d1$difftime)-1)
d1$difftime

d1$Start_Time = substring(d1$Start_Time,12)
d1$End_Time = substring(d1$End_Time,12)

#confirm correct values removed
d1$Start_Time
d1$End_Time

----------------------------------------------
