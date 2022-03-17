library(dplyr)
library(tidyverse)
library(stats)
library(ggplot2)
library(heatmaply)


#looking at the dataset
vehicles <- read.csv("C://Users//Admin//Desktop//datasets//vehicles.csv")
glimpse(vehicles)

#columns with NA values
vehicles %>% select(citympg, displacement, highwaympg) %>% summary()

#replacing the NA values for the mean in each respected column
vehicles <- vehicles %>% 
  mutate(citympg = ifelse(is.na(citympg), mean(citympg,na.rm = TRUE),citympg)) %>% 
   mutate(highwaympg = ifelse(is.na(highwaympg), mean(highwaympg, na.rm =TRUE), highwaympg)) %>%
    mutate(displacement = ifelse(is.na(displacement), mean(displacement, na.rm = TRUE), displacement))

#Comparing the mpg vs levels of carbon emissions. 
vehicles %>% ggplot() + 
  geom_point(mapping = aes(x = citympg, y = co2emissions),color = vehicles$cylinders, size = 2) +
    labs(title = "CO2 Emissions to Miles per Gallon By Cylinder Class", x = "MPG", y = "CO2 Emmissions")  +facet_grid(~cylinders)

#CO2 emissions by car type
vehicles %>% ggplot() + 
  geom_boxplot(mapping = aes(x = class, y = co2emissions),fill ="light blue") + 
  labs(tile = "CO2 Emissions by Vehicle Class", x = "Class", y ="CO2 Emmissions")

# 2-wheel drive cars are the most efficient. 
no.2wheel.4wheel <- vehicles$drive %>% filter_at(drive, as.character("2-Wheel Drive") & as.character("4-Wheel Drive") == NA)
vehicles %>% ggplot() + geom_bar(mapping = aes(x = citympg, fill= drive), color = "black") + coord_flip()

#Distribution of highway mpg
hist(x = vehicles$highwaympg)
mean(vehicles$highwaympg)
sd(vehicles$highwaympg)
better.hist <- ggplot(vehicles, aes(vehicles$highwaympg, fill = vehicles$class)) +geom_histogram(bins = 15, colour ="black")
better.hist

# Finding the average city mpg for each car class.
#Compact cars and mid-size are the only cars that have above average citympg.
vehicles %>% 
  group_by(class) %>%
  summarise(avg.mpg = mean(citympg)) %>%
  ggplot(., aes(x = class, y = avg.mpg)) + 
  geom_point() + theme_update() + coord_flip() +
  xlab("Vehicle Class") + ylab("Miles Per Gallon")



#Highway and City MPG are positively correlated.
cor(x = vehicles$citympg, y = vehicles$highwaympg)

#Testing correlation in vehicles dataset.
#Creating a new dataframe to test correlation of numeric values.
is.numeric(vehicles$transmissionspeeds)
new.data.frame <- data.frame(vehicles$citympg, vehicles$cylinders,vehicles$year, vehicles$co2emissions, vehicles$transmissionspeeds)

#Correlation Matrix of Vehicles dataset
heatmaply(x = cor(new.data.frame), xlab = "Features", ylab = "Features") 

