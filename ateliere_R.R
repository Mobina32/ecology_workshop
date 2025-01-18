library(tidyverse)
library(ratdat)


###exploration of data
?complete_old
summary(complete_old)
head(complete_old)
#a tibble is the dataframe
#a dataframe include the vecorts and the characters 
#the complete_old data is accessable by running the ratdat package
str(complete_old)

###it's the time to use ggplot
library(ggplot2) #gg means grammer of grafics

#https://r-graph-gallery.com/ #to view all the plots from ggplot package

#ggplot(data=<data>, mapping=aes(<VARIABLES)) + geom-function()+geom_function


ggplot(complete_old, mapping = aes(x = weight, y = hindfoot_length))+
  geom_point()

ggplot(complete_old, mapping = aes(x = weight, y = hindfoot_length))+
  geom_point(alpha = 0.2) #(alpha = 0.2): to make the points trasparent
#Note: we got the "Warning message" bcs there are na data
#so we need to filter them out

complete_old <- filter(complete_old, !is.na(weight) & !is.na(hindfoot_length))

#two ways to add color
ggplot(complete_old, mapping = aes(x = weight, y = hindfoot_length))+
  geom_point(alpha = 0.1, color = "blue")

#or as a function
ggplot(complete_old, mapping = aes(x = weight, y = hindfoot_length, color = plot_type))+
  geom_point(alpha = 0.1)


#we want to add sex as colour and year as color
ggplot(complete_old, mapping = aes(x = weight, y = hindfoot_length, color = plot_type))+
  geom_point(alpha = 0.1) +
  scale_color_viridis_d()+
  scale_x_log10()
 
ggplot(complete_old, mapping = aes(x = plot_type, y = hindfoot_length))+
         geom_boxplot() +
         geom_jitter(alpha = 0.1) +  
       scale_x_discrete(labels = label_wrap_gen(width = 10))


#changing the color
ggplot(complete_old, mapping = aes(x = plot_type, y = hindfoot_length, color = plot_type))+
  geom_boxplot() +
  geom_jitter(alpha = 0.1) +  
  scale_x_discrete(labels = label_wrap_gen(width = 10))

#if you don;t want change the color of the box
ggplot(complete_old, mapping = aes(x = plot_type, y = hindfoot_length,))+
  geom_boxplot() +
  geom_jitter(alpha = 0.1, aes(color = plot_type)) +  
  scale_x_discrete(labels = label_wrap_gen(width = 10))


#to change the color of the outlier
ggplot(complete_old, mapping = aes(x = plot_type, y = hindfoot_length,))+
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.1, aes(color = plot_type)) +  
  scale_x_discrete(labels = label_wrap_gen(width = 10))


#to change the priority with the boxplot appeaing on top of the points
ggplot(complete_old, mapping = aes(x = plot_type, y = hindfoot_length,))+
  geom_jitter(alpha = 0.1, aes(color = plot_type)) + 
  geom_boxplot(outlier.shape = NA) +
    scale_x_discrete(labels = label_wrap_gen(width = 10))

#if you want the inside of the boxplot to be transparent or filled with color
ggplot(complete_old, mapping = aes(x = plot_type, y = hindfoot_length,))+
  geom_jitter(alpha = 0.1, aes(color = plot_type)) + 
  geom_boxplot(outlier.shape = NA, fill = NA) +
    scale_x_discrete(labels = label_wrap_gen(width = 10))


#to change the plat to violin
ggplot(complete_old, mapping = aes(x = plot_type, y = hindfoot_length,))+
  geom_jitter(alpha = 0.1, aes(color = plot_type)) + 
  geom_violin(fill = NA) +
  scale_x_discrete(labels = label_wrap_gen(width = 10))


#to change the theme
ggplot(complete_old, mapping = aes(x = plot_type, y = hindfoot_length,))+
  geom_jitter(alpha = 0.1, aes(color = plot_type)) + 
  geom_violin(fill = NA) +
  scale_x_discrete(labels = label_wrap_gen(width = 10))+
  theme_bw()

#to change specific theme
ggplot(complete_old, mapping = aes(x = plot_type, y = hindfoot_length,))+
  geom_jitter(alpha = 0.1, aes(color = plot_type)) + 
  geom_boxplot(outlier.shape = NA, fill = NA) +
  scale_x_discrete(labels = label_wrap_gen(width = 10))+
  theme_bw() +
  theme(legend.position = "none")
  

#to add lables
ggplot(complete_old, mapping = aes(x = plot_type, y = hindfoot_length,))+
  geom_jitter(alpha = 0.1, aes(color = plot_type)) + 
  geom_boxplot(outlier.shape = NA, fill = NA) +
  scale_x_discrete(labels = label_wrap_gen(width = 10))+
  theme_bw() +
  theme(legend.position = "none")+
  labs(x = "Plot type", y = "Hindfoot length (mm)")


#to have diffrent sections within a plot: facet_wrap(vars(sex), ncol = 1)
ggplot(complete_old, mapping = aes(x = plot_type, y = hindfoot_length,))+
  geom_jitter(alpha = 0.1, aes(color = plot_type)) + 
  geom_boxplot(outlier.shape = NA, fill = NA) +
  facet_wrap(vars(sex), ncol = 1)+
  scale_x_discrete(labels = label_wrap_gen(width = 10))+
  theme_bw() +
  theme(legend.position = "none")+
  labs(x = "Plot type", y = "Hindfoot length (mm)")


#to create your final plot
plot_final <-  ggplot(complete_old, mapping = aes(x = plot_type, y = hindfoot_length,))+
  geom_jitter(alpha = 0.1, aes(color = plot_type)) + 
  geom_boxplot(outlier.shape = NA, fill = NA) +
  facet_wrap(vars(sex), ncol = 1)+
  scale_x_discrete(labels = label_wrap_gen(width = 10))+
  theme_bw() +
  theme(legend.position = "none")+
  labs(x = "Plot type", y = "Hindfoot length (mm)")

ggsave(filename = "figures/plot_final.png",
       plot = plot_final,
       height = 6,
       width = 8)  

#############tidyverse
# to import the data from this link provided by data carpentary:
#https://datacarpentry.github.io/R-ecology-lesson-alternative/instructor/working-with-data.html#importing-data
  
 
#to import data with tidyverse
surveys <- read_csv("data/raw/surveys_complete_77_89.csv")
View(surveys)
str(surveys)

#for selecting the column using function called "select()
#filter() for raw name
#mutate() for 
#group_by(
#summerise()

###to select 
select(surveys, plot_id, species_id)

select(surveys, c(3, 4))

select(surveys, -plot_id)   

select(surveys, where(is.numeric))

select(surveys, where(anyNA)) #to see all the NA data


###filter
filter(surveys, year == 1988) #only the year 1988

filter(surveys, species_id %in% c("RM", "DO")) #to selct only wihin a specif column and specific character names

filter(surveys, year == 1988 & species_id %in% c("RM", "DO")) 

#select and filter at the same time

#first challenge: 
#data between year 1980 and 1985 and varibles are year, month, species_id and plot_id
#Note: we use select for columnname and filter for the rows
#first way
surveys_80_85 <- filter(surveys, year >= 1980 & year <= 1985)
select(surveys_80_85, year, month, plot_id, species_id)

#second way
select(filter(surveys, year >= 1980 & year <= 1985), year, month, plot_id, species_id)

#third way using pipeline function
#ctrl + shift+M

surveys %>% 
  filter(year == 1980:1985) %>% 
  select(year, month, species_id, plot_id)

#second challenge:
#to select the column year 1988 with these rows: record id, month, species id

surveys %>% 
  filter(year == 1988) %>% 
  select(record_id, species_id, month)

#another function: mutate()

surveys %>% 
  mutate(weight_kg = weight / 1000) %>% 
  relocate(weight_kg, after = record_id)

surveys %>% 
  filter(!is.na(weight)) %>% 
  mutate(weight_kg = weight / 1000,
        weight_lbs = weights_kg * 2.2) %>% 
  relocate(weight_kg, after = record_id)
  relocate(weight_lbs, after = weight_kg)
  
#to transform the date to yy,mm, dd
surveys %>% 
    mutate(date = paste(year, month, day, sep = "-")) %>% 
  relocate(date, .after = year)

#we can use library (lubridate) within tidyverse for changing the character in date
library(lubridate)

surveys %>% 
  mutate(date = ymd(paste(year, month, day, sep = "-"))) %>% 
  relocate(date, .after = year)

#next function: group_by
surveys %>% 
  group_by(sex) %>% 
  summarize(mean.weight = mean(weight, na.rm = TRUE),
            count = n())

#challange: create a new column on date and then add the sex and Nb varible as a ggplot 


surveys %>% 
  mutate(date = ymd(paste(year, month, day, sep = "-"))) %>% 
  group_by(sex, date) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = date, y = count, color = sex)) +
  geom_line()
            
surveys %>% 
  filter(!is.na(sex)) %>% 
  mutate(date = ymd(paste(year, month, day, sep = "-"))) %>% 
  group_by(sex, date) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = date, y = count, color = sex)) +
  geom_line()





