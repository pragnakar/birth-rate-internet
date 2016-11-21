# data modifiaction

library(ggplot2)
library(rworldmap)
library(maps)
library(MASS)
library(grid)
hw <- theme_gray()+ theme(
  strip.text.y = element_blank(),
  strip.background=element_rect(fill=rgb(.9,.95,1),
                                colour=gray(.5), size=.2),
  
  panel.border=element_rect(fill=FALSE,colour=gray(.70)),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  panel.margin.x = unit(0.10,"cm"),
  panel.margin.y = unit(0.05,"cm"),
  
  axis.ticks=element_blank(),
  axis.text=element_text(colour="black"),
  axis.text.y=element_text(margin=margin(0,3,0,3), size =  14),
  axis.text.x=element_text(margin=margin(-1,0,3,0), size = 14),
  axis.title= element_text(size=16),
  plot.title = element_text(size=16)
)

data_frame<- read.csv(file.choose())



# plot by continents------------------------------------------------------- 
ggplot(data_frame, aes(x= Internet.users, y = Birth.rate, color = region ))+geom_point( size = 4  )+
  ggtitle("Birth rate Vs Internet users %, catagorized based on regions ") +
  xlab("internet user percentage")+
  ylab("Birth rate ")+hw


#boxplots

#birth rate
ggplot(data_frame, aes(x= region, y = Birth.rate )) + geom_boxplot( alpha=.3)

#continets

ggplot(data_frame, aes(x= region, y = Internet.users )) + geom_boxplot( alpha=.3)
#---------------------------------------------------------------------------


#plot by income group ------------------------------------------------------


ggplot(data_frame, aes(x= Internet.users, y = Birth.rate, color = Income.Group))+geom_point(size= 4)+
  ggtitle("Birth rate Vs Internet users %, categorized based on income group ")+
  xlab("internet user percentage")+
  ylab("Birth rate ") +hw

#boxplots 

#birth rate
ggplot(data_frame, aes(x= Income.Group, y = Birth.rate )) + geom_boxplot( alpha=.3)

#continets

ggplot(data_frame, aes(x= Income.Group, y = Internet.users )) + geom_boxplot( alpha=.3)

#---------------------------------------------------------------------------

# regression model----------------------------------------------------------

#PLOT
ggplot(data_frame, aes(x= Internet.users, y = Birth.rate))+ geom_smooth() + 
  geom_point(aes(color= Income.Group), size = 4)+
  ggtitle("Birth rate Vs Internet users %, categorized based on income group ")+
  xlab("internet user percentage")+
  ylab("Birth rate ") +hw



regression_model = lm(data_frame$Birth.rate~ poly(data_frame$Internet.users, 2) )

#quadratic annalysis


summary(regression_model)

#---------------------------------------------------------------------------

#--------- world maps-------------------------------------------------------

#birth rate
world_map <- map_data(map="world")
gg <- ggplot(data_frame)

gg <- gg + geom_map(dat=world_map, map = world_map, 
                    aes(map_id=region), fill="gray49", color="black")

gg <- gg + geom_map(map = world_map, 
                    aes(map_id = Country.Name, fill = Birth.rate ), colour = "black")


gg <- gg + expand_limits(x = world_map$long, y = world_map$lat) + ggtitle("World map based on Birth rate ")+hw

gg

#internet users 
world_map <- map_data(map="world")
gg <- ggplot(data_frame)

gg <- gg + geom_map(dat=world_map, map = world_map, 
                    aes(map_id=region), fill="gray49", color="black")

gg <- gg + geom_map(map = world_map, 
                    aes(map_id = Country.Name, fill = Internet.users ), colour = "black")


gg <- gg + expand_limits(x = world_map$long, y = world_map$lat) + ggtitle("World map based on internet user percentage")+hw

gg


#income group 

world_map <- map_data(map="world")
gg <- ggplot(data_frame)

gg <- gg + geom_map(dat=world_map, map = world_map, 
                    aes(map_id=region), fill="gray49", color="black")

gg <- gg + geom_map(map = world_map, 
                    aes(map_id = Country.Name, fill = Income.Group ), colour = "black")


gg <- gg + expand_limits(x = world_map$long, y = world_map$lat) +ggtitle("World map based on Income group ")+hw
  

gg

#---------------------------------------------------------------------------



