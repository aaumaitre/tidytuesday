#libraries
library(tidyverse)
library(scales)
library(extrafont)
library(ggpubr)


#data
park_visits <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/national_parks.csv")

# Cleaning data
# (I remember taking the cleaning from someone on twitter,
# but I don't remember whom, sorry for that)

parkv_clean <- park_visits%>%
  filter(year != "Total",
         unit_type == "National Park") %>%
  transmute(year = as.numeric(year),
            park = as.factor(parkname),
            visitors = as.numeric(visitors),
            region = as.factor(region))

#Getting the most visited parks
most_visited <- parkv_clean%>%
  filter(park != "NA")%>%
  group_by(park)%>%
  summarize(tot = sum(visitors))%>%
  arrange(desc(tot))%>%
  top_n(10)

#For the first plot, I need a vector with the top 10 visited parks:
temp_df <- most_visited%>% filter(tot > 84750237)
top_parks <- temp_df$park

#Filtering for only parks with most visits
lines_data <- parkv_clean%>% 
  filter(park %in% top_parks)%>%
  group_by(park, year)%>%
  mutate(visits = sum(visitors))


####################################
##### LINES PLOT ##################
###################################


#Color palette using shades of green
colfunc <- colorRampPalette(c("#7db80d", "#324907"))

#plot

p1 <- lines_data%>%
  ggplot(aes(x=year, y= visits, color = park))+ 
  scale_color_manual(values = colfunc(10))+
  geom_line(size = 1.1)+ guides(color = FALSE)+
  geom_curve(x = 2000, y = 5600000, xend = 2013, yend = 5200000, color = "darkolivegreen4",
             curvature = 0.1,  arrow = arrow(length = unit(0.1, "inches"))) +
  annotate("text", x= 1994, y = 5750000, 
           label= "Grand \nCanyon",
           color = "darkolivegreen4", size = 2.7)+
  geom_curve(x = 1996, y = 305000, xend = 2010, yend = 850000, color = "darkolivegreen4",
             curvature = 0.4,  arrow = arrow(length = unit(0.1, "inches"))) +
  annotate("text", x= 1990, y = 450000, 
           label= "Mount \nRainier",
           color = "darkolivegreen4", size = 2.7)+
  scale_y_continuous(labels = scales::comma)+
  labs(title = "Evolution of visits to most popular National Parks since 1904",
       x = NULL,
       y = "Number of\nvisitors",
       caption = "#tidytuesday by @ariamsita, data: data.world")+
  theme_minimal()+
  theme(plot.background = element_rect(fill = "moccasin"),
        panel.grid = element_line(color = "darkolivegreen4", size = 0.05),
        plot.margin = unit(c(1.2,0.5,0.5,0.5), "cm"), #adds some space around
        text = element_text(color = "white", size = 10,
                            family = "Eras Light ITC", face = "bold"),
        axis.text = element_text(color = "darkolivegreen4"),
        axis.title.y = element_text(angle = 0, color = "darkolivegreen4", size = 8),
        plot.caption = element_text(color = "darkolivegreen4", size = 9,
                                    family = "Eras Light ITC", hjust = 0),
        plot.title = element_text(face = "bold",
                                  hjust = 0.5,color = "darkolivegreen4",
                                  size = 13, vjust = 6,
                                  family = "Arial"), #vjust to move it towards margins
        panel.spacing = unit(2, "lines"))+
  ggsave("lines.png")


####################################
########## BAR PLOT ###############
####################################

p2 <- most_visited%>%
  ggplot(aes(x=reorder(park, tot), y = tot))+
  geom_bar(stat = "identity", fill = "darkolivegreen4",width = 0.6 )+
  coord_flip()+
  guides(fill = FALSE)+
  scale_y_continuous(labels = scales::comma)+
  labs(title = "Most visited parks, 1904-2016",
       x = NULL,
       y = "Number of visitors")+
  theme_minimal()+
  theme(plot.background = element_rect(fill = "moccasin"),
        panel.grid = element_line(color = "darkolivegreen4", size = 0.05),
        plot.margin = unit(c(1.2,0.5,0.5,0.5), "cm"), #adds some space around
        text = element_text(color = "darkolivegreen4", size = 10,
                            family = "Eras Light ITC", face = "bold"),
        axis.text = element_text(color = "darkolivegreen4"),
        axis.title.y = element_text(angle = 0, color = "darkolivegreen4", size = 8),
        plot.caption = element_text(color = "#ec99d3", size = 9,
                                    family = "Arial"),
        plot.title = element_text(face = "bold",
                                  hjust = 0.5,color = "darkolivegreen4",
                                  size = 13, vjust = 6,
                                  family = "Arial"), #vjust to move it towards margins
        panel.spacing = unit(2, "lines"))+
  ggsave("bars.png")

#######################################
######## PUTTING ALL TOGETHER ########
######################################

#Creating a plot list  to fix some issues with ggarrange (lines in the middle)
plot.list <- lapply(list(p1, p2), 
                    function(p) p + 
                      theme(plot.background = element_rect(color = "moccasin")))

#Final plot
ggarrange(plotlist = plot.list, ncol = 2, widths = c(1.6, 1))+
  ggsave("nparks.png", height = 6, width = 12)



    