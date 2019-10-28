#libraries
library(tidyverse)
library(extrafont)
library(ggpubr)

#data
school_diversity <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-24/school_diversity.csv")

#cleaning for density
school_div <- school_diversity%>%
  transmute(id = as.numeric(LEAID),
            school_year = as.factor(SCHOOL_YEAR),
            div = as.factor(diverse))

#Bar plot on top
p1 <- school_div%>%
  ggplot(aes(x= div, alpha= div))+
  geom_bar(fill = "#FDE725")+
  facet_wrap(~school_year)+
  scale_x_discrete(limits = c("Diverse", "Undiverse", "Extremely undiverse"))+
  scale_alpha_manual(limits = c("Diverse", "Undiverse", "Extremely undiverse"),
                     values = c(.6, .8, 1))+
  guides(alpha = FALSE)+
  labs(x = NULL, 
       y = "Number \nof schools",
       title = "School diversity has increased in the last two decades",
       subtitle = "Evolution of the number of diverse, undiverse and extremely undiverse schools")+
  theme_minimal()+
  theme(panel.spacing = unit(3, "lines"),
        axis.title.y = element_text(angle = 0, color = "grey40", size= 13),
        strip.text = element_text(color = "grey40", face = "bold", size = 16),
        plot.margin = unit(c(2,0.5,0.5,0.5), "cm"), #adds some space around

        plot.title = element_text(size = 24, face = "bold", hjust = 0.5, vjust = 8),
        plot.subtitle = element_text(size = 20, color = "grey40", hjust = 0.5),
        axis.text.x = element_text(size = 15))+
  ggsave("bars.png", width = 15, height = 5)


#cleaning for bar plots
school_clean <- school_diversity%>%
  transmute(id = as.numeric(LEAID),
            school_year = as.factor(SCHOOL_YEAR),
            AIAN,
            Asian,
            Black,
            Hispanic,
            White,
            Multi)%>%
  gather(c(3:8), key = "racial_group", value = "proportion")

school_summary <- school_clean%>%
  group_by(school_year, racial_group)%>%
  summarize(avg = mean(proportion, na.rm = TRUE))

#For later annotations
path1 <- data.frame(x = c(2.5,2.6,2.6), y = c(20, 20, 24))
path2 <- data.frame(x = c(2.6,2.6,2.5), y = c(36, 40, 40))
path3 <- data.frame(x = c(2.65,2.6,2.5), y = c(67, 71, 71))
path4 <- data.frame(x = c(2.5,2.6,2.6), y = c(76, 76, 78))
path5 <- data.frame(x = c(2.6,2.6,2.5), y = c(84, 86, 86))
path6 <- data.frame(x = c(2.5,2.6,2.6), y = c(89, 89, 90))
path7 <- data.frame(x = c(2.6,2.6,2.5), y = c(93, 94, 94))
path8 <- data.frame(x = c(2.9,2.7,2.5), y = c(93, 96, 96))
path9 <- data.frame(x = c(3.2,3.2,2.5), y = c(96, 98, 98))


#Second plot
p2 <- school_summary%>%
  ggplot(aes(x=school_year, y = avg))+
  geom_bar(stat = "identity", aes(fill = racial_group))+
geom_path(data = path1, aes(x,y), color = "grey40")+
  geom_path(data = path2, aes(x,y), color = "grey40")+
  geom_path(data = path3, aes(x,y), color = "grey40")+
  geom_path(data = path4, aes(x,y), color = "grey40")+
  geom_path(data = path5, aes(x,y), color = "grey40")+
  geom_path(data = path6, aes(x,y), color = "grey40")+
  geom_path(data = path7, aes(x,y), color = "grey40")+
  geom_path(data = path8, aes(x,y), color = "grey40")+
  geom_path(data = path9, aes(x,y), color = "grey40")+
  
  geom_text(aes(x = 2.6, y = 30, label = "Racial group: white"), color = "grey40")+
  geom_text(aes(x= 2.67, y = 63.5, label = "Multi-ethnic"), color = "grey40")+
  geom_text(aes(x = 2.6, y = 81, label = "Hispanic"), color = "grey40")+
  geom_text(aes(x = 2.6, y = 91.5, label = "Black"), color = "grey40")+
  geom_text(aes(x = 2.9, y = 91, label = "Asian"), color = "grey40")+
  geom_text(aes(x = 3.2, y = 94, label = "AIAN"), color = "grey40")+
  
  scale_fill_viridis_d()+
  guides(fill = FALSE)+
  coord_flip(clip = "off")+
  labs(y= "Percentage of students",
       x = NULL, 
      subtitle = "Percentage of different ratial groups in US schools, 1994-2017",
      caption = "#tidytuesday by @ariamsita, data: The Washington Post")+
  theme_minimal()+
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        plot.subtitle = element_text(size = 20, color  = "grey40", hjust = 0.5),
        axis.text.y = element_text(size = 15, color = "grey40", face = "bold"),
        axis.title.x = element_text(size = 13, color = "grey40"),
        plot.caption = element_text(size = 10, color = "grey40"))+
  ggsave("schools.png", height = 5, width = 15)



ggarrange(p1, p2, ncol = 1)+
  ggsave("div.png", height = 10, width = 15)

